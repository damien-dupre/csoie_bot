# =============================================================================
# CSO Data Bot for Bluesky
#
# Each run finds CSO tables released since yesterday that have not been posted
# since that release, draws a chart of each with ggplot2 and posts up to
# MAX_POSTS of them to Bluesky, newest release first.
#
# Two logs, committed back to the repository by the workflow:
#   data/posted_tables.csv   one row per post
#   data/skipped_tables.csv  one row per failed attempt, with the reason
# A release that fails MAX_FAILURES times is not tried again; the table's next
# release is.
#
# Test without posting:  DRY_RUN=true Rscript bot_cso_bluesky.R
# saves the charts in dry_run/ and leaves the logs untouched.
# =============================================================================

# libraries --------------------------------------------------------------------
library(bskyr)
library(csodata)
library(dplyr)
library(forcats)
library(ggplot2)
library(glue)
library(readr)
library(scales)
library(stringr)
library(systemfonts)

# settings ---------------------------------------------------------------------
MAX_POSTS    <- 3   # posts per run
MAX_TRIES    <- 10  # tables attempted per run, posted or not
MAX_FAILURES <- 3   # failed attempts before a release is given up
POST_DELAY   <- 30  # seconds between posts, to stay under Bluesky's rate limit

MAX_LINES   <- 6    # coloured lines on one chart
MAX_BARS    <- 12   # bars on one chart
MAX_FACETS  <- 12   # small multiples on one chart
MAX_PERIODS <- 20   # time points on a categorical x axis
MAX_YEARS   <- 6    # years on a year-over-year chart

# chart style
CHART_FONT <- "Roboto"   # any Google Font: downloaded when the bot runs
CSO_BLUE   <- "#2171B5"  # single lines, bars and the latest year
# Paul Tol's "vibrant" palette: colour-blind safe and readable on white
LINE_COLOURS <- c("#0077BB", "#EE7733", "#009988", "#EE3377", "#33BBEE", "#CC3311")
# earlier years on a year-over-year chart fade from light to mid grey
PAST_YEARS <- c("#D0D7E1", "#7D8A9C")
# a middle dot between the parts of a subtitle, written as an escape so it
# prints correctly whatever the locale
SEPARATOR <- " \u00b7 "

DRY_RUN <- Sys.getenv("DRY_RUN") == "true"

POSTED_FILE  <- "data/posted_tables.csv"
SKIPPED_FILE <- "data/skipped_tables.csv"
DRY_RUN_DIR  <- "dry_run"

# time columns in order of preference (csodata cleans names with make.names)
TIME_COLUMNS <- c(
  "Date",
  "Year", "Year.of.Occurrence", "Year.of.Registration",
  "Quarter", "Quarter.of.Occurrence", "Quarter.of.Registration",
  "Half.Year",
  "Month", "Month.of.Occurrence", "Month.of.Registration",
  "Week", "Day",
  "Date.of.Occurrence", "Date.of.Registration"
)
# fallback for time columns not listed above
TIME_PATTERN <- "(?i)(year|quarter|month|week|day|date|period|half)"
# monthly labels such as "2026 January"
MONTH_PATTERN <- paste0("^\\d{4}\\s+(", paste(month.name, collapse = "|"), ")$")
# aggregate levels such as "Total", "All ages", "Both sexes" and "State"
TOTAL_PATTERN <- "(?i)^(total|all\\b|both\\b|state$)"
# columns that are never dimensions
NOT_DIMENSIONS <- c("value", "Statistic")
# NACE industry codes after a name, such as "(105)", "(10 to 33)" or "(B to E)"
NACE_CODES <- "\\s*\\((?:[A-Z]{1,2}|\\d+)(?:\\s*(?:to|-|,|;)\\s*(?:[A-Z]{1,2}|\\d+))*\\)$"

# logs -------------------------------------------------------------------------
EMPTY_POSTED <- tibble(
  id = character(), title = character(), posted_date = character()
)
EMPTY_SKIPPED <- tibble(
  id = character(), title = character(), release_date = character(),
  reason = character(), skipped_date = character()
)

read_log <- function(file, empty) {
  if (!file.exists(file)) return(empty)
  read_csv(file, col_types = cols(.default = col_character()))
}

# rows are written one at a time, so a crash never loses a post already made
append_log <- function(row, file) {
  dir.create(dirname(file), showWarnings = FALSE)
  write_csv(row, file, append = file.exists(file))
}

log_failure <- function(tbl, reason) {
  reason <- str_trunc(str_squish(reason), 200)
  message(glue("  skipped: {reason}"))
  if (DRY_RUN) return(invisible())
  append_log(
    tibble(
      id = tbl$id, title = tbl$title,
      release_date = as.character(tbl$release_date),
      reason = reason, skipped_date = as.character(Sys.Date())
    ),
    SKIPPED_FILE
  )
}

# releases to post -------------------------------------------------------------
# A table is due when it was released since yesterday and not posted since.
# Dates, not times, are compared, so a table is posted at most once a day.
# Newest releases come first: GitHub starts this workflow far less often than
# the schedule asks, so when not everything due fits, today's tables win.
find_due_releases <- function(toc, posted, skipped) {
  last_posted <- posted |>
    mutate(last_posted = as.Date(posted_date)) |>
    slice_max(last_posted, n = 1, by = id, with_ties = FALSE) |>
    select(id, last_posted)

  given_up <- skipped |>
    count(id, release_date = as.Date(release_date), name = "failures") |>
    filter(failures >= MAX_FAILURES)

  toc |>
    mutate(release_date = as.Date(LastModified)) |>
    filter(release_date >= Sys.Date() - 1) |>
    left_join(last_posted, by = "id") |>
    filter(is.na(last_posted) | release_date > last_posted) |>
    anti_join(given_up, by = c("id", "release_date")) |>
    arrange(desc(LastModified))
}

# chart data -------------------------------------------------------------------
# tables with separate year, month and day columns get one Date column
combine_date_columns <- function(df) {
  year  <- intersect(c("Year", "Year.of.Occurrence", "Year.of.Registration"), names(df))[1]
  month <- intersect(c("Month", "Month.of.Occurrence", "Month.of.Registration"), names(df))[1]
  day   <- intersect(c("Date.of.Occurrence", "Date.of.Registration"), names(df))[1]
  if (anyNA(c(year, month, day))) return(df)

  dated <- df |>
    mutate(Date = as.Date(
      sprintf(
        "%s-%02d-%02d",
        as.character(.data[[year]]),
        match(as.character(.data[[month]]), month.name),
        as.integer(str_extract(as.character(.data[[day]]), "\\d+"))
      ),
      format = "%Y-%m-%d"
    )) |>
    filter(!is.na(Date))
  if (nrow(dated) > 0) dated else df
}

# the first time column that varies: listed names first, then keywords
find_time_column <- function(df) {
  candidates <- union(
    intersect(TIME_COLUMNS, names(df)),
    setdiff(str_subset(names(df), TIME_PATTERN), NOT_DIMENSIONS)
  )
  for (col in candidates) {
    if (n_distinct(df[[col]], na.rm = TRUE) > 1) return(col)
  }
  NA_character_
}

is_total <- function(x) str_detect(as.character(x), TOTAL_PATTERN)

# keep the n levels of `col` with the largest values on average
keep_largest_levels <- function(df, col, n) {
  largest <- df |>
    summarise(size = mean(abs(value)), .by = all_of(col)) |>
    slice_max(size, n = n, with_ties = FALSE)
  df |> filter(.data[[col]] %in% largest[[col]])
}

# Turns a tall CSO table into the data for one chart and decides its shape:
#   time and colour : coloured lines (on a year-over-year chart, one per year)
#   time only       : a single line
#   no time         : horizontal bars, one per level of `colour`
# `facet` adds small multiples. Every other dimension is held at its total, or
# at its first level when it has no total, so values are never averaged across
# categories.
prepare_chart_data <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0 || !"value" %in% names(df)) {
    stop("No usable data")
  }

  df <- df |>
    mutate(value = suppressWarnings(as.numeric(value))) |>
    filter(!is.na(value))
  if (nrow(df) == 0) stop("All values non-numeric")

  # one statistic per chart: the first one the CSO lists
  # (some tables label the column STATISTIC, as csodata itself allows for)
  if ("STATISTIC" %in% names(df)) df <- df |> rename(Statistic = STATISTIC)
  statistic <- "Value"
  if ("Statistic" %in% names(df)) {
    statistic <- as.character(df$Statistic[1])
    df <- df |> filter(Statistic == statistic)
  }

  df <- combine_date_columns(df)
  time <- find_time_column(df)
  time_columns <- c(intersect(TIME_COLUMNS, names(df)), time)

  # a table without a time axis can still be for one period, such as a census year
  single_period <- NA_character_
  listed_time <- intersect(TIME_COLUMNS, names(df))
  if (is.na(time) && length(listed_time) > 0) {
    single_period <- as.character(df[[listed_time[1]]][1])
  }

  # monthly labels become dates; over several years, a year-over-year chart
  # of the latest MAX_YEARS years, the current partial year included
  year_over_year <- FALSE
  if (!is.na(time)) {
    labels <- as.character(df[[time]])
    if (mean(str_detect(labels, MONTH_PATTERN), na.rm = TRUE) > 0.9) {
      df <- df |>
        mutate(
          .year  = str_sub(labels, 1, 4),
          .month = factor(str_extract(labels, "[A-Za-z]+$"), levels = month.name),
          .date  = as.Date(sprintf("%s-%02d-01", .year, as.integer(.month)), format = "%Y-%m-%d")
        ) |>
        filter(!is.na(.date))
      years <- sort(unique(df$.year), decreasing = TRUE)
      if (length(years) > 1) {
        df <- df |> filter(.year %in% head(years, MAX_YEARS))
        time <- ".month"
        year_over_year <- TRUE
      } else {
        time <- ".date"
      }
    }
  }

  # dimensions that still vary once their totals are set aside, sorted for
  # their roles: lines take the fewest levels first, bars the most
  dimensions <- setdiff(names(df), c(time_columns, ".year", ".month", ".date", NOT_DIMENSIONS))
  dimension_sizes <- tibble(column = dimensions) |>
    mutate(n_levels = vapply(
      column, function(col) n_distinct(df[[col]][!is_total(df[[col]])]), integer(1)
    )) |>
    filter(n_levels > 1) |>
    arrange(if (is.na(time)) desc(n_levels) else n_levels)

  if (year_over_year) {
    colour <- ".year"
    facet  <- dimension_sizes$column[1]
  } else {
    colour <- dimension_sizes$column[1]
    facet  <- dimension_sizes$column[2]
  }

  # every other dimension is held at its total, or else its first level
  held <- character()
  for (col in setdiff(dimensions, c(colour, facet))) {
    values <- as.character(unique(df[[col]]))
    keep <- c(values[is_total(values)], values)[1]
    df <- df |> filter(.data[[col]] == keep)
    if (length(values) > 1 && !is_total(keep)) {
      held <- c(held, paste0(to_label(col), ": ", keep))
    }
  }

  # dimensions with a role lose their totals and keep their largest levels
  if (!is.na(colour) && colour != ".year") {
    df <- df |>
      filter(!is_total(.data[[colour]])) |>
      keep_largest_levels(colour, if (is.na(time)) MAX_BARS else MAX_LINES)
  }
  if (!is.na(facet)) {
    df <- df |>
      filter(!is_total(.data[[facet]])) |>
      keep_largest_levels(facet, MAX_FACETS)
  }

  # one value per point (some tables have a second time column)
  keys <- c(time, colour, facet)
  keys <- keys[!is.na(keys)]
  if (length(keys) == 0) stop("Nothing to plot: no time column and no dimension")
  df <- df |> summarise(value = mean(value), .by = all_of(keys))

  # a categorical time axis keeps its last MAX_PERIODS periods, in CSO order
  if (!is.na(time) && !year_over_year && !inherits(df[[time]], "Date")) {
    periods <- if (is.factor(df[[time]])) levels(droplevels(df[[time]])) else sort(unique(df[[time]]))
    df <- df |> filter(.data[[time]] %in% tail(periods, MAX_PERIODS))
  }

  # industry names lose their NACE codes in legends, panels and bars
  for (col in intersect(c(colour, facet), str_subset(names(df), "(?i)nace"))) {
    df[[col]] <- drop_nace_codes(df[[col]])
  }

  period <- if (is.na(time)) single_period else describe_period(df, time, year_over_year)

  list(
    data = df, time = time, colour = colour, facet = facet,
    year_over_year = year_over_year, statistic = statistic, held = held,
    period = period
  )
}

# "Manufacture of dairy products (105)" -> "Manufacture of dairy products",
# unless two names would then be the same
drop_nace_codes <- function(x) {
  x <- droplevels(as.factor(x))
  short <- str_remove(levels(x), NACE_CODES)
  if (!anyDuplicated(short)) levels(x) <- short
  x
}

# the period a chart covers: "2013 to 2025", "January 2026 to August 2026"
describe_period <- function(df, time, year_over_year) {
  if (year_over_year) return(paste(range(df$.year), collapse = " to "))
  if (inherits(df[[time]], "Date")) {
    date_format <- if (time == ".date") "%B %Y" else "%e %B %Y"
    return(str_squish(paste(format(range(df[[time]]), date_format), collapse = " to ")))
  }
  periods <- if (is.factor(df[[time]])) levels(droplevels(df[[time]])) else sort(unique(df[[time]]))
  paste(unique(c(first(periods), last(periods))), collapse = " to ")
}

# chart ------------------------------------------------------------------------
cso_theme <- function() {
  theme_minimal(base_size = 13, base_family = CHART_FONT) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", size = 17, lineheight = 1.1, margin = margin(b = 6)),
      plot.subtitle = element_text(color = "grey35", size = 11.5, lineheight = 1.25, margin = margin(b = 14)),
      plot.caption  = element_text(color = "grey50", size = 9, hjust = 0, margin = margin(t = 14)),
      axis.title    = element_text(color = "grey35", size = 10.5),
      axis.title.x  = element_text(margin = margin(t = 10)),
      axis.title.y  = element_text(margin = margin(r = 10)),
      axis.text     = element_text(color = "grey35", size = 10),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
      strip.text    = element_text(face = "bold", size = 10.5, hjust = 0, vjust = 0, margin = margin(b = 6)),
      panel.spacing = unit(1.2, "lines"),
      legend.position      = "top",
      legend.justification = "left",
      legend.title  = element_text(color = "grey35", size = 10),
      legend.text   = element_text(size = 10),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin   = margin(18, 24, 12, 18)
    ) +
    # from ggplot2 3.5.0: legend keys fill rows first and line up with the title
    if (packageVersion("ggplot2") >= "3.5.0") theme(legend.byrow = TRUE, legend.location = "plot")
}

# "Type.of.Land.Use" -> "Type of Land Use"
to_label <- function(x) str_squish(str_replace_all(x, "[._]", " "))

# panel columns and image size, from the shape of the chart
chart_layout <- function(chart) {
  bars <- is.na(chart$time)
  n_facets <- if (is.na(chart$facet)) 0 else n_distinct(chart$data[[chart$facet]])
  n_bars <- if (bars) n_distinct(chart$data[[chart$colour]]) else 0
  ncol <- case_when(
    n_facets <= 1 ~ 1,
    bars          ~ min(n_facets, 3),  # bar panels sit side by side
    n_facets <= 3 ~ 1,
    n_facets <= 8 ~ 2,
    .default      = 3
  )
  rows <- max(1, ceiling(n_facets / ncol))
  panel_height <- case_when(
    bars          ~ 0.6 + 0.32 * n_bars,
    n_facets == 0 ~ 3.6,
    .default      = 1.9
  )
  list(ncol = ncol, width = c(8, 9, 10)[ncol], height = 2.4 + rows * panel_height)
}

# value axis title: the statistic and its unit, as in "Industrial Production
# Index (Base 2021=100)". The unit is left out when it is a plain count or
# already in the statistic; the statistic is left out when the chart title
# already says it and there is a unit to show instead.
measure_label <- function(chart, unit, title) {
  statistic <- chart$statistic
  show_unit <- length(unit) == 1 && !is.na(unit) && nzchar(unit) && unit != "Number" &&
    !str_detect(str_to_lower(statistic), fixed(str_to_lower(unit)))
  if (!show_unit) return(statistic)
  if (str_to_lower(statistic) == str_to_lower(title)) return(unit)
  paste0(statistic, " (", unit, ")")
}

# time axis title: "Year", "Quarter", "Half Year"...; monthly data say "Month"
time_label <- function(chart) {
  if (chart$time %in% c(".month", ".date")) "Month" else to_label(chart$time)
}

# subtitle: the period, the panels and anything held fixed (what is measured
# is on the value axis)
describe_chart <- function(chart) {
  panels <- if (!is.na(chart$facet)) paste("Panels:", to_label(chart$facet))
  period <- chart$period[!is.na(chart$period)]
  paste(c(period, panels, chart$held), collapse = SEPARATOR)
}

# earlier years fade from light to mid grey; the latest year is CSO blue
year_colours <- function(years) {
  years <- sort(unique(years))
  setNames(c(colorRampPalette(PAST_YEARS)(length(years) - 1), CSO_BLUE), years)
}

# x axis labels evenly spaced, as many as fit the panel width: months as Jan,
# Apr, Jul...; other periods counted back from the latest, which is always shown
x_scale <- function(chart, layout) {
  periods <- chart$data[[chart$time]]
  panel_width <- (layout$width - 1.2) / layout$ncol  # inches, less the y axis
  if (inherits(periods, "Date")) {
    return(scale_x_date(labels = label_date_short(), breaks = breaks_pretty(n = floor(panel_width / 0.9))))
  }
  periods <- levels(droplevels(as.factor(periods)))
  labels <- if (chart$year_over_year) str_sub(periods, 1, 3) else periods
  fits <- max(2, floor(panel_width / (0.08 * max(nchar(labels)) + 0.3)))
  every <- seq(1, length(periods), by = ceiling(length(periods) / fits))
  if (chart$year_over_year) {
    scale_x_discrete(breaks = periods[every], labels = function(x) str_sub(x, 1, 3))
  } else {
    scale_x_discrete(breaks = rev(periods)[every])
  }
}

# lines are listed from highest to lowest at the latest period, so the legend
# reads in the same order as the ends of the lines
order_by_latest <- function(df, time, colour) {
  latest <- df |>
    filter(as.numeric(.data[[time]]) == max(as.numeric(.data[[time]]))) |>
    summarise(value = mean(value), .by = all_of(colour)) |>
    arrange(desc(value))
  factor(df[[colour]], levels = union(as.character(latest[[colour]]), as.character(unique(df[[colour]]))))
}

# legend rows: as many keys on a row as the longest label allows
legend_rows <- function(labels, width) {
  longest <- max(nchar(unlist(str_split(labels, "\n"))))
  per_row <- max(1, floor((width - 1.5) / (0.08 * longest + 0.6)))
  ceiling(length(labels) / per_row)
}

draw_chart <- function(chart, tbl, unit = NULL) {
  df     <- chart$data
  time   <- chart$time
  colour <- chart$colour
  facet  <- chart$facet
  bars   <- is.na(time)
  layout <- chart_layout(chart)
  wrap   <- function(x) str_wrap(x, 30)
  # long series, such as daily data, get thinner lines
  dense  <- !bars && n_distinct(df[[time]]) > 100

  # titles, axis titles, theme and panels are the same for every chart type:
  # time runs across and the measure up, except on bars, where the measure
  # runs across and the categories up
  measure  <- measure_label(chart, unit, tbl$title)
  subtitle <- describe_chart(chart)
  shared <- list(
    labs(
      title    = str_wrap(tbl$title, round(7 * layout$width)),
      subtitle = if (nzchar(subtitle)) str_wrap(subtitle, round(11 * layout$width)),
      caption  = glue("Source: CSO PxStat ({tbl$id}){SEPARATOR}data.cso.ie")
    ),
    if (bars) {
      labs(x = str_wrap(measure, round(9 * layout$width)), y = to_label(colour))
    } else {
      # the y title is wrapped to the height of the panels
      labs(x = time_label(chart), y = str_wrap(measure, max(20, round(12 * (layout$height - 2.4)))))
    },
    cso_theme()
  )
  if (!is.na(facet)) {
    shared <- c(shared, list(facet_wrap(
      vars(.data[[facet]]),
      ncol = layout$ncol,
      scales = if (bars) "fixed" else "free_y",  # each panel shows its own variation
      labeller = as_labeller(function(x) str_wrap(x, c(80, 45, 32)[layout$ncol]))
    )))
  }

  # time with categories: coloured lines
  if (!bars && !is.na(colour)) {
    if (chart$year_over_year) {
      latest <- max(df$.year)
      legend_labels <- unique(df$.year)
      p <- ggplot(df, aes(.data[[time]], value, color = .year, group = .year)) +
        geom_line(aes(linewidth = .year == latest)) +
        scale_linewidth_manual(values = c(`FALSE` = 0.7, `TRUE` = 1.6), guide = "none") +
        scale_color_manual(values = year_colours(df$.year), name = NULL)
    } else {
      df[[colour]] <- order_by_latest(df, time, colour)
      legend_labels <- wrap(levels(df[[colour]]))
      p <- ggplot(df, aes(.data[[time]], value, color = .data[[colour]], group = .data[[colour]])) +
        geom_line(linewidth = if (dense) 0.5 else 1) +
        scale_color_manual(values = LINE_COLOURS, name = to_label(colour), labels = wrap)
    }
    p <- p +
      x_scale(chart, layout) +
      scale_y_continuous(labels = label_comma(), n.breaks = if (is.na(facet)) 6 else 4) +
      shared +
      guides(color = guide_legend(nrow = legend_rows(legend_labels, layout$width)))

  # time only: a single line, shaded down to zero when zero is near the data
  # (shading a series far from zero would flatten it)
  } else if (!bars) {
    shade <- min(df$value) >= 0 && min(df$value) < 0.5 * max(df$value)
    p <- ggplot(df, aes(.data[[time]], value, group = 1)) +
      (if (shade) geom_area(fill = CSO_BLUE, alpha = 0.12)) +
      geom_line(color = CSO_BLUE, linewidth = if (dense) 0.6 else 1.2) +
      (if (n_distinct(df[[time]]) <= 30) geom_point(color = CSO_BLUE, size = 2)) +
      x_scale(chart, layout) +
      scale_y_continuous(labels = label_comma(), n.breaks = 6) +
      shared

  # categories only: horizontal bars, largest at the top, each with its value
  # (so the x axis needs no numbers, only its title)
  } else {
    negative <- any(df$value < 0)
    p <- df |>
      mutate(bar = fct_reorder(wrap(.data[[colour]]), value)) |>
      ggplot(aes(value, bar)) +
      geom_col(fill = CSO_BLUE, width = 0.7) +
      (if (negative) geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4)) +
      geom_text(
        aes(label = label_comma()(value), hjust = if_else(value < 0, 1.15, -0.15)),
        size = 3.6, color = "grey30", family = CHART_FONT
      ) +
      scale_x_continuous(expand = expansion(mult = c(if (negative) 0.25 else 0, 0.25))) +
      coord_cartesian(clip = "off") +
      shared +
      theme(
        axis.text.x        = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing      = unit(2.5, "lines")
      )
  }

  p
}

make_chart <- function(tbl) {
  df <- cso_get_data(tbl$id, pivot_format = "tall")
  if (is.null(df)) stop("Download failed")
  # the unit of the first statistic, which is the one charted; the label goes
  # without a unit if the metadata can't be read
  unit <- tryCatch(cso_get_meta(tbl$id)$Units, error = function(e) NULL)

  chart <- prepare_chart_data(df)
  p <- draw_chart(chart, tbl, unit)

  layout <- chart_layout(chart)
  file <- file.path(if (DRY_RUN) DRY_RUN_DIR else tempdir(), glue("cso_{tbl$id}.png"))
  ggsave(
    file, p, device = ragg::agg_png,
    width = layout$width, height = layout$height, dpi = 150, bg = "white"
  )
  file
}

# post -------------------------------------------------------------------------
post_chart <- function(file, tbl, auth) {
  title <- str_trunc(tbl$title, 100)
  bs_post(
    text       = glue("New CSO data: {title}\nTable: {tbl$id}{SEPARATOR}data.cso.ie\n#Ireland #OpenData #CSO"),
    images     = file,
    images_alt = glue("Chart of CSO table {tbl$id}: {title}"),
    auth       = auth
  )
}

# main -------------------------------------------------------------------------
main <- function() {
  posted  <- read_log(POSTED_FILE, EMPTY_POSTED)
  skipped <- read_log(SKIPPED_FILE, EMPTY_SKIPPED)

  # only tables updated in the last week: older ones cannot be due
  toc <- tryCatch(
    cso_get_toc(suppress_messages = TRUE, from_date = as.character(Sys.Date() - 7)),
    error = function(e) NULL
  )
  if (is.null(toc) || nrow(toc) == 0) {
    message("Could not get the CSO table of contents. Exiting.")
    return(invisible())
  }

  due <- find_due_releases(toc, posted, skipped)
  message(glue("{nrow(due)} release(s) due."))
  if (nrow(due) == 0) return(invisible())

  if (DRY_RUN) {
    dir.create(DRY_RUN_DIR, showWarnings = FALSE)
    auth <- NULL
  } else {
    # one login per run; save_auth = FALSE keeps the session token off disk
    auth <- bs_auth(
      user = Sys.getenv("BLUESKY_CSOIE_APP_USER"),
      pass = Sys.getenv("BLUESKY_CSOIE_APP_PASS"),
      save_auth = FALSE
    )
  }

  # the chart font, from Google Fonts; if it can't be had, charts use the
  # default sans font rather than stopping the run
  tryCatch(
    require_font(CHART_FONT, fallback = "sans"),
    error = function(e) message(glue("Font {CHART_FONT} unavailable: {conditionMessage(e)}"))
  )

  n_posted <- 0
  for (i in seq_len(min(nrow(due), MAX_TRIES))) {
    if (n_posted == MAX_POSTS) break
    tbl <- due[i, ]
    message(glue("{tbl$id}: {tbl$title}"))

    chart_file <- tryCatch(make_chart(tbl), error = function(e) e)
    if (inherits(chart_file, "error")) {
      log_failure(tbl, conditionMessage(chart_file))
      next
    }

    if (DRY_RUN) {
      message(glue("  saved {chart_file}"))
      n_posted <- n_posted + 1
      next
    }

    if (n_posted > 0) Sys.sleep(POST_DELAY)
    post_error <- tryCatch(
      {
        post_chart(chart_file, tbl, auth)
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (!is.null(post_error)) {
      # most likely a Bluesky problem, not this table: stop and retry next run
      log_failure(tbl, paste("Bluesky:", post_error))
      break
    }

    append_log(
      tibble(id = tbl$id, title = tbl$title, posted_date = as.character(Sys.Date())),
      POSTED_FILE
    )
    n_posted <- n_posted + 1
    message("  posted")
  }

  message(glue("Done: {n_posted} {if (DRY_RUN) 'chart(s) saved' else 'post(s)'}."))
}

# run only when called with Rscript, not when sourced (for testing)
if (sys.nframe() == 0L) main()
