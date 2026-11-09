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

CSO_BLUE <- "#2171B5"
DRY_RUN  <- Sys.getenv("DRY_RUN") == "true"

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
      held <- c(held, paste0(to_sentence(col), ": ", keep))
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

  list(
    data = df, time = time, colour = colour, facet = facet,
    year_over_year = year_over_year, statistic = statistic, held = held
  )
}

# chart ------------------------------------------------------------------------
cso_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, lineheight = 1.2),
      plot.subtitle = element_text(color = "grey40", size = 10),
      plot.caption  = element_text(color = "grey55", size = 8),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      strip.text    = element_text(size = 8, lineheight = 1.1),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

# "Type.of.Beneficiary" -> "Type of beneficiary"
to_sentence <- function(x) str_to_sentence(str_replace_all(x, "[._]", " "))

draw_chart <- function(chart, tbl) {
  df     <- chart$data
  time   <- chart$time
  colour <- chart$colour
  facet  <- chart$facet

  if (!is.na(colour) && !chart$year_over_year) df[[colour]] <- str_wrap(df[[colour]], 30)
  show_points <- !is.na(time) && n_distinct(df[[time]]) <= 30

  # titles, theme and facets are the same for every chart type
  shared <- list(
    labs(
      title    = str_wrap(tbl$title, 55),
      subtitle = str_wrap(paste(c(chart$statistic, chart$held), collapse = " · "), 80),
      caption  = glue("Source: CSO PxStat ({tbl$id}) · data.cso.ie"),
      x = NULL, y = str_wrap(chart$statistic, 35)
    ),
    cso_theme()
  )
  if (!is.na(facet)) {
    facet_title <- to_sentence(facet)
    shared <- c(shared, list(facet_wrap(
      vars(.data[[facet]]), ncol = 1, strip.position = "right",
      labeller = as_labeller(function(x) str_wrap(paste0(facet_title, ": ", x), 30))
    )))
  }

  # time with categories: coloured lines
  if (!is.na(time) && !is.na(colour)) {
    legend_title <- if (chart$year_over_year) "Year" else str_wrap(to_sentence(colour), 20)
    p <- ggplot(df, aes(.data[[time]], value, color = .data[[colour]], group = .data[[colour]])) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1", name = legend_title) +
      scale_y_continuous(labels = label_comma()) +
      shared +
      theme(
        legend.position = "bottom",
        legend.title    = element_text(size = 9),
        legend.text     = element_text(size = 8)
      ) +
      guides(color = guide_legend(ncol = 2))
    if (show_points) p <- p + geom_point(size = 1.5)

  # time only: a single line
  } else if (!is.na(time)) {
    p <- ggplot(df, aes(.data[[time]], value, group = 1)) +
      geom_area(fill = CSO_BLUE, alpha = 0.15) +
      geom_line(color = CSO_BLUE, linewidth = 1.2) +
      scale_y_continuous(labels = label_comma()) +
      shared
    if (show_points) p <- p + geom_point(color = CSO_BLUE, size = 2)

  # categories only: horizontal bars
  } else {
    p <- df |>
      mutate(bar = fct_reorder(.data[[colour]], value)) |>
      ggplot(aes(bar, value)) +
      geom_col(fill = CSO_BLUE, width = 0.7) +
      geom_text(aes(label = label_comma()(value)), hjust = -0.1, size = 3, color = "grey30") +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      shared +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  }

  p
}

make_chart <- function(tbl) {
  df <- cso_get_data(tbl$id, pivot_format = "tall")
  if (is.null(df)) stop("Download failed")
  chart <- prepare_chart_data(df)
  p <- draw_chart(chart, tbl)

  n_facets <- if (is.na(chart$facet)) 0 else n_distinct(chart$data[[chart$facet]])
  file <- file.path(if (DRY_RUN) DRY_RUN_DIR else tempdir(), glue("cso_{tbl$id}.png"))
  ggsave(file, p, width = 8, height = 5 + 0.8 * n_facets, dpi = 150, bg = "white")
  file
}

# post -------------------------------------------------------------------------
post_chart <- function(file, tbl, auth) {
  title <- str_trunc(tbl$title, 100)
  bs_post(
    text       = glue("New CSO data: {title}\nTable: {tbl$id} · data.cso.ie\n#Ireland #OpenData #CSO"),
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
