# =============================================================================
# CSO Data Bot for Bluesky
# Checks for newly published CSO datasets, creates a ggplot2 visualisation,
# and posts up to 3 per run to Bluesky.
# Runs every 30 minutes; datasets that fail to download/plot are skipped
# (logged to data/skipped_tables.csv) and retried on the next run.
# =============================================================================

# libraries --------------------------------------------------------------------
library(bskyr)
library(csodata)
library(glue)
library(scales)
library(tidyverse)

# tracking file for already-posted tables --------------------------------------
posted_file <- "data/posted_tables.csv"

if (file.exists(posted_file)) {
  posted_tables <- read_csv(posted_file, col_types = cols(.default = "c"))
} else {
  posted_tables <- tibble(id = character(), title = character(), posted_date = character())
}

# 1. check for new/updated datasets -------------------------------------------
toc <- tryCatch(
  cso_get_toc(suppress_messages = TRUE, from_date = NULL),
  error = function(e) NULL
)

if (is.null(toc)) {
  message("Could not connect to CSO API. Exiting.")
  quit(status = 0)
}

new_tables <- toc |>
  filter(as.Date(LastModified) >= Sys.Date() - 1) |>
  filter(!id %in% posted_tables$id)

if (nrow(new_tables) == 0) {
  message("No new CSO datasets to post. Exiting.")
  quit(status = 0)
}

message(glue("Found {nrow(new_tables)} unposted dataset(s)."))

# 2. process up to MAX_POSTS datasets per run ----------------------------------
MAX_POSTS <- 3
tables_to_post <- new_tables |> arrange(LastModified) |> slice_head(n = MAX_POSTS)
skipped_tables <- tibble(id = character(), title = character(),
                         reason = character(), skipped_date = character())
n_posted <- 0

# shared theme -----------------------------------------------------------------
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

# convert column names like "Type.of.Beneficiary" → "Type of beneficiary"
to_sentence <- function(x) {
  x <- str_replace_all(x, "[._]", " ")
  str_to_sentence(x)
}

# helper: keep only the top-N levels of a column (by mean absolute value)
trim_levels <- function(data, col, max_n) {
  top <- data |>
    group_by(.data[[col]]) |>
    summarise(.mv = mean(abs(value), na.rm = TRUE), .groups = "drop") |>
    slice_max(.mv, n = max_n, with_ties = FALSE) |>
    pull(.data[[col]])
  data |> filter(.data[[col]] %in% top)
}

CSO_BLUE <- "#2171B5"

for (row_i in seq_len(nrow(tables_to_post))) {
  tbl       <- tables_to_post[row_i, ]
  tbl_id    <- tbl$id
  tbl_title <- tbl$title

  message(glue("Processing {tbl_id}: {tbl_title}"))

  # add delay between posts to avoid rate limiting
  if (n_posted > 0) Sys.sleep(30)

  # 3. download data -----------------------------------------------------------
  df <- tryCatch(
    cso_get_data(tbl_id, pivot_format = "tall"),
    error = function(e) {
      message(glue("Could not download {tbl_id}: {e$message}"))
      NULL
    }
  )

  if (is.null(df) || nrow(df) == 0 || !"value" %in% names(df)) {
    message(glue("No usable data for {tbl_id}. Skipping."))
    skipped_tables <- bind_rows(skipped_tables, tibble(
      id = tbl_id, title = tbl_title, reason = "No usable data",
      skipped_date = as.character(Sys.Date())))
    next
  }

  df$value <- suppressWarnings(as.numeric(df$value))
  df <- df |> filter(!is.na(value))

  if (nrow(df) == 0) {
    message(glue("All values non-numeric for {tbl_id}. Skipping."))
    skipped_tables <- bind_rows(skipped_tables, tibble(
      id = tbl_id, title = tbl_title, reason = "All values non-numeric",
      skipped_date = as.character(Sys.Date())))
    next
  }

  # 4. prepare data for visualisation ------------------------------------------

  # pick the most common statistic if multiple exist
  if ("Statistic" %in% names(df) && n_distinct(df$Statistic) > 1) {
    stat_pick  <- df |> count(Statistic) |> slice_max(n, n = 1, with_ties = FALSE) |> pull(Statistic)
    df         <- df |> filter(Statistic == stat_pick)
    stat_label <- stat_pick
  } else if ("Statistic" %in% names(df)) {
    stat_label <- unique(df$Statistic)[1]
  } else {
    stat_label <- "Value"
  }

  # attempt to construct a proper Date from year + month + day columns
  year_src  <- intersect(c("Year", "Year.of.Occurrence", "Year.of.Registration"), names(df))[1]
  month_src <- intersect(c("Month", "Month.of.Occurrence", "Month.of.Registration"), names(df))[1]
  day_src   <- intersect(c("Date.of.Occurrence", "Date.of.Registration"), names(df))[1]

  if (!is.na(year_src) && !is.na(month_src) && !is.na(day_src)) {
    df_dated <- df |>
      filter(!.data[[day_src]]   %in% c("Total", "All")) |>
      filter(!.data[[month_src]] %in% c("Total", "All")) |>
      filter(!.data[[year_src]]  %in% c("Total", "All")) |>
      mutate(
        .day  = as.integer(str_extract(.data[[day_src]], "\\d+")),
        Date  = as.Date(paste(.data[[year_src]], .data[[month_src]], .day),
                        format = "%Y %B %d")
      ) |>
      filter(!is.na(Date)) |>
      select(-.day)
    if (nrow(df_dated) > 0) df <- df_dated
  }

  # identify time column (ordered by preference, then fuzzy fallback)
  time_candidates <- c(
    "Date",
    "Year", "Year.of.Occurrence", "Year.of.Registration",
    "Quarter", "Quarter.of.Occurrence", "Quarter.of.Registration",
    "Half Year",
    "Month", "Month.of.Occurrence", "Month.of.Registration",
    "Week", "Day",
    "Date.of.Occurrence", "Date.of.Registration"
  )
  # pick first candidate that exists AND has more than 1 distinct value
  time_col <- NA_character_
  for (tc in intersect(time_candidates, names(df))) {
    if (n_distinct(df[[tc]], na.rm = TRUE) > 1) { time_col <- tc; break }
  }
  # fallback: match any column whose name contains a time-like keyword
  if (is.na(time_col)) {
    time_pattern <- "(?i)(year|quarter|month|week|day|date|period|half)"
    fuzzy_hits   <- names(df)[str_detect(names(df), time_pattern)]
    fuzzy_hits   <- setdiff(fuzzy_hits, c("value", "Statistic", "STATISTIC"))
    for (tc in fuzzy_hits) {
      if (n_distinct(df[[tc]], na.rm = TRUE) > 1) {
        time_col <- tc
        time_candidates <- union(time_candidates, tc)
        break
      }
    }
  }

  # year-over-year decomposition: when the time column holds "YYYY MonthName"
  # values spanning multiple years, split into Month (x-axis) + .year (color)
  yoy_col <- NULL
  if (!is.na(time_col)) {
    tv <- as.character(df[[time_col]])
    # check if most values match "YYYY MonthName" pattern
    is_ym <- str_detect(tv, "^\\d{4}\\s+(January|February|March|April|May|June|July|August|September|October|November|December)$")
    if (mean(is_ym, na.rm = TRUE) > 0.9) {
      df <- df |>
        mutate(
          .year      = str_extract(.data[[time_col]], "^\\d{4}"),
          .month_name = str_extract(.data[[time_col]], "[A-Za-z]+$"),
          .month_name = factor(.month_name,
                               levels = month.name, ordered = TRUE)
        ) |>
        filter(!is.na(.month_name))
      # keep last 6 complete years (12 months each) to avoid too many lines
      year_completeness <- df |> count(.year) |> filter(n >= 12) |> pull(.year)
      recent_years <- sort(as.integer(year_completeness), decreasing = TRUE)[1:min(6, length(year_completeness))]
      recent_years <- as.character(recent_years)
      df <- df |> filter(.year %in% recent_years)
      time_col <- ".month_name"
      yoy_col  <- ".year"
    }
  }

  # exclude ALL time-like columns from grouping, not just the selected one
  all_time_cols <- intersect(time_candidates, names(df))
  # also exclude synthetic YOY columns
  yoy_exclude <- if (!is.null(yoy_col)) c(yoy_col, time_col) else character()

  # identify and clean categorical columns --------------------------------------
  exclude_cols <- na.omit(unique(c(all_time_cols, yoy_exclude, "value", "Statistic", "STATISTIC")))
  other_cols   <- setdiff(names(df), exclude_cols)

  # drop aggregate / "total" rows so they don't inflate level counts or double-count
  agg_pattern <- "(?i)^(total|all\\b|both\\b)"
  for (col in other_cols) {
    is_agg <- str_detect(as.character(df[[col]]), agg_pattern)
    if (any(is_agg) && !all(is_agg)) df <- df |> filter(!is_agg)
  }

  # collect all categorical columns with >= 2 levels, sorted by level count
  cat_info <- tibble(col = other_cols) |>
    mutate(n_levels = map_int(col, ~ n_distinct(df[[.x]], na.rm = TRUE))) |>
    filter(n_levels >= 2) |>
    arrange(n_levels)

  # assign roles: fewest levels -> color (max 6), next -> facet (max 12)
  MAX_COLOR <- 6
  MAX_FACET <- 12
  group_col <- NULL
  facet_col <- NULL
  for (i in seq_len(nrow(cat_info))) {
    ci <- cat_info[i, ]
    if (is.null(group_col)) {
      if (ci$n_levels > MAX_COLOR) df <- trim_levels(df, ci$col, MAX_COLOR)
      group_col <- ci$col
    } else if (is.null(facet_col)) {
      if (ci$n_levels > MAX_FACET) df <- trim_levels(df, ci$col, MAX_FACET)
      facet_col <- ci$col
    }
  }

  # when year-over-year is active, .year becomes the color and existing group
  # becomes a facet
  if (!is.null(yoy_col)) {
    if (!is.null(group_col) && is.null(facet_col)) facet_col <- group_col
    group_col <- yoy_col
  }

  # aggregate to group_vars (include all categorical columns)
  group_vars <- na.omit(c(time_col, group_col, facet_col))
  if (length(group_vars) > 0) {
    plot_df <- df |>
      group_by(across(all_of(group_vars))) |>
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  } else {
    plot_df <- df |> summarise(value = mean(value, na.rm = TRUE))
  }

  # keep last 20 time points to avoid overcrammed x-axis (skip for Date columns
  # where ggplot handles continuous axes natively)
  if (!is.na(time_col) && time_col %in% names(plot_df) && !inherits(plot_df[[time_col]], "Date")) {
    plot_df  <- plot_df |> arrange(.data[[time_col]])
    time_vals <- unique(plot_df[[time_col]])
    if (length(time_vals) > 20) {
      plot_df <- plot_df |> filter(.data[[time_col]] %in% tail(time_vals, 20))
    }
  }

  # wrap facet labels to prevent overlap
  if (!is.null(facet_col)) {
    plot_df[[facet_col]] <- str_wrap(as.character(plot_df[[facet_col]]), 30)
  }

  # build a labeller that prefixes facet strips with the column name
  facet_labeller <- if (!is.null(facet_col)) {
    facet_title <- to_sentence(facet_col)
    function(labels) {
      labels[[1]] <- paste0(facet_title, ": ", labels[[1]])
      labels
    }
  } else {
    NULL
  }

  # 5. build visualisation -----------------------------------------------------
  # skip individual points when many time values (e.g. daily data)
  n_time <- if (!is.na(time_col)) n_distinct(plot_df[[time_col]]) else 0
  show_points <- n_time <= 30

  p <- tryCatch({

    # --- Case 1: time series with groups -> coloured lines --------------------
    if (!is.na(time_col) && !is.null(group_col)) {
      group_label <- if (!is.null(yoy_col) && group_col == yoy_col) "Year" else str_wrap(to_sentence(group_col), 20)
      if (is.null(yoy_col)) plot_df[[group_col]] <- str_wrap(plot_df[[group_col]], 30)
      pp <- ggplot(plot_df, aes(.data[[time_col]], value,
                          color = .data[[group_col]],
                          group = .data[[group_col]])) +
        geom_line(linewidth = 1) +
        scale_color_brewer(palette = "Set1", name = group_label) +
        scale_y_continuous(labels = label_comma()) +
        labs(
          title    = str_wrap(tbl_title, 55),
          subtitle = stat_label,
          caption  = glue("Source: CSO PxStat ({tbl_id}) · data.cso.ie"),
          x = NULL, y = str_wrap(stat_label, 35)
        ) +
        cso_theme() +
        theme(legend.position = "bottom",
              legend.title    = element_text(size = 9),
              legend.text     = element_text(size = 8)) +
        guides(color = guide_legend(ncol = 2))
      if (show_points) pp <- pp + geom_point(size = 1.5)
      if (!is.null(facet_col)) {
        pp <- pp + facet_wrap(vars(.data[[facet_col]]), ncol = 1,
                              strip.position = "right",
                              labeller = facet_labeller)
      }
      pp

      # --- Case 2: time series, no groups -> single line ----------------------
    } else if (!is.na(time_col)) {
      pp <- ggplot(plot_df, aes(.data[[time_col]], value, group = 1)) +
        geom_area(fill = CSO_BLUE, alpha = 0.15) +
        geom_line(color = CSO_BLUE, linewidth = 1.2) +
        scale_y_continuous(labels = label_comma()) +
        labs(
          title    = str_wrap(tbl_title, 55),
          subtitle = stat_label,
          caption  = glue("Source: CSO PxStat ({tbl_id}) · data.cso.ie"),
          x = NULL, y = str_wrap(stat_label, 35)
        ) +
        cso_theme()
      if (show_points) pp <- pp + geom_point(color = CSO_BLUE, size = 2)
      if (!is.null(facet_col)) {
        pp <- pp + facet_wrap(vars(.data[[facet_col]]), ncol = 1,
                              strip.position = "right",
                              labeller = facet_labeller)
      }
      pp

      # --- Case 3: categorical only -> horizontal bar chart -------------------
    } else if (!is.null(group_col)) {
      plot_df |>
        slice_max(abs(value), n = 12, with_ties = FALSE) |>
        mutate(!!group_col := fct_reorder(.data[[group_col]], value)) |>
        ggplot(aes(.data[[group_col]], value)) +
        geom_col(fill = CSO_BLUE, width = 0.7) +
        geom_text(aes(label = label_comma()(round(value))),
                  hjust = -0.1, size = 3, color = "grey30") +
        scale_y_continuous(labels = label_comma(),
                           expand = expansion(mult = c(0, 0.15))) +
        coord_flip() +
        labs(
          title    = str_wrap(tbl_title, 55),
          subtitle = stat_label,
          caption  = glue("Source: CSO PxStat ({tbl_id}) · data.cso.ie"),
          x = NULL, y = str_wrap(stat_label, 35)
        ) +
        cso_theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

      # --- Case 4: nothing useful -> skip -------------------------------------
    } else {
      NULL
    }

  }, error = function(e) {
    message(glue("Plot failed: {e$message}"))
    NULL
  })

  if (is.null(p)) {
    message(glue("Could not visualise {tbl_id}. Skipping."))
    skipped_tables <- bind_rows(skipped_tables, tibble(
      id = tbl_id, title = tbl_title, reason = "Plot failed",
      skipped_date = as.character(Sys.Date())))
    next
  }

  # 6. save image --------------------------------------------------------------
  img_height <- if (!is.null(facet_col)) 5 + n_distinct(plot_df[[facet_col]]) * 0.8 else 5
  img_file   <- file.path(tempdir(), glue("cso_{tbl_id}.png"))
  ggsave(img_file, plot = p, width = 8, height = img_height, dpi = 150, bg = "white")

  if (!file.exists(img_file)) {
    message("ggsave failed to produce an image. Skipping.")
    skipped_tables <- bind_rows(skipped_tables, tibble(
      id = tbl_id, title = tbl_title, reason = "ggsave failed",
      skipped_date = as.character(Sys.Date())))
    next
  }

  # 7. compose and post --------------------------------------------------------
  post_text <- glue(
    "New CSO data: {str_trunc(tbl_title, 100)}\n",
    "Table: {tbl_id} · data.cso.ie\n",
    "#Ireland #OpenData #CSO"
  )

  post_ok <- tryCatch({
    bs_post(
      text       = post_text,
      images     = img_file,
      images_alt = glue("Chart of CSO table {tbl_id}: {str_trunc(tbl_title, 100)}"),
      user       = Sys.getenv("BLUESKY_CSOIE_APP_USER"),
      pass       = Sys.getenv("BLUESKY_CSOIE_APP_PASS")
    )
    TRUE
  }, error = function(e) {
    message(glue("Failed to post {tbl_id}: {e$message}"))
    FALSE
  })

  unlink(img_file)

  if (post_ok) {
    message(glue("Posted {tbl_id} to Bluesky."))
    posted_tables <- bind_rows(
      posted_tables,
      tibble(id = tbl_id, title = tbl_title, posted_date = as.character(Sys.Date()))
    )
    n_posted <- n_posted + 1
  } else {
    skipped_tables <- bind_rows(skipped_tables, tibble(
      id = tbl_id, title = tbl_title, reason = "Bluesky post failed",
      skipped_date = as.character(Sys.Date())))
  }

} # end for loop

# 8. save tracking files -------------------------------------------------------
dir.create("data", showWarnings = FALSE)
write_csv(posted_tables, posted_file)

if (nrow(skipped_tables) > 0) {
  skipped_file <- "data/skipped_tables.csv"
  if (file.exists(skipped_file)) {
    existing_skipped <- read_csv(skipped_file, col_types = cols(.default = "c"))
    skipped_tables <- bind_rows(existing_skipped, skipped_tables)
  }
  write_csv(skipped_tables, skipped_file)
}

message(glue("Done. Posted {n_posted} dataset(s), skipped {nrow(skipped_tables)}."))