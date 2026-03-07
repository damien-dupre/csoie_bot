# =============================================================================
# CSO Data Bot for Bluesky
# Checks for newly published CSO datasets, creates a visualisation using
# ggauto (with fallback), and posts one to Bluesky per run.
# Designed to run every 2 hours so multiple daily releases get spaced out.
# =============================================================================

# libraries --------------------------------------------------------------------
library(bskyr)
library(csodata)
library(ggauto)
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
toc <- cso_get_toc(suppress_messages = TRUE, from_date = NULL)

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

# 2. pick the oldest unposted table (FIFO) ------------------------------------
tbl <- new_tables |> arrange(LastModified) |> slice(1)
tbl_id    <- tbl$id
tbl_title <- tbl$title

message(glue("Processing {tbl_id}: {tbl_title}"))

# 3. download data -------------------------------------------------------------
df <- tryCatch(
  cso_get_data(tbl_id, pivot_format = "tall"),
  error = function(e) {
    message(glue("Could not download {tbl_id}: {e$message}"))
    NULL
  }
)

if (is.null(df) || nrow(df) == 0 || !"value" %in% names(df)) {
  message(glue("No usable data for {tbl_id}. Recording and exiting."))
  posted_tables <- bind_rows(posted_tables, tibble(id = tbl_id, title = tbl_title, posted_date = as.character(Sys.Date())))
  write_csv(posted_tables, posted_file)
  quit(status = 0)
}

df$value <- suppressWarnings(as.numeric(df$value))
df <- df |> filter(!is.na(value))

# 4. prepare data for visualisation --------------------------------------------
# pick the most common statistic if multiple exist
if ("Statistic" %in% names(df) && n_distinct(df$Statistic) > 1) {
  stat_pick <- df |> count(Statistic) |> slice_max(n, n = 1) |> pull(Statistic)
  df <- df |> filter(Statistic == stat_pick[1])
  stat_label <- stat_pick[1]
} else if ("Statistic" %in% names(df)) {
  stat_label <- unique(df$Statistic)[1]
} else {
  stat_label <- "Value"
}

# identify time column
time_col <- intersect(c("Year", "Quarter", "Month", "Week", "Half Year"), names(df))[1]

# identify grouping column (2-6 levels, excluding time/value/Statistic)
other_cols <- setdiff(names(df), c(time_col, "value", "Statistic", "STATISTIC"))
group_col <- NULL
for (col in other_cols) {
  nl <- n_distinct(df[[col]])
  if (nl >= 2 && nl <= 6) { group_col <- col; break }
}

# aggregate
group_vars <- na.omit(c(time_col, group_col))
if (length(group_vars) > 0) {
  plot_df <- df |>
    group_by(across(all_of(group_vars))) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
} else {
  plot_df <- df
}

# keep last 20 time points if time series
if (!is.na(time_col)) {
  plot_df <- plot_df |> arrange(.data[[time_col]])
  time_vals <- unique(plot_df[[time_col]])
  if (length(time_vals) > 20) {
    plot_df <- plot_df |> filter(.data[[time_col]] %in% tail(time_vals, 20))
  }
}

# 5. create visualisation with ggauto (fallback to manual) ---------------------
p <- tryCatch({
  if (!is.na(time_col) && !is.null(group_col)) {
    ggauto(plot_df[[time_col]], plot_df$value, plot_df[[group_col]],
           title = tbl_title,
           subtitle = stat_label,
           caption = glue("Source: CSO PxStat ({tbl_id}) | data.cso.ie"),
           xlab = time_col, ylab = stat_label)
  } else if (!is.na(time_col)) {
    ggauto(plot_df[[time_col]], plot_df$value,
           title = tbl_title,
           subtitle = stat_label,
           caption = glue("Source: CSO PxStat ({tbl_id}) | data.cso.ie"),
           xlab = time_col, ylab = stat_label)
  } else if (!is.null(group_col)) {
    ggauto(plot_df[[group_col]], plot_df$value,
           title = tbl_title,
           subtitle = stat_label,
           caption = glue("Source: CSO PxStat ({tbl_id}) | data.cso.ie"),
           ylab = stat_label)
  } else {
    ggauto(plot_df$value,
           title = tbl_title,
           subtitle = stat_label,
           caption = glue("Source: CSO PxStat ({tbl_id}) | data.cso.ie"))
  }
}, error = function(e) {
  message(glue("ggauto failed: {e$message}. Using fallback."))
  NULL
})

# fallback: simple ggplot2
if (is.null(p)) {
  p <- tryCatch({
    if (!is.na(time_col)) {
      ggplot(plot_df, aes(.data[[time_col]], value)) +
        geom_line(group = 1, color = "#2171B5", linewidth = 1) +
        geom_point(color = "#2171B5", size = 1.5) +
        labs(title = str_wrap(tbl_title, 60),
             subtitle = stat_label,
             caption = glue("Source: CSO PxStat ({tbl_id}) | data.cso.ie"),
             x = time_col, y = stat_label) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold"))
    } else if (!is.null(group_col)) {
      plot_df |>
        slice_max(abs(value), n = 12) |>
        mutate(across(all_of(group_col), ~ fct_reorder(.x, value))) |>
        ggplot(aes(.data[[group_col]], value)) +
        geom_col(fill = "#2171B5") +
        coord_flip() +
        labs(title = str_wrap(tbl_title, 60),
             subtitle = stat_label,
             caption = glue("Source: CSO PxStat ({tbl_id}) | data.cso.ie"),
             x = "", y = stat_label) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))
    }
  }, error = function(e) NULL)
}

if (is.null(p)) {
  message(glue("Could not visualise {tbl_id}. Recording and exiting."))
  posted_tables <- bind_rows(posted_tables, tibble(id = tbl_id, title = tbl_title, posted_date = as.character(Sys.Date())))
  write_csv(posted_tables, posted_file)
  quit(status = 0)
}

# 6. save and post --------------------------------------------------------------
img_file <- glue("cso_{tbl_id}.png")
ggsave(img_file, plot = p, width = 8, height = 5, dpi = 150, bg = "white")

post_text <- glue(
  "New CSO data: {str_trunc(tbl_title, 100)}\n",
  "\nTable: {tbl_id}\nhttps://data.cso.ie\n",
  "\n#Ireland #OpenData #CSO"
)

bs_post(
  text = post_text,
  images = img_file,
  images_alt = glue("Chart showing CSO table {tbl_id}: {str_trunc(tbl_title, 80)}"),
  user = Sys.getenv("BLUESKY_CSOIE_APP_USER"),
  pass = Sys.getenv("BLUESKY_CSOIE_APP_PASS")
)
message(glue("Posted {tbl_id} to Bluesky."))

# 7. update tracking file -------------------------------------------------------
posted_tables <- bind_rows(
  posted_tables,
  tibble(id = tbl_id, title = tbl_title, posted_date = as.character(Sys.Date()))
)
dir.create("data", showWarnings = FALSE)
write_csv(posted_tables, posted_file)

unlink(img_file)
message("Done.")