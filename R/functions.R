library(conflicted)
library(dplyr)
conflicts_prefer(dplyr::filter)
library(fs)
library(ggplot2)
library(gt)
library(purrr)
library(stringr)
library(tidyr)

get_labels <- function(codes_df, code_subject) {
  codes_df |>
    filter(subject == code_subject) |>
    mutate(label = str_c(subject, label, sep = "_")) |>
    select(label, code) |>
    tibble::deframe()
}

load_copus <- function(copus_path, codes_df) {
  students <-
    readxl::read_xlsx(
      copus_path,
      range = cellranger::cell_limits(c(5, 2), c(NA, 14))
    ) |>
    # Expected number of rows for either 50 or 80 min based on
    # `COPUS-templates_UseToRecordElectronically.xlsx`
    filter(row_number() <= ifelse(n() == 46, 40, 25)) |>
    rename(get_labels(codes_df, "students"))

  instructors <-
    readxl::read_xlsx(
      copus_path,
      range = cellranger::cell_limits(c(5, 15), c(NA, 26))
    ) |>
    filter(row_number() <= ifelse(n() == 46, 40, 25)) |>
    rename(get_labels(codes_df, "instructors"))

  students |>
    bind_cols(instructors) |>
    mutate(
      across(everything(), \(x) !is.na(x)),
      min = seq(2, 2 * n(), by = 2),
      .before = 1
    )
}

get_collapsed_codes <- function(codes_df) {
  codes_df |>
    mutate(
      label = str_c(subject, label, sep = "_"),
      collapsed_code = str_c(subject, collapsed_code, sep = "_")
    ) |>
    select(label, collapsed_code)
}

add_collapsed_codes_to_copus <- function(copus_df, codes_df) {
  collapsed_codes_df <- get_collapsed_codes(codes_df)

  collapsed_copus <- copus_df |>
    pivot_longer(
      students_listening:instructors_other,
      names_to = "label",
      values_to = "present"
    ) |>
    inner_join(
      collapsed_codes_df,
      by = join_by(label),
      relationship = "many-to-many"
    ) |>
    summarize(
      present = any(present),
      .by = c(date, course, instructor, min, collapsed_code)
    ) |>
    arrange(collapsed_code) |>
    pivot_wider(
      names_from = collapsed_code,
      values_from = present,
      values_fill = FALSE
    ) |>
    relocate(
      starts_with("students"),
      starts_with("instructors"),
      .after = last_col()
    )

  copus_df |>
    inner_join(
      collapsed_copus,
      by = join_by(date, course, instructor, min)
    )
}

process_copus <- function(copus_dir, codes_path) {
  codes_df <- readr::read_csv(codes_path, show_col_types = FALSE)

  copus_dir |>
    dir_ls(glob = "*.xlsx") |>
    set_names(path_file) |>
    map(\(path) load_copus(path, codes_df)) |>
    list_rbind(names_to = "file") |>
    tidyr::separate_wider_delim(
      file,
      delim = "_",
      names = c("date", NA, "course", "instructor")
    ) |>
    mutate(
      date = lubridate::ymd(date),
      across(course:instructor, \(x) str_replace_all(x, "-", " ")),
      instructor = str_remove(instructor, ".xlsx")
    ) |>
    add_collapsed_codes_to_copus(codes_df)
}

merge_segments <- function(end_times_df) {
  # If two segments are contiguous, the end time of the first is the start
  # time of the next and the number is shared among them and can be removed
  end_time_with_shared <- end_times_df |> pull(min)
  start_time_with_shared <- end_time_with_shared - 2
  start_time <- setdiff(start_time_with_shared, end_time_with_shared)
  end_time <- setdiff(end_time_with_shared, start_time_with_shared)
  tibble(start_time, end_time)
}

prepare_copus_for_plotting <- function(copus_df, ...) {
  # Use ... for codes to use, either original codes or collapsed ones
  copus_df |>
    select(date:min, ...) |>
    pivot_longer(
      ...,
      names_to = "code",
      values_to = "present"
    ) |>
    separate_wider_regex(
      code,
      patterns = c(subject = "^[:alpha:]+", "_", code = ".+")
    ) |>
    mutate(
      code = code |>
        # Included for collapsed codes
        str_remove("collapsed_") |>
        str_replace_all("_", " ") |>
        str_to_sentence()
    )
}

plot_copus_timelines <- function(copus_long_df) {
  copus_long_df |>
    filter(present) |>
    nest(segments = min) |>
    mutate(segments = map(segments, merge_segments)) |>
    unnest(segments) |>
    arrange(start_time) |>
    mutate(code = factor(code, levels = unique(code))) |>
    ggplot(
      aes(
        x = start_time, xend = end_time,
        y = code,
        color = subject
      )
    ) +
    geom_segment(linetype = 1, linewidth = 4) +
    labs(x = "Time [min]", y = NULL) +
    theme(legend.position = "none") +
    facet_grid(rows = vars(subject), cols = vars(date), scales = "free_y")
}

plot_codes_timelines <- function(copus_df) {
  copus_df |>
    prepare_copus_for_plotting(students_listening:instructors_other) |>
    plot_copus_timelines()
}

plot_collapsed_codes_timelines <- function(copus_df) {
  copus_df |>
    prepare_copus_for_plotting(
      students_collapsed_other:instructors_presenting
    ) |>
    plot_copus_timelines()
}

plot_copus_bars <- function(copus_long_df) {
  copus_long_df |>
    summarize(proportion = mean(present), .by = c(date, subject, code)) |>
    filter(proportion != 0) |>
    ggplot(aes(x = proportion, y = code, fill = subject)) +
    geom_col() +
    scale_x_continuous(name = "Percent", labels = scales::label_percent()) +
    ylab(label = NULL) +
    theme(legend.position = "none") +
    facet_grid(rows = vars(subject), cols = vars(date), scales = "free_y")
}

plot_codes_bars <- function(copus_df) {
  copus_df |>
    prepare_copus_for_plotting(students_listening:instructors_other) |>
    plot_copus_bars()
}

plot_collapsed_codes_bars <- function(copus_df) {
  copus_df |>
    prepare_copus_for_plotting(
      students_collapsed_other:instructors_presenting
    ) |>
    plot_copus_bars()
}

get_code_descritpion_table <- function(codes_path) {
  readr::read_csv(codes_path, show_col_types = FALSE) |>
    arrange(subject, collapsed_code, label) |>
    select(subject, collapsed_code, label, description) |>
    mutate(
      across(
        subject:label,
        ~ .x |>
          str_remove("collapsed_") |>
          str_replace_all("_", " ") |>
          str_to_sentence()
      )
    ) |>
    gt(groupname_col = "subject") |>
    cols_label(
      label          = "Activity",
      collapsed_code = "Collapsed code",
      description    = "Description"
    )
}
