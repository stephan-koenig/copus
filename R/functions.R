library(conflicted)
library(dplyr)
conflicts_prefer(dplyr::filter)
library(fs)
library(ggplot2)
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
  students <- readxl::read_xlsx(copus_path, range = "A5:N45") |>
    rename(get_labels(codes_df, "students"))
  
  instructors <- readxl::read_xlsx(copus_path, range = "O5:Z45") |>
    rename(get_labels(codes_df, "instructors"))
  
  students |>
    bind_cols(instructors) |>
    mutate(
      across(everything(), ~ tidyr::replace_na(.x, 0)),
      across(!min, as.logical)
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
    filter(present) |>
    inner_join(
      collapsed_codes_df,
      by = join_by(label),
      relationship = "many-to-many"
    ) |>
    select(!label) |>
    distinct() |>
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

merge_segments <- function(df) {
  # If two segments are contiguous, the end time of the first is the start
  # time of the next and the number is shared among them and can be removed
  end_time_with_shared   <- df |> pull(min)
  start_time_with_shared <- end_time_with_shared - 2
  start_time <- setdiff(start_time_with_shared, end_time_with_shared)
  end_time   <- setdiff(end_time_with_shared, start_time_with_shared)
  tibble(start_time, end_time)
}

plot_copus_timelines <- function(copus_df, codes_path) {
  collapsed_codes_df <- readr::read_csv(codes_path, show_col_types = FALSE) |> 
    get_collapsed_codes() |> 
    mutate(
      collapsed_code = collapsed_code |>
        str_remove("(students|instructors)_") |>
        str_replace_all("_", " ") |> 
        str_to_sentence()
    ) |> 
    rename(code = label)

  copus_df |>
    select(date:instructors_other) |>
    pivot_longer(
      students_listening:instructors_other,
      names_to = "code",
      values_to = "present"
    ) |>
    inner_join(collapsed_codes_df, by = join_by(code)) |>
    separate_wider_regex(
      code,
      patterns = c(subject = "^[:alpha:]+", "_", code = ".+")
    ) |>
    filter(present) |>
    nest(segments = min) |>
    mutate(
      code = code |> str_replace_all("_", " ") |> str_to_sentence(),
      segments = map(segments, merge_segments)
    ) |> 
    unnest(segments) |>
    ggplot(
      aes(
        x = start_time, xend = end_time,
        y = code,
        color = subject
      )
    ) +
    geom_segment(linetype = 1, linewidth = 4) +
    facet_grid(rows = vars(subject), cols = vars(date), scales = "free_y")
}
