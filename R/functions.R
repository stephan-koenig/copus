library(conflicted)
library(dplyr)
conflicts_prefer(dplyr::filter)
library(fs)
library(purrr)

get_labels <- function(codes, code_type) {
  codes |> 
    filter(type == code_type) |>
    mutate(label = stringr::str_c(type, label, sep = "_")) |> 
    select(label, code) |> 
    tibble::deframe()
}

load_copus <- function(copus_path, codes_path) {
  
  codes <- readr::read_csv(codes_path, show_col_types = FALSE)
  
  students <- readxl::read_xlsx(copus_path, range = "A5:N45") |> 
    rename(get_labels(codes, "students"))
  
  instructors <- readxl::read_xlsx(copus_path, range = "O5:Z45") |> 
    rename(get_labels(codes, "instructors"))
  
  students |> 
    bind_cols(instructors) |> 
    mutate(
      across(everything(), ~ tidyr::replace_na(.x, 0)),
      across(!min, as.logical)
    )
}

process_copus <- function(copus_dir, codes_path) {
  copus_dir |>
    dir_ls(glob = "*.xlsx") |>
    set_names(path_file) |> 
    map(\(path) load_copus(path, codes_path = codes_path)) |>
    list_rbind(names_to = "file") |> 
    tidyr::separate_wider_delim(
      file,
      delim = "_",
      names = c("date", NA, "course", "instructor")
    ) |>
    mutate(
      date = lubridate::ymd(date),
      across(course:instructor, \(x) stringr::str_replace_all(x, "-", " ")),
      instructor = stringr::str_remove(instructor, ".xlsx")
    )
}
