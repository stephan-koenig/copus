library(conflicted)
library(dplyr)
conflicts_prefer(dplyr::filter)

get_labels <- function(codes, code_type) {
  codes |> 
    filter(type == code_type) |>
    mutate(label = stringr::str_c(type, label, sep = "_")) |> 
    select(label, code) |> 
    tibble::deframe()
}

get_data <- function(copus_path, codes_path) {
  
  codes <- readr::read_csv(code_path)
  
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


