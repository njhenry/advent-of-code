## #######################################################################################
##
## ADVENT OF CODE DAY 09
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-07 (ET)
##
## To be executed in R v4.1.0+
##
## #######################################################################################

## SETUP FOR THE DAY
library(httr); library(data.table); library(purrr); library(stringr)

load_input_as_text <- function(day, year = 2023, cache_dir = Sys.getenv('AOC_CACHE')){
  cache_file <- glue::glue("{cache_dir}/{year}-12-{day}.txt")
  if(file.exists(cache_file)){
    input <- readLines(cache_file)
  } else {
    input <- glue::glue("http://adventofcode.com/{year}/day/{day}/input") |>
      httr::GET(
        config = httr::set_cookies(session = Sys.getenv("AOC_SESSION_COOKIE"))
      ) |>
      httr::content(as = 'text') |>
      stringr::str_split('\\n') |>
      unlist()
    # Only cache if it's valid input
    if(startsWith(input[1], "Please don't repeatedly")){
      stop("Input not ready yet!")
    } else {
      writeLines(input, con = cache_file)
    }
  }
  if(data.table::last(input) == '') input <- input[seq_len(length(input) - 1)]
  return(input)
}

input_list <- load_input_as_text(day = 9)

get_next_val <- function(int_vec){
  diff_vec <- diff(int_vec)
  if(uniqueN(diff_vec) == 1){
    return(last(int_vec) + last(diff_vec))
  } else {
    return(last(int_vec) + get_next_val(diff_vec))
  }
}

get_previous_val <- function(int_vec){
  diff_vec <- diff(int_vec)
  if(uniqueN(diff_vec) == 1){
    return(first(int_vec) - first(diff_vec))
  } else {
    return(first(int_vec) - get_previous_val(diff_vec))
  }
}

int_list <- strsplit(input_list, split = ' ') |> lapply(as.integer)
p1_answer <- lapply(int_list, get_next_val) |> unlist() |> sum()
message(p1_answer)
p2_answer <- lapply(int_list, get_previous_val) |> unlist() |> sum()
message(p2_answer)
