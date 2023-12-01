## #######################################################################################
##
## ADVENT OF CODE DAY UNO
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-01 (ET)
##
## To be executed in R v4.1.0+
##
## #######################################################################################

## SETUP FOR THE DAY
library(httr); library(data.table)

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
    if(startsWith(input, "Please don't repeatedly")){
      stop("Input not ready yet!")
    } else {
      writeLines(input, con = cache_file)
    }
  }
  return(input)
}

split_on_val <- function(char_vec, split_val){
  list_idx <- cumsum(char_vec == split_val) + 1
  out_list <- lapply(
    seq(max(list_idx)),
    function(ii) char_vec[list_idx == ii] |> setdiff(y = "")
  )
  # If the input ends with the split line (e.g. a blank line), drop it
  if(length(out_list[[length(out_list)]]) == 0) out_list[[length(out_list)]] <- NULL
  return(out_list)
}

input <- load_input_as_text(day = 1)


## FIRST PART: Just match numerals
total_val <- lapply(input, function(x) gsub('[[:alpha:]]', '', x)) |>
  lapply(function(vv) paste0(substr(vv, 1, 1), substr(vv, nchar(vv), nchar(vv)))) |>
  lapply(as.integer) |>
  unlist() |>
  sum(na.rm = T)


## SECOND PART: Match the first and last numerals OR number words
valid <- c(
  'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
  as.character(1:9)
)
valid_map <- c(1:9, 1:9)
names(valid_map) <- valid
first_digits <- regmatches(input, gregexpr(paste(valid, collapse='|'), input)) |>
  lapply(data.table::first) |>
  lapply(function(dig) valid_map[dig])

# To avoid text overlaps, do a reverse regex on the back end of the script
reverse_string <- function(x) strsplit(x, '') |> 
  data.table::first() |> 
  rev() |> 
  paste0(collapse='')
valid_rev <- sapply(valid, reverse_string)
valid_map_rev <- valid_map
names(valid_map_rev) <- valid_rev

input_rev <- sapply(input, reverse_string)
last_digits <- regmatches(input_rev, gregexpr(paste(valid_rev, collapse='|'), input_rev)) |>
  lapply(data.table::first) |>
  lapply(function(dig) valid_map_rev[dig])

line_vals_part_2 <- seq_along(input) |>
  lapply(function(ii) as.integer(paste0(first_digits[[ii]], last_digits[[ii]]))) |>
  unlist() |>
  unname()

answer_part_2 <- sum(line_vals_part_2)
