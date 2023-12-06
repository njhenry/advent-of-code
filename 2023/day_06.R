## #######################################################################################
##
## ADVENT OF CODE DAY SIX
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-06 (ET)
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

input <- load_input_as_text(day = 6)

times <- substr(input[1], 10, nchar(input[1]))[[1]] |> strsplit(split = ' ') |>
  unlist() |> setdiff('') |> as.integer()
min_dist <- substr(input[2], 10, nchar(input[2]))[[1]] |> strsplit(split = ' ') |>
  unlist() |> setdiff('') |> as.integer()

button_min_times <- floor(sqrt(min_dist)) + 1

p1_result <- 1
for(ii in 1:length(times)){
  hold_options <- seq_len(times[ii])
  winning_times <- sum(hold_options * (times[ii] - hold_options) > min_dist[ii])
  p1_result <- p1_result * winning_times
}

# Part 2
p2_time <- times |> as.character() |> paste(collapse = '') |> as.double()
p2_min_dist <- min_dist |> as.character() |> paste(collapse = '') |> as.double()

# We're just brute forcing it ¯\_(ツ)_/¯
p2_hold_options <- seq_len(p2_time)
p2_result <- sum(p2_hold_options * (p2_time - p2_hold_options) > p2_min_dist) 
