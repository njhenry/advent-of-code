## #######################################################################################
##
## ADVENT OF CODE DAY CUATRO
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-04 (ET)
##
## To be executed in R v4.1.0+
##
## #######################################################################################

## SETUP FOR THE DAY
library(httr); library(data.table); library(purrr)

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

input <- load_input_as_text(day = 4)

score_fun <- function(n_matches) if (n_matches == 0) 0 else 2**(n_matches - 1)

get_score <- function(winners, ours) sum(ours %in% winners) |> score_fun()

split_white_space <- function(text) text |> 
  strsplit(split = ' ') |>
  unlist() |>
  stringr::str_trim() |>
  setdiff("")

# Get winning numbers and your numbers for each card
winning_numbers <- lapply(input, function(line){
  substr(line, regexpr(": ", line, fixed = T) + 1, regexpr("|", line, fixed = T) - 1)[[1]] |>
    split_white_space()
})
our_numbers <- lapply(input, function(line){
  substr(line, regexpr("|", line, fixed = T) + 1, nchar(line)) |>
  split_white_space()
})

# Answer to part 1
total_score <- sapply(
  seq_along(input),
  function(ii) get_score(winning_numbers[[ii]], our_numbers[[ii]])
) |> sum()
message(total_score)

# Answer to part 2
n_cards <- rep(1, length(input))
for(ii in seq_along(input)){
  cards_here <- n_cards[ii]
  n_matches <- sum(our_numbers[[ii]] %in% winning_numbers[[ii]])
  if(n_matches > 0){
    n_cards[seq(ii + 1, ii + n_matches)] <- (
      n_cards[seq(ii + 1, ii + n_matches)] + cards_here
    )
  }
}
message(sum(n_cards))

