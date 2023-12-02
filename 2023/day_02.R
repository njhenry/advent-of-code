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

input <- load_input_as_text(day = 2)

## DATA PREP: Load color counts for each round of each game
strip_colon <- function(string) substr(string, regexpr(": ", string) + 2, nchar(string))
build_counts_table <- function(string_vec){
  split_list <- strsplit(string_vec, split = ', ')
  split_table <- lapply(seq_along(split_list), function(round) data.table::data.table(
    round = round,
    count = strsplit(split_list[[round]], split = ' ') |>
      purrr::map(1) |> unlist() |> as.integer(),
    color = strsplit(split_list[[round]], split = ' ') |> purrr::map(2) |> unlist()
  )) |> data.table::rbindlist()
  return(split_table)
}

# Return a list where each list item is a game, and for each game, each member of the
#  string vector is a round
list_of_games <- input |>
  sapply(strip_colon) |>
  strsplit(split = '; ')

# Convert into a table with fields 'game', 'round', 'color', and 'count'
table_of_games_long <- lapply(seq_along(list_of_games), function(game_num){
  counts_table <- build_counts_table(list_of_games[[game_num]])
  counts_table[, game := game_num]
  return(counts_table)
}) |> data.table::rbindlist()

# Reshape
table_of_games_wide <- dcast(table_of_games_long, game + round ~ color, value.var = 'count')
table_of_games_wide[is.na(table_of_games_wide)] <- 0

# Part one: get all games plausible with particular limits
max_by_game <- table_of_games_wide[, lapply(.SD, max), by = game]
valid_games <- max_by_game[(red <= 12) & (green <= 13) & (blue <= 14), ]
pt1_answer <- valid_games[, sum(game)]
message(pt1_answer)

# Part 2: get "power" of each round and sum
max_by_game[, power := red * blue * green ]
pt2_answer <- max_by_game[, sum(power)]
message(pt2_answer)
