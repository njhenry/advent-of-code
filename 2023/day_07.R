## #######################################################################################
##
## ADVENT OF CODE DAY 7
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-07 (ET)
##
## To be executed in R v4.1.0+
##
## #######################################################################################

## SETUP FOR THE DAY
library(httr); library(data.table); library(purrr); library(stringr); library(data.table)
library(gtools)

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

input <- load_input_as_text(day = 7) |> gsub(pattern = ' ', replacement = ',') |>
  paste(collapse = '\n') |> data.table::fread()
colnames(input) <- c('hand', 'bid')

fill_to_five <- function(vec) c(vec, rep(NA_character_, 5 - length(vec)))

hand_type <- function(chars){
  chars_dt <- chars |> strsplit('') |>
    lapply(table) |> lapply(sort, decreasing = T) |> lapply(fill_to_five) |> 
    unlist() |> as.vector() |> matrix(byrow = T, ncol = 5) |> as.data.table()
  (chars_dt
    [, type := 'HC' ]
    [V1 == 2, type := '1P']
    [(V1 == 2) & (V2 == 2), type := '2P']
    [V1 == 3, type := '3K']
    [(V1 == 3) & (V2 == 2), type := 'FH']
    [V1 == 4, type := '4K']
    [V1 == 5, type := '5K']
  )
  return(chars_dt$type)
}

input$type <- hand_type(input$hand) |> factor(levels = hand_levels)

card_levels <- c('A','K','Q','J','T','9','8','7','6','5','4','3','2')

for(ii in 1:5){
  input[[paste0('c', ii)]] <- factor(
    input[, substr(hand, ii, ii)],
    levels = card_levels
  )
}

sorted_input <- input[order(-type, -c1, -c2, -c3, -c4, -c5)][, rank := .I]

# P1 answer
total_score <- sorted_input[, sum(bid * rank)]
message(total_score)

## PART 2
card_levels_no_j <- setdiff(card_levels, 'J')
card_levels_p2 <- c(card_levels_no_j, 'J')

hand_type_special_jacks <- function(chars){
  chars_no_jacks <- gsub('J', '', chars)
  n_jacks <- 5 - nchar(chars_no_jacks)
  jack_possibilities <- gtools::combinations(
    n = length(card_levels_no_j), v = card_levels_no_j, r = n_jacks,
    repeats.allowed = TRUE
  ) |> apply(MARGIN = 1, FUN = function(x) paste0(x, collapse = ''))
  possible_types <- hand_type(paste0(chars_no_jacks, jack_possibilities)) |>
    factor(levels = hand_levels)
  return(sort(possible_types, decreasing = F)[1] |> as.character())
}

input_p2 <- load_input_as_text(day = 7) |> gsub(pattern = ' ', replacement = ',') |>
  paste(collapse = '\n') |> data.table::fread()
colnames(input_p2) <- c('hand', 'bid')

has_jacks <- grepl('J', input_p2$hand)
input_p2[!has_jacks, type := hand_type(hand) ]
for(row_i in which(has_jacks)){
  message(row_i)
  input_p2$type[row_i] <- hand_type_special_jacks(input_p2$hand[row_i])
}
input_p2$type <- factor(input_p2$type, levels = hand_levels)

for(ii in 1:5){
  input_p2[[paste0('c', ii)]] <- factor(
    input_p2[, substr(hand, ii, ii)],
    levels = card_levels_p2
  )
}

# P2 answer
sorted_input_p2 <- input_p2[order(-type, -c1, -c2, -c3, -c4, -c5)][, rank := .I]
total_score_p2 <- sorted_input_p2[, sum(bid * rank)]
message(total_score_p2)
