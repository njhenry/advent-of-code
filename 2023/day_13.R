## #######################################################################################
##
## ADVENT OF CODE DAY 13
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-14 (ET)
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
  out_list <- lapply(seq(max(list_idx)), function(ii){
    split_on <- char_vec[list_idx == ii]
    return(split_on[split_on != ''])
  }
  )
  # If the input ends with the split line (e.g. a blank line), drop it
  if(length(out_list[[length(out_list)]]) == 0) out_list[[length(out_list)]] <- NULL
  return(out_list)
}

input <- load_input_as_text(day = 13) |> split_on_val('')

check_reflection_h <- function(grid, line){
  keep_rows <- min(line, length(grid) - line)
  before <- grid[seq(line - keep_rows + 1, line)]
  after <- grid[seq(line + 1, line + keep_rows)] |> rev()
  return(all(after == before))
}
check_reflection_v <- function(grid, line){
  keep_cols <- min(line, nchar(grid)[1] - line)
  before <- substr(grid, line - keep_cols + 1, line)
  after <- substr(grid, line + 1, line + keep_cols) |> stringi::stri_reverse()
  return(all(before == after))
}

score <- 0
for(grid in input){
  for(row in seq_len(length(grid) - 1)){
    if(check_reflection_h(grid, row)){
      score <- score + 100 * row
      message('!', appendLF = F)
    }
  }
  for(col in seq_len(nchar(grid[1]) - 1)){
    if(check_reflection_v(grid, col)){
      score <- score + col
      message('.', appendLF = F)
    }
  }
}
message(score)

# PART 2 -------------------------------------------------------------------------------->

check_reflection_h_p2 <- function(grid, line){
  keep_rows <- min(line, length(grid) - line)
  before <- grid[seq(line - keep_rows + 1, line)]
  after <- grid[seq(line + 1, line + keep_rows)] |> rev()
  if(sum(after != before) == 1){
    check_index <- which(after != before)
    after_split <- strsplit(after[check_index], split = '') |> unlist()
    before_split <- strsplit(before[check_index], split = '') |> unlist()
    if(sum(after_split != before_split) == 1) return(TRUE)
  }
  return(FALSE)
}
check_reflection_v_p2 <- function(grid, line){
  keep_cols <- min(line, nchar(grid)[1] - line)
  before <- substr(grid, line - keep_cols + 1, line)
  after <- substr(grid, line + 1, line + keep_cols) |> stringi::stri_reverse()
  if(sum(after != before) == 1){
    check_index <- which(after != before)
    after_split <- strsplit(after[check_index], split = '') |> unlist()
    before_split <- strsplit(before[check_index], split = '') |> unlist()
    if(sum(after_split != before_split) == 1) return(TRUE)
  }
  return(FALSE)
}

score <- 0
for(ii in seq_along(input)){
  message(ii, appendLF = F)
  grid <- input[[ii]]
  for(row in seq_len(length(grid) - 1)){
    if(check_reflection_h_p2(grid, row)){
      score <- score + 100 * row
      message('!', appendLF = F)
    }
  }
  for(col in seq_len(nchar(grid[1]) - 1)){
    if(check_reflection_v_p2(grid, col)){
      score <- score + col
      message('.', appendLF = F)
    }
  }
}
message(score)
