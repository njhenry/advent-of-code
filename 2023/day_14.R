## #######################################################################################
##
## ADVENT OF CODE DAY 14
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

input <- load_input_as_text(day = 14) |> strsplit(split = '') |> do.call(what = 'rbind')

# Iteratively move north
calc_weight <- function(input){
  n_rows <- nrow(input)
  n_cols <- ncol(input)
  max_row <- as.integer(input[1, ] != '.')
  for(row_i in seq(2, n_rows)){
    for(col_i in seq(1, n_cols)){
      if(input[row_i, col_i] == 'O'){
        max_row_val <- max_row[col_i]
        if(max_row_val < (row_i - 1)){
          input[row_i, col_i] <- '.'
          input[max_row_val + 1, col_i] <- 'O'
          max_row[col_i] <- max_row_val + 1
        } else {
          max_row[col_i] <- row_i
        }
      } else if(input[row_i, col_i] == '#'){
          max_row[col_i] <- row_i
      }
    }
  }
  # Calculate total weight
  total_weight <- lapply(seq_len(n_rows), function(row_idx){
    row_weight <- n_rows - row_idx + 1
    return(sum(input[row_idx, ] == 'O') * row_weight)
  }) |> unlist() |> sum()
  return(list(output = input, total_weight = total_weight))
}
p1_answer <- calc_weight(input)$total_weight
message(p1_answer)

# Function to rotate a list in the N-W-S-E direction
rotate_cw <- function(x) t(apply(x, 2, rev))

full_cycle <- function(input){
  for(ii in 1:4){
    output_list <- calc_weight(input)
    input <- rotate_cw(output_list$output)
  }
  total_weight <- lapply(seq_len(nrow(input)), function(row_idx){
    row_weight <- nrow(input) - row_idx + 1
    return(sum(input[row_idx, ] == 'O') * row_weight)
  }) |> unlist() |> sum()
  return(list(output = input, total_weight = total_weight))
}

input_p2 <- load_input_as_text(day = 14) |> strsplit(split = '') |> do.call(what = 'rbind')
history_list <- vector('list', length = 1e5)
iteration <- 0
found_period <- FALSE

# At some point, the rocks will be in the exact same spot as in a previous cycle
# From there on, the same pattern repeats indefinitely
while(!found_period){
  iteration <- iteration + 1
  message('.', appendLF = F)
  history_list[[iteration]] <- full_cycle(input_p2)
  input_p2 <- history_list[[iteration]]$output
  # Compare to all previous arrangements
  for(compare_iteration in seq_len(iteration - 1)){
    if(all(history_list[[compare_iteration]]$output == input_p2)){
      found_period <- TRUE
    }
  }
}
starting_iteration <- sapply(
  seq_len(iteration - 1),
  function(ii) all(history_list[[ii]]$output == input_p2)
) |> which()
period <- iteration - starting_iteration

# Find the spot in the cycle matching the target number of iterations
target <- 1000000000
cycle_idx <- (target - starting_iteration) %% period
matching_sequence <- starting_iteration + cycle_idx
p2_answer <- history_list[[matching_sequence]]$total_weight
message(p2_answer)

input_p2 <- c(
  "O....#....",
  "O.OO#....#",
  ".....##...",
  "OO.#O....O",
  ".O.....O#.",
  "O.#..O.#.#",
  "..O..#O..O",
  ".......O..",
  "#....###..",
  "#OO..#...."
) |> strsplit(split = '') |> unlist() |> matrix(ncol = 10, byrow = TRUE)
