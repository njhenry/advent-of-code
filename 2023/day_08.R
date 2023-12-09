## #######################################################################################
##
## ADVENT OF CODE DAY OCHO
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

input_list <- load_input_as_text(day = 8) |> split_on_val(split_val = "")

# L => 1, R => 2
rl <- input_list[[1]] |> strsplit(split = '') |> unlist() |> 
  sapply(function(x) if(x == 'L') 1 else 2)

# Parse network as named list
map_list <- vector('list', length = length(input_list[[2]]))
headers <- vector('character', length = length(input_list[[2]]))
for(ii in seq_along(map_list)){
  this_item_text <- input_list[[2]][ii]
  headers[ii] <- substr(this_item_text, 1, 3)
  map_list[[ii]] <- c(substr(this_item_text, 8, 10), substr(this_item_text, 13, 15))  
}
names(map_list) <- headers

# Some helper functions
get_step_remainder <- function(step_counts, size = length(rl)){
  remainders <- step_counts %% size
  remainders[remainders == 0 ] <- size
  return(remainders)
}
map_apply_step <- function(locs, next_step, the_map = map_list){
  Map(f = function(x) x[next_step], the_map[locs]) |> unlist()
}

# Part 1 solution
current_loc <- 'AAA'
end_loc <- 'ZZZ'
step_count <- 0

while(current_loc != end_loc){
  step_count <- step_count + 1
  # Repeat list items if necessary
  step_remainder <- get_step_remainder(step_count)
  next_step <- rl[step_remainder]
  current_loc <- map_apply_step(current_loc, next_step)
}
message(step_count)

# Part 2 solutions
start_locs <- grep("A$", headers, value = T)
possible_solutions <- grep("Z$", headers, value = T)
check_end_condition <- function(locs) all(locs %in% possible_solutions)

solutions_list <- lapply(start_locs, function(x){
  y <- vector('list', length = length(possible_solutions))
  names(y) <- possible_solutions
  return(y)
})
names(solutions_list) <- start_locs

check_period_condition <- function(step_counts){
  max(table(get_step_remainder(step_counts))) > 1
}

# Check solutions for each start point separately and find the map period
for(start_loc in start_locs){
  message("Finding period starting at ", start_loc)
  current_loc <- start_loc
  step_count <- 0
  period_condition_met <- FALSE
  while(!period_condition_met){
    step_count <- step_count + 1
    step_remainder <- get_step_remainder(step_count)
    next_step <- rl[step_remainder]
    current_loc <- map_apply_step(current_loc, next_step)
    end_condition_met <- check_end_condition(current_loc)
    if(end_condition_met){
      # Append the latest step count that matches the starting position to this solution
      solutions_list[[start_loc]][[current_loc]] <- c(
        solutions_list[[start_loc]][[current_loc]],
        step_count
      )
      # If the step remainder for this solution is the same as for any previous step
      #  count, we have found the period!
      period_condition_met <- check_period_condition(
        solutions_list[[start_loc]][[current_loc]]
      )
    }
  }
  message("Period for ", start_loc, " found after ", step_count, " steps.")
}

# Coincidentally, all of the starting points have periods that are multiples of the
#  step list
periods <- sapply(start_locs, function(sl) min(unlist(solutions_list[[sl]])))
if(any(get_step_remainder(periods) != length(rl))){
  stop("Periods not the length of the instructions list")
}

# Simply find the least common multiple of all periods
p2_solution <- DescTools::LCM(periods)
message(p2_solution)
