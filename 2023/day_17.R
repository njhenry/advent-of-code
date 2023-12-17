## #######################################################################################
##
## ADVENT OF CODE DAY 17 (take 2)
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-10 (ET)
##
## To be executed in R v4.1.0+
##
## #######################################################################################

## SETUP FOR THE DAY
library(httr); library(data.table); library(stringr); library(sf); library(R6)

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

# Costs matrix from source (1,1) to destination (141,141)
base_costs_vector <- load_input_as_text(day = 17) |>
  strsplit(split = '') |>
  unlist() |>
  as.integer()
# Rows and columns of the surface
MAX_DIM <- sqrt(length(base_costs_vector))
# Convert to table with row and column IDs
base_costs_dt <- data.table::data.table(
  row = rep(seq_len(MAX_DIM), each = MAX_DIM),
  col = rep(seq_len(MAX_DIM), times = MAX_DIM),
  base_cost = base_costs_vector
)

# Get valid next steps given previous steps
directions <- c("N","S","E","W")
opposite <- function(d){
  if(d == 'N') return('S')
  if(d == 'S') return('N')
  if(d == 'E') return('W')
  if(d == 'W') return('E')
  stop("Issue with input")
}
valid_next_steps <- function(history){
  history <- strsplit(history, split = '') |> unlist()
  possible <- directions
  # No immediate u-turns
  possible <- setdiff(possible, opposite(data.table::last(history)))
  # No 4 of the same direction in a row
  if((length(history) >= 3) & (uniqueN(history) == 1)){
    possible <- setdiff(possible, history[1])
  }
  return(possible)
}
# Function to step histories
step_histories <- function(histories, next_steps){
  new_histories <- paste0(histories, next_steps)
  to_shorten <- (nchar(new_histories) > 3)
  new_histories[to_shorten] <- gsub('^.', '', new_histories[to_shorten])
  return(new_histories)
}

# Get all valid histories
all_histories <- directions
for(d1 in directions){
  dir_2 <- valid_next_steps(d1)
  all_histories <- c(all_histories, paste(d1, dir_2, sep = ''))
  for(d2 in dir_2){
    dir_3 <- valid_next_steps(c(d1, d2))
    all_histories <- c(all_histories, paste(d1, d2, dir_3, sep = ''))
  }
}
# Create a table expanding all possible histories for each grid cell
grid_histories_dt <- merge(
  x = base_costs_dt[, merge_on := 1 ],
  y = data.table::data.table(history = all_histories, merge_on = 1),
  by = 'merge_on',
  allow.cartesian = TRUE
)[, merge_on := NULL ][, min_cost := Inf ]

# Create a table expanding out all possible next steps given a 3-step history
next_steps_template_dt <- lapply(all_histories, function(hist){
  data.table::data.table(old_history = hist, next_step = valid_next_steps(hist))
}) |> data.table::rbindlist()
(next_steps_template_dt
  # Add next history
  [, history := step_histories(old_history, next_step)]
  # Add change in row and column based on step direction
  [, c('dx', 'dy') := 0 ]
  [next_step == "N", dy := -1 ][next_step == "S", dy := 1 ]
  [next_step == "W", dx := -1 ][next_step == "E", dx := 1 ]
)

# Iteratively step cost accumulators
next_steps <- data.table::data.table(
  row = c(1, 2), col = c(2, 1), history = c('E','S'), min_cost = 0
)
max_iterations <- MAX_DIM**2
iterations <- 1
while((nrow(next_steps) > 0) & (iterations <= max_iterations)){
  if(iterations %% 100 == 0) message(iterations, '. ', appendLF = F)
  # Merge on next steps
  (grid_histories_dt
    [next_steps, check_cost := i.min_cost + base_cost, on = c('row','col','history')]
    [, update := (check_cost < min_cost) ]
    [update == TRUE, min_cost := check_cost ]
  )
  # Merge on possible next steps
  next_steps <- merge(
    x = grid_histories_dt[update == TRUE, .(row, col, min_cost, old_history = history)],
    y = next_steps_template_dt,
    on = 'old_history',
    allow.cartesian = TRUE
  )
  # Step locations, drop invalid locations
  next_steps[, row := row + dy ][, col := col + dx ]
  next_steps <- next_steps[(row >= 1) & (col >= 1) & (row <= MAX_DIM) & (col <= MAX_DIM)]
  # Get smallest possible cost by new location and history
  next_steps <- next_steps[, .(min_cost = min(min_cost)), by = .(history, row, col)]
  # Clean up
  iterations <- iterations + 1
  grid_histories_dt[, `:=` (check_cost = NA_real_, update = FALSE), ]
}

# P1 solution
message(grid_histories_dt[(row == MAX_DIM) & (col == MAX_DIM), min(min_cost)])


## Part 2: Revenge of the Ultra Crucibles ----------------------------------------------->

# Ultra crucibles have a new way to record histories and new step patterns
uc_histories <- directions
for(ii in 2:10){
  for(dir in directions){
    uc_histories <- c(uc_histories, paste(rep(dir, ii), collapse = ''))
  }
}
step_histories_uc <- function(histories, next_steps){
  last_directions <- substr(histories, nchar(histories), nchar(histories))
  new_histories <- paste0(histories, next_steps)
  to_swap <- which(next_steps != last_directions)
  new_histories[to_swap] <- next_steps[to_swap]
  return(new_histories)
}
valid_next_steps_uc <- function(history){
  history <- strsplit(history, split = '') |> unlist()
  last_direction <- data.table::last(history)
  possible <- directions
  # For less than three in a row, must move the same direction
  if(length(history) < 4){
    possible <- last_direction
  } else if((length(history) >= 4) & (length(history) < 10)){
    # For steps 4 through 9, can move straight, left, or right
    possible <- setdiff(possible, opposite(last_direction))
  } else {
    # For step 10, MUST turn left or right
    possible <- setdiff(possible, c(last_direction, opposite(last_direction)))
  }
  return(possible)
}
# Create a table expanding out all possible next steps given direction history
next_steps_template_dt_uc <- lapply(uc_histories, function(hist){
  data.table::data.table(old_history = hist, next_step = valid_next_steps_uc(hist))
}) |> data.table::rbindlist()
(next_steps_template_dt_uc
  # Add next history
  [, history := step_histories_uc(old_history, next_step)]
  # Add change in row and column based on step direction
  [, c('dx', 'dy') := 0 ]
  [next_step == "N", dy := -1 ][next_step == "S", dy := 1 ]
  [next_step == "W", dx := -1 ][next_step == "E", dx := 1 ]
)

# Create a table expanding all possible histories for each grid cell
grid_histories_dt_uc <- merge(
  x = base_costs_dt[, merge_on := 1 ],
  y = data.table::data.table(history = uc_histories, merge_on = 1),
  by = 'merge_on',
  allow.cartesian = TRUE
)[, merge_on := NULL ][, min_cost := Inf ]

# Iteratively step cost accumulators
next_steps_uc <- data.table::data.table(
  row = c(1, 2), col = c(2, 1), history = c('E','S'), min_cost = 0
)
max_iterations <- 360
iterations <- 1
while((nrow(next_steps_uc) > 0) & (iterations <= max_iterations)){
  if(iterations %% 100 == 0) message(iterations, '. ', appendLF = F)
  # Merge on next steps
  (grid_histories_dt_uc
    [next_steps_uc, check_cost := i.min_cost + base_cost, on = c('row','col','history')]
    [, update := (check_cost < min_cost) ]
    [update == TRUE, min_cost := check_cost ]
  )
  # Merge on possible next steps
  next_steps_uc <- merge(
    x = grid_histories_dt_uc[update == TRUE, .(row, col, min_cost, old_history = history)],
    y = next_steps_template_dt_uc,
    on = 'old_history',
    allow.cartesian = TRUE
  )
  # Step locations, drop invalid locations
  next_steps_uc[, row := row + dy ][, col := col + dx ]
  next_steps_uc <- next_steps_uc[(row >= 1) & (col >= 1) & (row <= MAX_DIM) & (col <= MAX_DIM)]
  # Get smallest possible cost by new location and history
  next_steps_uc <- next_steps_uc[, .(min_cost = min(min_cost)), by = .(history, row, col)]
  # Clean up
  iterations <- iterations + 1
  grid_histories_dt_uc[, `:=` (check_cost = NA_real_, update = FALSE), ]
}

# P2 solution
# Constrained b/c the ultra crucibles need to travel 4 squares straight before stopping
message(
  grid_histories_dt_uc[
    (nchar(history) >= 4) & (row == MAX_DIM) & (col == MAX_DIM),
    min(min_cost)
  ]
)
