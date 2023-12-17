## #######################################################################################
##
## ADVENT OF CODE DAY 16
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

# Mirrors matrix
mirrors_matrix <- load_input_as_text(day = 16) |>
  lapply(function(row) strsplit(row, '')[[1]] |> matrix(nrow = 1)) |>
  do.call(what = 'rbind')

# Directions matrix to contain previously encountered directions by grid cell
directions_matrix <- matrix('', nrow = nrow(input_matrix), ncol = ncol(input_matrix))
# 0/1 matrix to contain energized state
energy_matrix <- matrix(0, nrow = nrow(input_matrix), ncol = ncol(input_matrix))

store_direction <- function(beam){
  dd <- directions_matrix[beam$y, beam$x]
  dd <- paste0(dd, beam$direction)
  directions_matrix[beam$y, beam$x] <<- dd
  invisible(NULL)
}
matches_previous_direction <- function(beam){
  dd <- directions_matrix[beam$y, beam$x]
  return(grepl(beam$direction, dd))
}

convert_direction <- function(direction, mirror){
  if(mirror == '.'){
    return(direction)
  } else if(mirror == '|') {
    if(direction %in% c('N','S')) return(direction) else return(c('N','S'))
  } else if(mirror == '-'){
    if(direction %in% c('E','W')) return(direction) else return(c('E','W'))
  } else if(mirror == '\\'){
    redirect_list <- list(N = 'W', S = 'E', E = 'S', W = 'N')
    return(redirect_list[[direction]])
  } else if(mirror == '/'){
    redirect_list <- list(N = 'E', S = 'W', E = 'N', W = 'S')
    return(redirect_list[[direction]])
  } else {
    stop("Mirror shape not recognized")
  }
}

# Create a Beam object
Beam <- R6::R6Class('Beam', public = list(
  x = NULL, y = NULL, direction = NULL,
  initialize = function(x, y, direction){
    self$x <- x
    self$y <- y
    self$direction <- direction
    return(self) 
  },
  # Take a step
  directional_step = function(){
    if(self$direction == 'N') self$y <- self$y - 1
    if(self$direction == 'S') self$y <- self$y + 1
    if(self$direction == 'E') self$x <- self$x + 1
    if(self$direction == 'W') self$x <- self$x - 1
  })
)

# Function to step a beam
# May return 0 to 2 beams
step_beam <- function(beam){
  # Step to next position
  beam$directional_step()
  # If beam is now off the grid, return no beams
  if((beam$x < 1) | (beam$y < 1) | (beam$x > nrow(mirrors_matrix)) | (beam$y > nrow(mirrors_matrix))){
    return(list())
  }
  # If the beam matches a previous beam in both location and direction, return no beams
  if(matches_previous_direction(beam)){
    return(list())
  } else {
    store_direction(beam)
  }
  # Energize new position
  energy_matrix[beam$y, beam$x] <<- 1
  # Handle the mirror
  mirror <- mirrors_matrix[beam$y, beam$x]
  new_directions <- convert_direction(beam$direction, mirror)
  return(lapply(new_directions, function(dir) Beam$new(beam$x, beam$y, dir)))
}

# Reset energy and directions matrices
directions_matrix <- matrix('', nrow = nrow(input_matrix), ncol = ncol(input_matrix))
energy_matrix <- matrix(0, nrow = nrow(input_matrix), ncol = ncol(input_matrix))
# Interatively step beams until there are no beams left
beams_list <- list(Beam$new(x = 0, y = 1, direction = 'E'))
iterations <- 0
max_iterations <- prod(dim(mirrors_matrix))
while((length(beams_list) > 0) & (iterations < max_iterations)){
  beams_list <- sapply(beams_list, step_beam) |> unlist() |> Filter(f = Negate(is.null))
  iterations <- iterations + 1
  if(iterations %% 100 == 0) message(iterations, '. ', appendLF = F)
}
# Part 1 answer
message(sum(energy_matrix))

energy_list <- list(
  N = rep(0, nrow(mirrors_matrix)),
  s = rep(0, nrow(mirrors_matrix)),
  E = rep(0, nrow(mirrors_matrix)),
  W = rep(0, nrow(mirrors_matrix))
)

mm_rows <- nrow(mirrors_matrix)
for(starting_dir in c('N','S','E','W')){
  for(starting_row_col in seq_len(mm_rows)){
    message('.', appendLF = F)
    directions_matrix <- matrix('', nrow = mm_rows, ncol = mm_rows)
    energy_matrix <- matrix(0, nrow = mm_rows, ncol = mm_rows)
    # Starting position depends on starting direction
    if(starting_dir == 'N'){
      beams_list <- list(Beam$new(x = starting_row_col, y = mm_rows + 1, direction = 'N'))
    } else if(starting_dir == 'S'){
      beams_list <- list(Beam$new(x = starting_row_col, y = 0, direction = 'S'))
    } else if(starting_dir == 'E'){
      beams_list <- list(Beam$new(x = 0, y = starting_row_col, direction = 'E'))
    } else {
      beams_list <- list(Beam$new(x = mm_rows + 1, y = starting_row_col, direction = 'W'))
    }
    iterations <- 0
    max_iterations <- prod(dim(mirrors_matrix))
    while((length(beams_list) > 0) & (iterations < max_iterations)){
      beams_list <- sapply(beams_list, step_beam) |> unlist() |> Filter(f = Negate(is.null))
      iterations <- iterations + 1
    }
    # Set energy for this direction and starting position
    energy_list[[starting_dir]][starting_row_col] <- sum(energy_matrix)
  }
  message("Finished ", starting_dir)
}

# Part 2 solution
energy_list |> unlist() |> max() |> message()
