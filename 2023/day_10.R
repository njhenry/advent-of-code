## #######################################################################################
##
## ADVENT OF CODE DAY 10
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-10 (ET)
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

input_list <- load_input_as_text(day = 10)

directions <- list(
  "|" = c('N','S'), "-" = c('E','W'),
  "L" = c('N','E'), "J" = c("N", "W"),
  "7" = c('S','W'), "F" = c("S", "E")
)

opposite <- function(start_dir){
  if(start_dir == 'N') return('S')
  if(start_dir == 'E') return('W')
  if(start_dir == 'W') return('E')
  if(start_dir == 'S') return('N')
  stop("Whoops, should be one of NEWS.")
}

get_next_step <- function(tile_position, direction){
  if(direction == 'N') return(tile_position + c(0, -1))
  if(direction == 'E') return(tile_position + c(1, 0))
  if(direction == 'W') return(tile_position + c(-1, 0))
  if(direction == 'S') return(tile_position + c(0, 1))
}

get_new_direction <- function(start_dir, tile){
  if(!tile %in% names(directions)) stop("Invalid tile value: ", tile)
  return(setdiff(directions[[tile]], opposite(start_dir)))
}

## Part 1 answer
num_steps <- 0
pipes <- strsplit(input_list, split = '')
y_position <- which(grepl('S', input_list))
x_position <- regexpr('S', input_list[start_y])
tile_position <- c(x_position, y_position)
tile_val <- 'test'
move_direction <- 'E'

all_x_vals <- rep(NA_integer_, 13676)
all_y_vals <- rep(NA_integer_, 13676)

while(tile_val != 'S'){
  num_steps <- num_steps + 1
  all_x_vals[num_steps] <- tile_position[1]
  all_y_vals[num_steps] <- tile_position[2]
  tile_position <- get_next_step(tile_position, move_direction)
  tile_val <- pipes[[tile_position[2]]][tile_position[1]]
  if(tile_val != 'S'){
    move_direction <- get_new_direction(move_direction, tile_val)
  }
}
message(num_steps / 2)


## Part 2: lol let's use GIS
xy_table <- data.table::data.table(
  x = c(all_x_vals, all_x_vals[1]), # Close the loop using the first point
  y = c(all_y_vals, all_y_vals[1]), 
  skip = 1
)

candidate_points <- data.table::CJ(
  x = seq_len(length(pipes[[1]])), y = seq_len(length(pipes))
)[xy_table, skip := i.skip, on = c('x','y')][is.na(skip), ][, skip := NULL]
xy_table[, skip := NULL]

pipes_poly <- sf::st_polygon(list(as.matrix(xy_table))) |> sf::st_sfc() |> sf::st_sf()
pipes_poly$intersection <- 1
candidate_points <- candidate_points |>
  sf::st_as_sf(coords = c('x','y'))

tt <- sf::st_join(x = candidate_points, y = pipes_poly)
num_inside <- sum(!is.na(tt$intersection))

message(num_inside)
