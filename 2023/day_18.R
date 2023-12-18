## #######################################################################################
##
## ADVENT OF CODE DAY 18 (take 2)
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
input <- load_input_as_text(day = 18) |> strsplit(split = " ") |>
  lapply(function(x) data.table::data.table(direction = x[[1]], count = as.integer(x[[2]]))) |>
  data.table::rbindlist()

# Store current position
pos <- list(x = 0, y = 0)

perimeter_list <- vector('list', length = nrow(input))

# Get the perimeter coordinate centers
for(ii in seq_len(nrow(input))){
  dir <- input[ii, direction]
  ct <- input[ii, count]
  if(dir == 'L'){
    pos$x <- pos$x - ct
  } else if(dir == 'R'){
    pos$x <- pos$x + ct
  } else if(dir == 'U'){
    pos$y <- pos$y + ct
  } else {
    pos$y <- pos$y - ct
  }
  perimeter_list[[ii]] <- data.table(x = pos$x, y = pos$y)
}

perimeter_table <- data.table::rbindlist(
  c(list(data.table(x = 0, y = 0)), perimeter_list)
)

# Convert to spatial object
perimeter_sf <- perimeter_table |> as.matrix() |> list() |> 
  sf::st_polygon() |> sf::st_sfc() |> sf::st_sf()

# Formula for total area (including missing edges)
p1_answer <- (
  st_area(perimeter_sf) + # Interior of the polygon (ignoring ~1/2 the area of the perimeter)
  st_length(st_cast(perimeter_sf,"MULTILINESTRING")) / 2 + # Most squares have a missing area of 1/2 m**3
  1 # Four extra corner "boxes" with area 0.25 m^3
)
message(p1_answer)


# Part 2
input_p2 <- load_input_as_text(day = 18) |> strsplit(split = " ") |>
  lapply(function(x) data.table::data.table(hex = x[[3]])) |>
  data.table::rbindlist()
(input_p2
  [, ct := strtoi(substr(hex, 3, 7), base = 16)]
  [, dir_idx := as.integer(substr(hex, 8, 8))]
  [dir_idx == 0, dir := "R"]
  [dir_idx == 1, dir := "D"]
  [dir_idx == 2, dir := "L"]
  [dir_idx == 3, dir := "U"]
)
pos <- list(x = 0, y = 0)
p_list_p2 <- vector('list', length = nrow(input))
for(ii in seq_len(nrow(input))){
  dir <- input_p2[ii, dir]
  ct <- input_p2[ii, ct]
  if(dir == 'R'){
    pos$x <- pos$x + ct
  } else if(dir == 'L') {
    pos$x <- pos$x - ct
  } else if(dir == 'U') {
    pos$y <- pos$y + ct
  } else {
    pos$y <- pos$y - ct
  }
  p_list_p2[[ii]] <- data.table(x = pos$x, y = pos$y)
}

p_table_p2 <- data.table::rbindlist(
  c(list(data.table(x = 0, y = 0)), p_list_p2)
)

perimeter_sf_p2 <- p_table_p2 |> as.matrix() |> list() |> 
  sf::st_polygon() |> sf::st_sfc() |> sf::st_sf()

p2_answer <- (
  st_area(perimeter_sf_p2) +
  st_length(st_cast(perimeter_sf_p2,'MULTILINESTRING')) / 2 + 1
)
options(scipen = 999) # Turn off scientific notation for this answer
message(p2_answer)
