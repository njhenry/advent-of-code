## #######################################################################################
##
## ADVENT OF CODE DAY 11
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-10 (ET)
##
## To be executed in R v4.1.0+
##
## #######################################################################################

## SETUP FOR THE DAY
library(httr); library(data.table); library(stringr); library(sf)

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

input_list <- load_input_as_text(day = 11)

# Expand the universe

# Expand by rows
empty_row_val <- paste(rep('.', 140), collapse='')
empty_rows <- which(input_list == empty_row_val)
# Expand by columns
empty_cols <- strsplit(unlist(input_list), split = '') |>
  lapply(function(row_vec) which(row_vec == '.')) |>
  Reduce(f = intersect)

n_rows <- length(input_list)
n_cols <- input_list[1] |> nchar() |> unname()

num_galaxies <- 0
galaxy_positions_list <- list()

for(row in seq_len(n_rows)){
  for(col in seq_len(n_cols)){
    if(substr(input_list[row], col, col) == '#'){
      num_galaxies <- num_galaxies + 1
      galaxy_positions_list[[num_galaxies]] <- data.table::data.table(x = col, y = row)
    }
  }
}

galaxy_positions <- data.table::rbindlist(galaxy_positions_list)

galaxy_positions[, galaxy_id := .I ][, merge_on := 1 ]
all_paths <- merge(
  x = galaxy_positions, y = galaxy_positions, by = 'merge_on',
  allow.cartesian = T, suffixes = c('1','2')
)[galaxy_id1 < galaxy_id2, ]
all_paths[, n_moves := (abs(x2 - x1) + abs(y2 - y1))]

# Part 1 answer: add 1 for every empty row or column crossed
all_paths_p1 <- copy(all_paths)
for(er in empty_rows){
  all_paths_p1[((y1 < er) & (y2 > er)) | ((y1 > er) & (y2 < er)), n_moves := n_moves + 1]
}
for(ec in empty_cols){
  all_paths_p1[((x1 < ec) & (x2 > ec)) | ((x1 > ec) & (x2 < ec)), n_moves := n_moves + 1]
}
message(all_paths_p1[, sum(n_moves)])

# Part 2 answer: add 1000 for every empty row or column crossed
all_paths_p2 <- copy(all_paths)
for(er in empty_rows){
  all_paths_p2[((y1 < er) & (y2 > er)) | ((y1 > er) & (y2 < er)), n_moves := n_moves + 1e6 - 1]
}
for(ec in empty_cols){
  all_paths_p2[((x1 < ec) & (x2 > ec)) | ((x1 > ec) & (x2 < ec)), n_moves := n_moves + 1e6 - 1]
}
message(all_paths_p2[, sum(n_moves)])
