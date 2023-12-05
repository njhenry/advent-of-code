## #######################################################################################
##
## ADVENT OF CODE DAY THREE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-04 (ET)
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

input <- load_input_as_text(day = 5)

seeds <- gsub("seeds: ", "", input[1]) |> strsplit(" ") |> unlist() |> as.double()
maps_text <- input[3:length(input)]


maps_idx <- 0
maps_list <- list(data.table())
for(row in seq_along(maps_text)){
  if(maps_text[row] == '') next
  if(endsWith(maps_text[row], 'map:')){
    maps_idx <- maps_idx + 1
    maps_list[[maps_idx]] <- data.table()
  } else {
    vals <- strsplit(maps_text[row], ' ')[[1]] |> as.double()
    maps_list[[maps_idx]] <- rbind(
      maps_list[[maps_idx]],
      data.table(xmin = vals[2], xmax = vals[2] + vals[3] - 1, diff = vals[1] - vals[2])
    )
  }
}
maps_list <- lapply(maps_list, function(x) x[order(xmin)])

locations <- sapply(seeds, function(seed){
  for(map in maps_list){
    i <- 1
    matched <- FALSE
    while((i <= nrow(map)) & (matched == FALSE)){
      if((seed >= map[i, xmin]) & (seed <= map[i, xmax])){
        seed <- seed + map[i, diff]
        matched <- TRUE
      }
      i <- i + 1
    }
  }
  return(seed)
})

# Part 1 answer
message(min(locations))

# Part 2
seed_ranges <- lapply(
  seq_len(length(seeds) / 2),
  function(ii) c(seeds[2*ii-1], seeds[2*ii - 1] + seeds[2*ii])
)



apply_map <- function(range, map){
  if(range[2] < min(map$xmin)) return(list(range))
  if(range[1] > max(map$xmax)) return(list(range))
  out_ranges <- list()
  out_ii <- 1
  # Handle any parts of the range below the first xmin
  if(range[1] < min(map$xmin)){
    out_ranges[[out_ii]] <- c(range[1], min(map$xmin) - 1)
    out_ii <- out_ii + 1
    range[1] <- min(map$xmin)
  }
  # Handle any ranges intersecting sections of the map
  for(row_i in seq_len(nrow(map))){
    if(range[1] <= map[row_i, xmax]){
      if(range[2] <= map[row_i, xmax]){
        out_ranges[[out_ii]] <- range + map[row_i, diff]
        return(out_ranges)
      } else {
        out_ranges[[out_ii]] <- c(range[1], map[row_i, xmax]) + map[row_i, diff]
        out_ii <- out_ii + 1
        range[1] <- map[row_i, xmax + 1]
      }
    }
  }
  # Handle any parts of the range above the last xmax
  if(range[2] > max(map$xmax)){
    out_ranges[[out_ii]] <- c(max(map$xmax), range[2])
  }
  return(out_ranges)
}

in_ranges <- seed_ranges
for(map in maps_list){
  out_ranges <- list()
  for(range_ii in seq_along(in_ranges)){
    out_ranges <- c(
      out_ranges,
      apply_map(in_ranges[[range_ii]], map)
    )
  }
  in_ranges <- out_ranges
}
