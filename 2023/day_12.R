## #######################################################################################
##
## ADVENT OF CODE DAY 12
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-12 (ET)
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

input_list <- load_input_as_text(day = 12)
cleaned_list <- input_list |>
  strsplit(split = " ") |> 
  lapply(function(x){
    segments <- strsplit(x[1], split = '\\.')[[1]]
    segments <- segments[segments != '']
    counts <- strsplit(x[2], split = ',')[[1]] |> as.integer()
    return(list(segments = segments, counts = counts))
})

iterations <- 0

# Get the 'remainder' of a match -- which characters are left for future matches
get_match_remainder <- function(count, segment){
  if(nchar(segment) > count + 1){
    return(substr(segment, count + 2, nchar(segment)))
  } else {
    return(character(0))
  }
}

# Truncate segments by cutting off the first value
truncate_segments <- function(segments){
  if(substr(segments[1], 1, 1) == '#') return(character(0))
  segments[1] <- gsub('^.', '', segments[1])
  return(segments[segments != ''])
}

get_next_match <- function(count, segments, last){
  iterations <<- iterations + 1
  found_match <- FALSE
  while(sum(found_match) < 1){
    if(length(segments) == 0) return(list(match = FALSE))
    found_match <- nchar(segments[1]) >= count
    if(!found_match & grepl('#', segments[1])) return(list(match = FALSE))
    if(!found_match) segments <- segments[-1]
  }
  # Check potential match
  remainder <- c(get_match_remainder(count, segments[1]), segments[-1])
  next_possible <- truncate_segments(segments)

  # This CANNOT be a match if:
  # - There is a pound immediately after the match
  # - This is the last match and there are pounds in subsequent matches
  pound_after_match <- grepl('#', substr(segments[1], count + 1, count + 1))
  pound_in_remainder <- any(grepl('#', remainder))
  impossible_match <- pound_after_match | (last & pound_in_remainder)
  if(impossible_match){
    # Check later in the string
    return(get_next_match(count, next_possible, last = last))
  } else {
    # Found a valid match!
    return(list(match = TRUE, remainder = remainder, next_test = next_possible))
  }
}

# Memoization hack
CACHE <- list()
empty_cache <- function() CACHE <<- list()
construct_key <- function(...){
  key_list <- lapply(list(...), function(x) paste(sapply(x, as.character), collapse="+"))
  key <- seq_along(key_list) |> 
    sapply(function(ii) paste0(names(key_list)[ii], ':', key_list[[ii]])) |>
    paste(collapse = ';')
  return(key)
}
cache_function <- function(fun, ...){
  key <- construct_key(...)
  if(key %in% names(CACHE)){
    result <- CACHE[[key]]
  } else {
    result <- fun(...)
    CACHE[[key]] <<- result
  }
  return(result)
}


# Get total number of matches for each line
process_line <- function(counts, segments){
  n_matches <- 0
  last <- (length(counts) == 1)
  next_match <- get_next_match(counts[1], segments, last = last)
  while(next_match$match & (length(segments) > 0)){
    remainder <- next_match$remainder
    segments <- next_match$next_test
    if(last){
      n_matches <- n_matches + 1
    } else {
      n_matches <- n_matches + 1 * cache_function(
        process_line, counts = counts[-1], segments = remainder
      )
    }
    next_match <- get_next_match(counts[1], segments, last = last)
  }
  return(n_matches)
}

# Part 1 answer
p1_combos <- lapply(cleaned_list, function(line){
  empty_cache()
  process_line(line$counts, line$segments)
})
message(p1_combos |> unlist() |> sum())

# Part 2 answer?
cleaned_list_p2 <- lapply(seq_along(input_list), function(ii){
  counts <- rep(cleaned_list[[ii]]$counts, times = 5)
  segments <- rep(strsplit(input_list[ii], split=' ')[[1]][1], times = 5) |> 
    paste(collapse='?') |>
    strsplit(split='\\.')
  segments <- segments[[1]]
  segments <- segments[segments != '']
  return(list(counts = counts, segments = segments))
})

p2_combos <- lapply(cleaned_list_p2, function(line){
  message('.', appendLF = F)
  empty_cache()
  process_line(counts = line$counts, segments = line$segments)
})
message(p2_combos |> unlist() |> sum())
