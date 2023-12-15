## #######################################################################################
##
## ADVENT OF CODE DAY 15
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-15 (ET)
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

input <- load_input_as_text(day = 15) |> strsplit(split = ',') |> first()

HASH_fun <- function(text_string){
  ascii_values <- as.integer(charToRaw(text_string))
  score <- 0
  for(av in ascii_values){
    score <- ((score + av) * 17) %% 256
  }
  return(score)
}

# p1 answer
scores <- sapply(input, HASH_fun)
message(sum(scores))

# p2 instructions: HASH map
lens_boxes <- lapply(1:256, vector, mode = 'list')
for(instructions in input){
  if(endsWith(instructions, '-')){
    lens_name <- gsub('.$', '', instructions)
    box_number <- HASH_fun(lens_name)
    lens_boxes[[box_number + 1]][[lens_name]] <- NULL
  } else if(grepl('=', instructions)){
    parsed <- strsplit(instructions, split = '=')[[1]]
    lens_name <- parsed[1]
    lens_val <- as.integer(parsed[2])
    box_number <- HASH_fun(lens_name)
    lens_boxes[[box_number + 1]][[lens_name]] <- lens_val
  } else {
    stop("Instructions ", instructions, " did not match expected pattern.")
  }
}

# Drop any empty spots (lenses that had been replaced) from the list
lens_boxes_cleaned <- lapply(lens_boxes, function(box) Filter(Negate(is.null), box))

p2_score <- 0
for(box_i in seq_along(lens_boxes_cleaned)){
  for(lens_i in seq_along(lens_boxes_cleaned[[box_i]])){
    p2_score <- p2_score + (box_i * lens_i * lens_boxes_cleaned[[box_i]][[lens_i]])
  }
}

