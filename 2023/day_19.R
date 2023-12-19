## #######################################################################################
##
## ADVENT OF CODE DAY 19
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-06 (ET)
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

input <- load_input_as_text(day = 19) |> split_on_val("")

vals <- strsplit(input[[2]], ',') |>
  lapply(function(char) gsub(pattern = '(\\{x=|m=|a=|s=)', replacement = '', x = char)) |>
  lapply(function(char) gsub(pattern = '}', replacement = '', x = char)) |>
  unlist() |>
  as.double() |>
  matrix(ncol = 4, byrow = TRUE) |>
  data.table::as.data.table()
colnames(vals) <- c('x','m','a','s')


parse_one_rule <- function(char){
  has_lt <- grepl('<', char)
  has_gt <- grepl('>', char)
  if(!has_lt & !has_gt){
    return(function(x) char)
  } else {
    colon <- regexpr(':', char)
    test_col <- substr(char, 1, 1)
    compare_val <- as.integer(substr(char, 3, colon - 1))
    result <- substr(char, colon + 1, nchar(char))
    if(has_lt){
      return(function(x) if(x[[test_col]] < compare_val) result else 'NEXT')
    } else {
      return(function(x) if(x[[test_col]] > compare_val) result else 'NEXT')
    }
  }
}

parse_all_rules <- function(char){
  functions_list <- strsplit(char, ',')[[1]] |> lapply(parse_one_rule)
  full_function <- function(x){
    out_val <- 'NEXT'
    ii <- 1
    while(out_val == 'NEXT'){
      out_val <- functions_list[[ii]](x)
      ii <- ii + 1
    }
    return(out_val)
  }
  return(full_function)
}

rules_list <- strsplit(input[[1]], '\\{') |>
  lapply(function(x) gsub('\\}', '', x[2])) |>
  lapply(parse_all_rules)
rule_names <- strsplit(input[[1]], '\\{') |> lapply(function(x) x[1]) |> unlist()
names(rules_list) <- rule_names

num_not_finished <- nrow(vals)
vals[, status := 'in']
to_run <- vals[, .I]
while(length(to_run) > 0){
  for(row_i in to_run){
    vals$status[row_i] <- rules_list[[vals$status[row_i]]](as.list(vals[row_i,]))
  }
  to_run <- which((vals$status != 'A') & (vals$status != 'R'))
}

# Part 1 answer
message(vals[status == 'A', .(x, m, a, s)] |> as.matrix() |> sum())


## Part 2 ------------------------------------------------------------------------------->

# Traverse the maps in a different way
subset_on_condition <- function(row, col, val, gt, include){
  min_col <- paste0(col, 'min')
  max_col <- paste0(col, 'max')
  if(gt){
    if(!include) val <- val + 1
    row[[min_col]] <- val
  } else {
    if(!include) val <- val - 1
    row[[max_col]] <- val
  }
  if(row[[min_col]] > row[[max_col]]){
    return(NULL)
  } else {
    return(row)
  }
}

split_row_on_condition <- function(row, col, val, gt){
  meets_condition <- subset_on_condition(row, col, val, gt = gt, include = FALSE)
  negates_condition <- subset_on_condition(row, col, val, gt = !gt, include = TRUE)
  return(list(meets = meets_condition, negates = negates_condition))
}

# Parse a single rule; returns a data.table with 0+ rows
apply_single_rule <- function(in_range, test_text){
  has_lt <- grepl('<', test_text)
  has_gt <- grepl('>', test_text)
  if(!has_lt & !has_gt){
    return(list(meets = in_range[, NEXT := test_text]))
  } else {
    colon <- regexpr(':', test_text)
    test_col <- substr(test_text, 1, 1)
    compare_val <- as.integer(substr(test_text, 3, colon - 1))
    result <- substr(test_text, colon + 1, nchar(test_text))
    split_vals <- split_row_on_condition(in_range, test_col, compare_val, has_gt)
    if(!is.null(split_vals$meets)) split_vals$meets$NEXT <- result
    return(split_vals)
  }
}

apply_rule_set <- function(in_range, test_text){
  # Split up test text into component rules
  results_table <- data.table()
  in_list <- list(negates = in_range)
  rules_vec <- strsplit(test_text, split = ',')[[1]]
  ii <- 0
  while(!is.null(in_list$negates)){
    ii <- ii + 1
    if(ii > length(rules_vec)) stop("Issue with tests: ", test_text)
    in_list <- apply_single_rule(in_list$negates, rules_vec[ii])
    results_table <- rbind(results_table, in_list$meets)
  }
  return(results_table)
}

input_p2 <- load_input_as_text(day = 19) |> split_on_val("")
rules_text_p2 <- strsplit(input_p2[[1]], split = '\\{') |>
  lapply(function(x) gsub('\\}', '', x[2]))
names(rules_text_p2) <- rule_names

ongoing_ranges <- data.table::data.table(
  xmin = 1, xmax = 4000, mmin = 1, mmax = 4000, amin = 1, amax = 4000, smin = 1,
  smax = 4000, NEXT = 'in'
)
accepted_ranges <- data.table()

# Continue splitting until we find all accepted ranges
n_iterations <- 0
while(nrow(ongoing_ranges) >= 1){
  n_iterations <- n_iterations + 1
  if(n_iterations %% 100 == 0) message(n_iterations, ".", appendLF = F)
  ongoing_ranges <- lapply(seq_len(nrow(ongoing_ranges)), function(row_idx){
    function_name <- ongoing_ranges$NEXT[row_idx]
    apply_rule_set(ongoing_ranges[row_idx, ], rules_text_p2[[function_name]])
  }) |> rbindlist()
  accepted_ranges <- rbind(accepted_ranges, ongoing_ranges[NEXT == 'A', ])
  ongoing_ranges <- ongoing_ranges[(NEXT != 'A') & (NEXT != 'R'), ]
}

# Calculate total volume of each 4-D cube
options(scipen = 999)
accepted_ranges[, area4d := (xmax-xmin+1)*(mmax-mmin+1)*(amax-amin+1)*(smax-smin+1)]
accepted_ranges[, idx := .I ]

# Check overlap in every 4d area
overlap_area <- 0
overlap_rows <- integer(0)
for(row_ii in seq_len(nrow(accepted_ranges))){
  bl <- as.list(accepted_ranges[idx == row_ii, ])
  comparisons <- accepted_ranges[idx > row_ii, ]
  ol <- (
    copy(accepted_ranges)
    [, `:=` (
        xmin = pmax(xmin, bl$xmax), xmax = pmin(xmax, bl$xmin),
        mmin = pmax(mmin, bl$mmax), mmax = pmin(mmax, bl$mmin),
        amin = pmax(amin, bl$amax), amax = pmin(amax, bl$amin),
        smin = pmax(smin, bl$smax), smax = pmin(smax, bl$smin)
      )
    ]
    [(xmax >= xmin) & (mmax >= mmin) & (amax >= amin) & (smax >= smin), ]
    [, area4d := (xmax-xmin+1)*(mmax-mmin+1)*(amax-amin+1)*(smax-smin+1)]
  )
  if(nrow(ol) > 0){
    overlap_rows <- c(overlap_rows, row_ii)
    overlap_area <- overlap_area + ol[, sum(area4d)]
  }
}

# P2 answer
message(accepted_ranges[, sum(area4d)] - overlap_area)
