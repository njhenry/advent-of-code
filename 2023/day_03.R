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
library(httr); library(data.table); library(purrr)

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

input <- load_input_as_text(day = 3)

n_cols <- nchar(input[1])
n_rows <- length(input)

# Get all possible symbols
symbols <- strsplit(input, '') |> unlist() |> unique() |> setdiff(c(as.character(0:9), '.'))

# Find each instance of numbers in the input
matches_list <- gregexpr(pattern = '\\d+', text = input)

results_list <- list()
for(row_idx in seq_len(n_rows)){
  n_matches <- length(matches_list[[row_idx]])
  for(match_idx in seq_len(n_matches)){
    col_start <- matches_list[[row_idx]][match_idx]
    match_n_chars <- attr(matches_list[[row_idx]], 'match.length')[match_idx]
    col_end <- col_start + match_n_chars - 1
    num <- substr(input[row_idx], col_start, col_end) |> as.integer()
    check_indices <- data.table::data.table(
      ii = c(
        rep(row_idx - 1, match_n_chars + 2),
        rep(row_idx, 2),
        rep(row_idx + 1, match_n_chars + 2)
      ),
      jj = c(
        seq(col_start - 1, col_end + 1),
        col_start - 1, col_end + 1,
        seq(col_start - 1, col_end + 1)
      )
    )[(ii > 0) & (ii <= n_rows) & (jj > 0) & (jj <= n_cols), ]
    for(ij in seq_len(nrow(check_indices))){
      check_row <- check_indices[ij, ii]
      check_col <- check_indices[ij, jj]
      char <- substr(input[check_row], check_col, check_col)
      if(char %in% symbols){
        results_list <- c(
          results_list,
          list(data.table(char = char, count = num, row = check_row, col = check_col))
        )
      }
    }
  }
}

results_table <- rbindlist(results_list)

# Answer to part 1
message(results_table[, sum(count)])

# Answer to part 2
gears <- results_table[char == '*', ]
gear_ratios <- (gears
  [, .(times = .N, ratio = prod(count)), by = .(row, col)]
  [ times == 2, ]
)
message(gear_ratios[, sum(ratio)])
