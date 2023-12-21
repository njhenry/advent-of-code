## #######################################################################################
##
## ADVENT OF CODE DAY 20
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2023-12-20 (ET)
##
## To be executed in R v4.1.0+
##
## #######################################################################################

## SETUP FOR THE DAY
library(httr); library(R6)

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
      strsplit(split = '\\n') |>
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

Module <- R6::R6Class('Module', public = list(
  type = NULL,
  name = NULL,
  inputs = integer(0),
  targets = NULL,
  state = 0,

  parse_input_text = function(input_text){
    input_text <- gsub(' ', '', input_text)
    start_char <- substr(input_text, 1, 1)
    if(start_char %in% c('&', '%')){
      type <- start_char
      input_text <- gsub('^.', '', input_text)
    } else {
      type <- 'broadcaster'
    }
    name <- strsplit(input_text, split = '->')[[1]][1]
    targets <- strsplit(input_text, split = '->')[[1]][2] |>
      strsplit(split = ',') |>
      unlist()
    return(list(type = type, name = name, targets = targets))
  },

  add_input = function(input_name){
    old_input_names <- names(self$inputs)
    self$inputs <- c(self$inputs, 0)
    names(self$inputs) <- c(old_input_names, input_name)
    invisible(NULL)
  },

  initialize = function(input_text){
    parsed_text <- self$parse_input_text(input_text)
    self$type <- parsed_text$type
    self$name <- parsed_text$name
    self$targets <- parsed_text$targets
  },

  get_state = function(){
    if(self$type == '&'){
      return(paste0(self$inputs, collapse = ''))
    } else {
      return(self$state)
    }
  },

  receive_pulse = function(pulse_type, from){
    if(self$type == '&'){
      self$inputs[from] <- pulse_type
      return(self$send_pulse())
    } else if((self$type == '%') & (pulse_type == 0)){
      self$state <- as.integer(!self$state)
      return(self$send_pulse())
    } else if(self$type == 'broadcaster'){
      self$state <- pulse_type
      return(self$send_pulse())
    } else {
      return(NULL)
    }
  },

  send_pulse = function(){
    if(self$type == '&'){
      # Conjunction
      pulse_val <- if(all(self$inputs == 1)) 0 else 1
    } else if(self$type %in% c('%', 'broadcaster')){
      # Flip flop
      pulse_val <- as.integer(self$state)
    }
    pulse <- lapply(
      self$targets,
      function(target_name) list(source = self$name, target = target_name, value = pulse_val)
    )
    if(pulse_val == 0){
      LOW_COUNTER <<- LOW_COUNTER + length(pulse)
    } else {
      HIGH_COUNTER <<- HIGH_COUNTER + length(pulse)
    }
    return(pulse)
  }
))

System <- R6::R6Class("System", public = list(
  modules = list(),
  histories = character(0),

  initialize = function(input_text_vec){
    self$modules <- lapply(input_text_vec, function(input_line) Module$new(input_line))
    if(length(self$modules) == 0) stop("No modules initialized")
    names(self$modules) <- sapply(self$modules, function(module) module$name)
    # Set inputs for the conjunction modules
    for(module in self$modules){
      input_name <- module$name
      for(target_name in module$targets){
        if(target_name %in% names(self$modules)){
          self$modules[[target_name]]$add_input(input_name)
        } else {
          message("Target name ", target_name, " not in list - removing.")
        }
      }
    }
  },

  push_button = function(verbose = FALSE){
    # Low pulse comes from the button
    LOW_COUNTER <<- LOW_COUNTER + 1
    if(verbose) message('button -0-> broadcaster')
    out_pulses <- self$modules[['broadcaster']]$receive_pulse(0, from = 'button')
    # Iterate through all signals
    while(length(out_pulses) > 0){
      first_pulse <- out_pulses[[1]]
      out_pulses[[1]] <- NULL
      if(verbose == TRUE){
        message(first_pulse$source, ' -', first_pulse$value, '-> ', first_pulse$target)
      }
      target_name <- first_pulse$target
      if(target_name %in% names(self$modules)){
        last_pulse <- self$modules[[first_pulse$target]]$receive_pulse(
          pulse_type = first_pulse$value, from = first_pulse$source
        )
        if(length(last_pulse) > 0){
          out_pulses <- c(out_pulses, last_pulse)
        }
      }
      out_pulses <- Filter(Negate(is.null), out_pulses)
    }
    # Record the state of all modules
    module_state <- lapply(self$modules, function(module) module$get_state()) |>
      lapply(as.character) |>
      unlist() |>
      paste(collapse = '')
    return(module_state)
  },

  last_history_is_cycle = function(){
    if(length(self$histories) == 0){
      return(FALSE)
    } else {
      last_history <- data.table::last(self$histories)
      starting_val <- rep('0', nchar(last_history)) |> paste(collapse = '')
      return(last_history == starting_val)
    }
  },

  find_cycle = function(){
    while(!self$last_history_is_cycle()){
      newest_end_state <- self$push_button()
      self$histories <- c(self$histories, newest_end_state)
    }
    cycle_length <- length(self$histories)
    message("Cycle length is ", cycle_length, " button pushes long.")
    return(cycle_length)
  }
))

input_text <- load_input_as_text(day = 20)

# Part 1
p1_n_button_presses <- 1e3
HIGH_COUNTER <- 0
LOW_COUNTER <- 0

p1_system <- System$new(input_text)
for(ii in seq_len(p1_n_button_presses)) p1_system$push_button()
message(HIGH_COUNTER * LOW_COUNTER)

# Part 2
p2_system <- System$new(input_text)

# Find the conjunction switch that leads to rx
rx_input <- lapply(p2_system$modules, function(mod) 'rx' %in% mod$targets) |>
  unlist() |> which() |> names()
# Find the switches that need to be on for the conjunction switch to send a low signal
upstream_inputs_list <- p2_system$modules[[rx_input]]$inputs |> as.list() |> lapply(setdiff, 0)

all_cycles_found <- function(){
  lapply(upstream_inputs_list, function(ii) length(ii) >= 2) |> unlist() |> all()
}

# Find the cycle for each upstream input
p2_n_presses <- 0
while(!all_cycles_found()){
  p2_n_presses <- p2_n_presses + 1
  if(p2_n_presses %% 100 == 0) message(p2_n_presses, '.', appendLF = F)
  p2_system$push_button()
  upstream_signals <- p2_system$modules[[rx_input]]$inputs
  upstream_matches <- names(upstream_signals)[which(upstream_signals == 1)]
  if(length(upstream_matches) > 0){
    for(match_idx in upstream_matches){
      upstream_inputs_list[[match_idx]] <- c(upstream_inputs_list[[match_idx]], p2_n_presses)
    }
  }
}
