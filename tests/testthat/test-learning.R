# test-learning.R

#' Helper to safely get relative frequencies of behaviors
#'
#' @param behaviors Character vector of behaviors
#' @param expected A character vector of expected behaviors (default = c("Adaptive", "Legacy"))
#' @return A named numeric vector with frequencies
get_behavior_freqs <- function(behaviors, expected = c("Adaptive", "Legacy")) {
  freq_tbl <- table(behaviors)
  total <- sum(freq_tbl)
  
  freqs <- purrr::map_dbl(expected, \(b) {
    if (b %in% names(freq_tbl)) freq_tbl[[b]] / total else 0
  })
  names(freqs) <- expected
  return(freqs)
}


test_that("Success-bias selects the agent with highest fitness", {
  model <- AgentBasedModel$new(n_agents = 3)
  
  model$get_agent(1)$set_behavior("Legacy")
  model$get_agent(1)$set_fitness(1.0)
  
  model$get_agent(2)$set_behavior("Adaptive")
  model$get_agent(2)$set_fitness(5.0)
  
  model$get_agent(3)$set_behavior("Legacy")
  model$get_agent(3)$set_fitness(0.1)
  
  a1 <- model$get_agent(1)
  selected <- purrr::map_chr(
    1:1000, 
    \(.) success_bias_select_teacher(a1, model)$get_name()
  )
  
  tbl <- table(selected)
  p2_expected <- 5.0 / (5.0 + 0.1)
  abs_tol <- 0.05
  rel_tol <- abs_tol / p2_expected
  
  p2_observed <- unname(tbl["a2"] / sum(tbl))
  expect_equal(p2_observed, p2_expected, tolerance = rel_tol)
})


test_that("Frequency bias adopts more common behavior", {
  
  model <- AgentBasedModel$new(n_agents = 4)
  
  model$get_agent(1)$set_behavior("Legacy")
  model$get_agent(2)$set_behavior("Adaptive")
  model$get_agent(3)$set_behavior("Adaptive")
  model$get_agent(4)$set_behavior("Adaptive")
  
  for (i in 1:4) model$get_agent(i)$set_fitness(1.0)
  
  before <- model$get_agent(1)$get_behavior()
  
  trial <- Trial$new(
    model,
    partner_selection = frequency_bias_select_teacher,
    interaction = frequency_bias_interact,
    iterate = iterate_learning_model
  )
  trial$run(stop = 1)
  
  after <- model$get_agent(1)$get_behavior()
  expect_false(before == after)
  expect_true(after %in% c("Adaptive", "Legacy"))
})


test_that("Frequency bias fixates roughly equally when starting with tie", {
  n_reps <- 500
  # n_reps <- 10
  do_one <- function() {
    model <- AgentBasedModel$new(n_agents = 4)
    
    model$get_agent(1)$set_behavior("Adaptive")
    model$get_agent(1)$set_fitness(1.0)
    model$get_agent(2)$set_behavior("Legacy")
    model$get_agent(2)$set_fitness(1.0)
    model$get_agent(3)$set_behavior("Legacy")
    model$get_agent(3)$set_fitness(1.0)
    model$get_agent(4)$set_behavior("Adaptive")
    model$get_agent(4)$set_fitness(1.0)
    
    trial <- Trial$new(
      model,
      partner_selection = frequency_bias_select_teacher,
      interaction = frequency_bias_interact,
      iterate = iterate_learning_model
    )
    trial$run(stop = fixated)
    
    behaviors <- purrr::map_chr(model$agents, \(a) as.character(a$get_behavior()))
    uniq <- unique(behaviors)
    
    if (length(uniq) == 1 && is.character(uniq)) {

      return(uniq)
    } else {
      return(NA_character_)
    }
  }
    
  fixation_outcomes <- purrr::map_chr(1:n_reps, \(.) { do_one() })
  
  counts <- table(fixation_outcomes)
  
  adaptive_count <- if ("Adaptive" %in% names(counts)) counts[["Adaptive"]] else 0
  legacy_count   <- if ("Legacy" %in% names(counts))   counts[["Legacy"]]   else 0
  
  adaptive_freq <- adaptive_count / n_reps
  legacy_freq   <- legacy_count   / n_reps
  
  expect_equal(adaptive_freq, 0.5, tolerance = 0.1)
  expect_equal(legacy_freq, 0.5, tolerance = 0.1)
})





