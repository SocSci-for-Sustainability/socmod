#' Helper function to calculate relative error from an absolute error and the 
#' known value used for comparison. Useful because for comparing small probability
#' values in the function below.
#' 
#' @param abs_tol Desired absolute tolerance
#' @param known_val Known value against which we calculate error of observed value
#' @examples
#' expected <- 0.17
#' calculated <- 0.15
#' abs_tol <- 0.05
#' rel_tol <- abs_to_rel_tol(abs_tol, expected)
abs_to_rel_tol <- function(abs_tol, abs_known_val) {
  return (abs_tol / abs_known_val)
}

#--------- Success-biased learning tests -------------#
test_that("Success-bias selects teachers as expected", {
  model <- AgentBasedModel$new(n_agents = 4)
  a1 <- model$get_agent(1)
  a2 <- model$get_agent(2)
  a3 <- model$get_agent(3)
  a4 <- model$get_agent(4)
  
  a2$set_fitness(0.0)
  a3$set_fitness(1.0)
  a4$set_fitness(0.0)
  expect_equal(success_bias_select_teacher(a1, model)$get_name(), "a3")
  
  a2$set_fitness(1.0)
  a3$set_fitness(2.0)
  a4$set_fitness(3.0)
  
  n_selections <- 10
  selected_idxs <- purrr::map_vec(1:n_selections, \(.) {
    success_bias_select_teacher(a1, model)$get_name()
  })
  
  tab <- table(selected_idxs)
  select_frequency <- as.vector(tab / n_selections)
  names(select_frequency) <- names(tab)
  
  abs_tol <- 0.05
  
  expect_equal(select_frequency[["a2"]], 1.0 / 6.0, tolerance = abs_to_rel_tol(abs_tol, 1.0 / 6.0))
  expect_equal(select_frequency[["a3"]], 1.0 / 3.0, tolerance = abs_to_rel_tol(abs_tol, 1.0 / 3.0))
  expect_equal(select_frequency[["a4"]], 1.0 / 2.0, tolerance = abs_to_rel_tol(abs_tol, 1.0 / 2.0))
})

test_that("Success-biased learning results in expected learned behaviors", {
  model <- AgentBasedModel$new(n_agents = 4)
  a1 <- model$get_agent(1)
  a2 <- model$get_agent(2)
  a3 <- model$get_agent(3)
  a4 <- model$get_agent(4)
  
  a1$set_fitness(1.0)
  a2$set_fitness(1.0)
  a3$set_fitness(1e9)
  a4$set_fitness(1.0)
  
  b3 <- "Behavior #3"
  a3$set_behavior(b3)
  
  out <- run(model,
             partner_selection = success_bias_select_teacher,
             interaction = success_bias_interact,
             iterate_m = iterate_learning_model)$output
  
  expect_equal(a1$get_behavior(), b3)
  expect_equal(nrow(out), 2)
  expect_equal(out$t, c(0, 1))
  
  do_one <- function() {
    model <- AgentBasedModel$new(n_agents = 4)
    a1 <- model$get_agent(1)
    a2 <- model$get_agent(2)
    a3 <- model$get_agent(3)
    a4 <- model$get_agent(4)
    
    a2$set_fitness(1.0)
    a3$set_fitness(2.0)
    a4$set_fitness(3.0)
    
    a2$set_behavior("B-2")
    a3$set_behavior("B-3")
    a4$set_behavior("B-4")
    
    run(model,
        partner_selection = success_bias_select_teacher,
        interaction = success_bias_interact,
        iterate_m = iterate_learning_model)
    
    return (a1$get_behavior())
  }
  
  # n_reps <- 2000
  n_reps <- 10
  a1_learned_behaviors <- purrr::map_vec(1:n_reps, \(.){ do_one() })
  
  tab <- table(a1_learned_behaviors)
  select_frequency <- as.vector(tab / n_reps)
  select_labels <- names(tab)
  
  abs_tol <- 0.05
  
  expect_equal(select_frequency[["B-2"]], 1.0 / 6.0, tolerance = abs_to_rel_tol(abs_tol, 1.0 / 6.0))
  expect_equal(select_frequency[["B-3"]], 1.0 / 3.0, tolerance = abs_to_rel_tol(abs_tol, 1.0 / 3.0))
  expect_equal(select_frequency[["B-4"]], 1.0 / 2.0, tolerance = abs_to_rel_tol(abs_tol, 1.0 / 2.0))
})

#--------- Frequency-biased learning tests -------------#
test_that("Frequency-biased learning leads to more common behaviors adopted more frequently", {
  do_one <- function() {
    model <- AgentBasedModel$new(n_agents = 4)
    a1 <- model$get_agent(1)
    a2 <- model$get_agent(2)
    a3 <- model$get_agent(3)
    a4 <- model$get_agent(4)
    
    a2$set_fitness(1.0)
    a3$set_fitness(2.0)
    a4$set_fitness(3.0)
    
    a1$set_behavior("Legacy")
    a2$set_behavior("Adaptive")
    a3$set_behavior("Adaptive")
    a4$set_behavior("Legacy")
    
    run(model,
        partner_selection = frequency_bias_select_teacher,
        interaction = frequency_bias_interact,
        iterate_m = iterate_learning_model)
    
    return (a1$get_behavior())
  }
  
  # n_reps <- 2000
  n_reps <- 10
  a1_learned_behaviors <- purrr::map_vec(1:n_reps, \(.){ do_one() })
  
  behavior_tbl <- count(tibble(curr_behavior = a1_learned_behaviors), curr_behavior)
  behavior_tbl$freq <- behavior_tbl$n / n_reps
  
  expect_equal(dplyr::filter(behavior_tbl, curr_behavior == "Legacy")$freq, 1.0 / 3.0, tolerance = 1e-1)
  expect_equal(dplyr::filter(behavior_tbl, curr_behavior == "Adaptive")$freq, 2.0 / 3.0, tolerance = 1e-1)
})
