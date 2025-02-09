#' Helper function to calculate relative error from an absolute error and the 
#' known value used for comparison. Useful because for comparing small probability
#' values in the function below.
#' 
#' @param abs_tol Desired absolute tolerance
#' @param known_val Known value against which we calculate error of observed value
#' @examples
#' # Comparing expected to calculated like this...
#' expected <- 0.17
#' calculated <- 0.15
#' expect_equal(calculated, expected, tolerance = 1e-1)
#' # ...fails because it is calculating relative tolerance. Let's make the 
#' # absolute tolerance 0.05, seems reasonable.
#' abs_tol <- 0.05
#' rel_tol <- abs_to_rel_tol(abs_tol, abs_known_val)
#'
abs_to_rel_tol <- function(abs_tol, abs_known_val) {
  return (abs_tol / abs_known_val)
}

#--------- Success-biased learning tests -------------#
test_that("Success-bias selects teachers as expected",
{
  # Initialize a model with fully-connected network (default).
  model <- AgentBasedModel$new(n_agents = 4)

  # Extract agents for convenience.
  a1 <- model$get_agent(1)
  a2 <- model$get_agent(2)
  a3 <- model$get_agent(3)
  a4 <- model$get_agent(4)

  # Set fitness for agents 2-4, with 3 having fitness 1, others 0.
  a2$set_fitness(0.0)
  a3$set_fitness(1.0)
  a4$set_fitness(0.0)

  # Then we expect that teacher selection will select agent 3.
  expect_equal(success_bias_select_teacher(a1, model)$name, 3)

  # Now set the fitnesses to be multiples of 1 so the probabilities of selection
  # are 1/6, 1/3, and 1/2; expect observe this fraction of 10000 selections.
  a2$set_fitness(1.0)
  a3$set_fitness(2.0)
  a4$set_fitness(3.0)

  n_selections <- 2000

  selected_idxs <-
    purrr::map_vec(1:n_selections,
                   \(.) { success_bias_select_teacher(a1, model)$name })

  select_frequency <- rle(sort(selected_idxs))$lengths / n_selections
   
  # Set absolute tolerance to be 0.05 for these tests.
  a1_freq_expected <- 1.0 / 6.0
  abs_tol <- 0.05
  rel_tol <- abs_to_rel_tol(abs_tol, a1_freq_expected)
  expect_equal(select_frequency[1], a1_freq_expected, tolerance = rel_tol)

  a2_freq_expected <- 1.0 / 3.0
  rel_tol <- abs_to_rel_tol(abs_tol, a2_freq_expected)
  expect_equal(select_frequency[2], a2_freq_expected, tolerance = rel_tol)

  a3_freq_expected <- 1.0 / 2.0
  rel_tol <- abs_to_rel_tol(abs_tol, a3_freq_expected)
  expect_equal(select_frequency[3], a3_freq_expected, tolerance = rel_tol)
})


test_that("Success-biased learning results in expected learned behaviors",
{
  # Initialize a default-networked model as above, but now testing behavior learned.
  model <- AgentBasedModel$new(n_agents = 4)

  # Extract agents for convenience.
  a1 <- model$get_agent(1)
  a2 <- model$get_agent(2)
  a3 <- model$get_agent(3)
  a4 <- model$get_agent(4)

  # Set fitness for agents 2-4, with 3 having fitness 1, others 0.
  a1$set_fitness(1.0)  # Need this so we don't have NA for others' learning.
  a2$set_fitness(1.0)
  a3$set_fitness(1e9)
  a4$set_fitness(1.0)

  # Check that a3 is the one that a1 learned from.
  b3 <- "Behavior #3"
  a3$curr_behavior <- b3

  out <- run(model,
             partner_selection = success_bias_select_teacher,
             interaction = success_bias_interact,
             iterate_m = iterate_learning_model)$output

  expect_equal(a1$curr_behavior, b3)
  expect_equal(nrow(out), 2)
  expect_equal(out$t, c(0, 1))
  expect_equal(out$t, c(0, 1))

  # Now set the fitnesses to be multiples of 1 so the probabilities of selection
  # are 1/6, 1/3, and 1/2; expect observe this fraction of 10000 selections.
  do_one <- function() {

    # Initialize a new model every time to only test first learning step.
    model <- AgentBasedModel$new(n_agents = 4)

    # Extract agents for convenience.
    a1 <- model$get_agent(1)
    a2 <- model$get_agent(2)
    a3 <- model$get_agent(3)
    a4 <- model$get_agent(4)

    a2$set_fitness(1.0)
    a3$set_fitness(2.0)
    a4$set_fitness(3.0)

    a2$curr_behavior <- "B-2"
    a3$curr_behavior <- "B-3"
    a4$curr_behavior <- "B-4"

    run(model,
        partner_selection = success_bias_select_teacher,
        interaction = success_bias_interact,
        iterate_m = iterate_learning_model)

    return (a1$curr_behavior)
  }

  n_reps <- 2000
  a1_learned_behaviors <- purrr::map_vec(1:n_reps, \(.){ do_one() })

  select_count <- rle(sort(a1_learned_behaviors))
  select_frequency <- select_count$lengths / n_reps
  select_labels <- select_count$values
  names(select_frequency) <- select_labels

  # Set absolute tolerance to be 0.01 for these tests.
  a1_freq_expected <- 1.0 / 6.0
  abs_tol <- 0.05
  rel_tol <- abs_to_rel_tol(abs_tol, a1_freq_expected)
  expect_equal(select_frequency[[select_labels[1]]], 
               a1_freq_expected, tolerance = rel_tol)

  a2_freq_expected <- 1.0 / 3.0
  rel_tol <- abs_to_rel_tol(abs_tol, a2_freq_expected)
  expect_equal(select_frequency[[select_labels[2]]], 
               a2_freq_expected, tolerance = rel_tol)

  a3_freq_expected <- 1.0 / 2.0
  rel_tol <- abs_to_rel_tol(abs_tol, a3_freq_expected)
  expect_equal(select_frequency[[select_labels[3]]], 
               a3_freq_expected, tolerance = rel_tol)
})


#--------- Frequency-biased learning tests -------------#
test_that(
"Frequency-biased learning leads to more common behaviors adopted more frequently", {

  do_one <- function() {
  
    # Initialize a new model every time to only test first learning step.
    model <- AgentBasedModel$new(n_agents = 4)
    
    # Extract agents for convenience.
    a1 <- model$get_agent(1)
    a2 <- model$get_agent(2)
    a3 <- model$get_agent(3)
    a4 <- model$get_agent(4)
    
    a2$set_fitness(1.0)
    a3$set_fitness(2.0)
    a4$set_fitness(3.0)
    
    a1$curr_behavior <- "Legacy"
    a2$curr_behavior <- "Adaptive"
    a3$curr_behavior <- "Adaptive"
    a4$curr_behavior <- "Legacy"
  
    run(model, 
      partner_selection = frequency_bias_select_teacher,
      interaction = frequency_bias_interact, 
      iterate_m = iterate_learning_model)
  
    return (a1$curr_behavior)
  }

  n_reps <- 2000
  a1_learned_behaviors <- purrr::map_vec(1:n_reps, \(.){ do_one() })

  behavior_tbl <- count(tibble(curr_behavior = a1_learned_behaviors), curr_behavior)
  behavior_tbl$freq <- behavior_tbl$n / n_reps

  # Now set the fitnesses to be multiples of 1 so the probabilities of selection
  # are 1/6, 1/3, and 1/2; expect observe this fraction of 2000 selections.
  expect_equal(dplyr::filter(behavior_tbl, curr_behavior == "Legacy")$freq, 
               1.0 / 3.0,
               tolerance = 1e-1)
  expect_equal(dplyr::filter(behavior_tbl, curr_behavior == "Adaptive")$freq,
               2.0 / 3.0,
               tolerance = 1e-1)
}
)

