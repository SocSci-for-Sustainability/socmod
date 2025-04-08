# test-trial.R

test_that("Trial records observations and outcomes correctly", {
  # Minimal model with adaptive initialization
  g <- igraph::make_ring(3)
  model <- AgentBasedModel$new(graph = g)
  
  for (agent in model$agents) {
    agent$set_behavior("Adaptive")
    agent$set_fitness(1.0)
  }
  
  # Define trivial interaction and iteration
  interact <- function(learner, teacher, model) {}
  iterate <- function(model) {}
  
  trial <- Trial$new(model, interaction = interact, iterate = iterate)
  trial$run(stop = fixated)
  
  obs <- trial$get_observations()
  out <- trial$get_outcomes()
  
  expect_s3_class(obs, "tbl_df")
  expect_true(nrow(obs) >= 1)
  expect_true("behavior" %in% names(obs))
  expect_true(out$adaptation_success)
  expect_equal(out$fixation_steps, 1)
})

test_that("Trial stops after max steps and adapts outcomes", {
  model <- AgentBasedModel$new(n_agents = 4)
  interact <- function(learner, teacher, model) {}
  iterate <- function(model) {}
  
  model$agents[[1]]$set_behavior("Adaptive")
  model$agents[[2]]$set_behavior("Legacy")
  model$agents[[3]]$set_behavior("Legacy")
  model$agents[[4]]$set_behavior("Adaptive")
  
  for (i in 1:4) {
    model$agents[[i]]$set_fitness(1.0)
  }
  
  trial <- Trial$new(model, interaction = interact, iterate = iterate)
  trial$run(stop = 3)
  
  out <- trial$get_outcomes()
  expect_false(out$adaptation_success)
  expect_equal(out$fixation_steps, 3)
})
