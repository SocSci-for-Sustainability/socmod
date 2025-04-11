# test-trial.R

test_that("Trial records observations and outcomes correctly", {
  
  # Minimal model with adaptive initialization.
  g <- igraph::make_ring(3)
  model <- AgentBasedModel$new(graph = g)
  
  for (agent in model$agents) {
    agent$set_behavior("Adaptive")
    agent$set_fitness(1.0)
  }
  
  # Define trivial interaction and iteration.
  interact <- function(learner, teacher, model) {}
  iterate <- function(model) {}
  
  trial <- Trial$new(model, interaction = interact, iterate = iterate)
  trial$run(stop = fixated)
  
  obs <- trial$get_observations()
  out <- trial$get_outcomes()
  
  expect_s3_class(obs, "tbl_df")
  expect_true(nrow(obs) >= 1)
  expect_true("Behavior" %in% names(obs))
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

test_that("run_trials() returns expected number of Trial objects", {
  gen <- function() {
    agents <- list(
      Agent$new(1, name = "1", behavior = "Legacy", fitness = 1),
      Agent$new(2, name = "2", behavior = "Adaptive", fitness = 4)
    )
    net <- igraph::make_graph(~ 1-2)
    AgentBasedModel$new(agents = agents, graph = net)
  }
  
  trials <- run_trials(n = 5, model_generator = gen, stop = 10)
  expect_length(trials, 5)
  expect_true(all(purrr::map_lgl(trials, ~ inherits(.x, "Trial"))))
  
  # optional: check observation structure
  obs_list <- purrr::map(trials, ~ .x$get_observations())
  expect_true(all(purrr::map_lgl(obs_list, tibble::is_tibble)))
})


test_that("summarise_by_label() correctly aggregates trial outcomes", {
  gen <- function() {
    agents <- list(
      Agent$new(1, name = "1", behavior = "Adaptive", fitness = 4),
      Agent$new(2, name = "2", behavior = "Legacy", fitness = 1)
    )
    graph <- igraph::make_graph(~ 1-2)
    AgentBasedModel$new(agents = agents, graph = graph)
  }
  
  trials <- append(
    run_trials(3, gen, label = "success", stop = 5),
    run_trials(2, gen, label = "control", stop = 5)
  )
  
  summary <- summarise_adoption(trials)
  result <- summarise_by_label(summary)
  
  # print(summary)
  # print(result)
  
  expect_true(is.data.frame(result))
  expect_true(
    all(
      c("label", "n_trials", "success_rate", "mean_fixation", "sd_fixation") %in% 
      names(result)
    )
  )
  
  expect_equal(nrow(result), 2)
  expect_equal(sort(unique(result$label)), c("control", "success"))
})
