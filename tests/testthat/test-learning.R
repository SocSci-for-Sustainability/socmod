# test-learning.R


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

# Inside the test for frequency bias

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


# Helper to create a test model with params
example_model_with_params <- function(params = list()) {

  g <- make_small_world(N = 10, k = 2, p = 0.1)

  model_params <- list(adopt_rate = 0.1, drop_rate = 0.0, stubbornness = 0.0)

  model <- AgentBasedModel$new(graph = g, parameters = model_params)

  for (i in seq_along(model$agents)) {
    agent <- model$get_agent(i)
    agent$set_behavior("Legacy")
    agent$set_fitness(1.0)
  }

  return(model)
}


test_that("contagion_partner_selection returns a neighbor Agent", {
  model <- example_model_with_params()
  agent <- model$get_agent(1)
  partner <- contagion_partner_selection(agent, model)
  expect_true(inherits(partner, "Agent"))
  expect_true(partner$get_name() %in% sapply(agent$get_neighbors()$agents, function(a) a$get_name()))
})


test_that("learner always adopts adaptive behavior in contagion interactions when adopt_rate = 1.0", {
  model <- AgentBasedModel$new(
    n_agents = 2, parameters = list(adopt_rate = 1.0)
  )
  
  learner <- model$get_agent(1)
  teacher <- model$get_agent(2)
  
  learner$set_behavior("Legacy")
  teacher$set_behavior("Adaptive")
  
  contagion_interaction(learner, teacher, model)
  
  expect_equal(learner$get_next_behavior(), "Adaptive")
  expect_equal(learner$get_next_fitness(), 2.0)
})
 

test_that("If drop rate is 0 an agent doing Adaptive will never revert to Legacy", {
  model <- example_model_with_params(list(adopt_rate = 1.0))
  learner <- model$get_agent(1)
  teacher <- model$get_agent(2)

  learner$set_behavior("Adaptive")
  learner$set_next_behavior("Adaptive")
  learner$set_fitness(0.0)
  learner$set_next_fitness(0.0)
  teacher$set_behavior("Legacy")
  teacher$set_fitness(100.0)
  teacher$set_next_fitness(100.0)

  contagion_interaction(learner, teacher, model)

  expect_equal(learner$get_next_behavior(), "Adaptive")  # unchanged
  expect_equal(learner$get_next_fitness(), 0.0)  # fitness not changed
})

# Test contagion model step with drop

test_that("contagion_model_step causes one doing Adaptive to do Legacy when drop_rate = 1.0", {

  model <- example_model_with_params(list(drop_rate = 1.0))

  for (i in seq_along(model$agents)) {
    agent <- model$get_agent(i)
    agent$set_behavior("Adaptive")
    agent$set_fitness(2.0)
  }

  contagion_model_step(model)

  for (i in seq_along(model$agents)) {
    agent <- model$get_agent(i)
    expect_equal(agent$get_next_behavior(), "Legacy")
    expect_equal(agent$get_next_fitness(), 1.0)
  }
})


test_that("run_trial correctly stores metadata in Trial", {
  model <- AgentBasedModel$new(n_agents = 10)
  
  # Assign half to Adaptive, half to Legacy
  for (i in 1:5) model$get_agent(i)$set_behavior("Adaptive")
  for (i in 6:10) model$get_agent(i)$set_behavior("Legacy")
  
  # Define test metadata
  test_metadata <- list(seed_set = "TestGroup", adaptive_fitness = 1.5)
  
  # Run trial
  trial <- run_trial(
    model = model,
    interaction = success_bias_interact,
    iterate = iterate_learning_model,
    stop = 5,
    label = "Test Trial",
    metadata = test_metadata
  )
  
  # Check class explicitly
  expect_true(inherits(trial, "Trial"))
  
  # Check metadata exists and matches
  expect_named(trial$metadata, c("seed_set", "adaptive_fitness"))
  expect_equal(trial$metadata$seed_set, "TestGroup")
  expect_equal(trial$metadata$adaptive_fitness, 1.5)
})
