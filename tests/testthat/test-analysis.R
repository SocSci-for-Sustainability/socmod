test_that("summarise_prevalence summarizes prevalence across trials", {
  # Create mock trials
  trials <- purrr::map(1:3, function(ii) {
    model <- make_abm(make_model_parameters(n_agents = 5))
    trial <- run_trial(model, stop = 10)
    return (trial)
  })
  
  # Summarize within each trial's series indexed by trial_id
  prevalence_summary_keeptrials <- summarise_prevalence(trials, across_trials = FALSE)
  
  expect_s3_class(prevalence_summary_keeptrials, "tbl_df")
  expect_true(all(c("Step", "Behavior", "Count", "Prevalence", "trial_id") %in% names(prevalence_summary_keeptrials)))
  expect_true(nrow(prevalence_summary_keeptrials) >= 1)
  
  # If any input parameters were included, test they appear as columns
  param_cols <- names(trials[[1]]$model$get_parameters()$as_list())
  expect_true(all(param_cols %in% names(prevalence_summary_keeptrials)))

  # Now summarize over all agents and trials: removes trial_id, keeps others
  prevalence_summary <- summarise_prevalence(trials)
  expect_s3_class(prevalence_summary, "tbl_df")
  expect_true(
    all(c("Step", "Behavior", "Count", "Prevalence") %in% 
        names(prevalence_summary))
  )
  expect_true(nrow(prevalence_summary) >= 1)
  expect_false("trial_id" %in% names(prevalence_summary))
})

test_that("initialize_agents sets correct number of adaptive and legacy agents", {
  model <- make_abm(n_agents = 20)
  initialize_agents(model, initial_prevalence = 0.25, adaptive_fitness = 2.0, legacy_fitness = 1.0)
  
  behaviors <- vapply(model$agents, \(a) a$get_behavior(), character(1))
  fitnesses <- vapply(model$agents, \(a) a$get_fitness(), numeric(1))
  
  expect_equal(sum(behaviors == "Adaptive"), 5)
  expect_equal(sum(behaviors == "Legacy"), 15)
  expect_true(all(fitnesses[behaviors == "Adaptive"] == 2.0))
  expect_true(all(fitnesses[behaviors == "Legacy"] == 1.0))
})

test_that("make_abm(...) passes args to make_model_parameters", {
  model <- make_abm(n_agents = 10)
  expect_s3_class(model, "AgentBasedModel")
  expect_equal(model$get_parameter("n_agents"), 10)
})

