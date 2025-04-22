devtools::load_all();
# mps <- make_model_parameters(
#   contagion_learning_strategy, n_agents = 10, adaptive_fitness = 2.0, 
#   legacy_fitness = 1.0, drop_rate = 0.01, adoption_rate = 1.0
# )
# 
# cabm <- make_abm(mps); 
# cabm$get_agent(1)$set_behavior("Baglowie"); 
# 
# cabm$get_agent(5)$set_behavior("Baglowie"); 
# 
# trial <- run_trial(cabm, stop = fixated, adaptive_behavior = "Baglowie")

# Now try run_trials_grid with just two .
# Create toy 4-agent ABM
gen <- function(model_parameter_row) {
  
  # Extract adaptive_fitness to create agents.
  adaptive_fitness <- model_parameter_row$adaptive_fitness

  agent_1 <- Agent$new(1, behavior = "Legacy", fitness = 1.0, name = "a1")
  agent_2 <- Agent$new(2, behavior = "Adaptive", 
                       fitness = adaptive_fitness, name = "a2")
  agent_3 <- Agent$new(3, behavior = "Legacy", fitness = 1.0, name = "a3")
  agent_4 <- Agent$new(4, behavior = "Legacy", fitness = 1.0, name = "a4")
  
  agents <- list(agent_1, agent_2, agent_3, agent_4)
  graph <- igraph::make_graph(~ 1-2, 1-3, 1-4, 3-2)

  # Extract other necessary model parameters.
  learning_strategy <- model_parameter_row$learning_strategy
  drop_rate <- model_parameter_row$drop_rate
  adoption_rate <- model_parameter_row$adoption_rate
  
  # Make ModelParameters to encapsulate this model's parameters.
  model_parameters <- make_model_parameters(
    learning_strategy, graph, adaptive_fitness = adaptive_fitness, 
    adoption_rate = adoption_rate, drop_rate = drop_rate
  )

  return (
    make_abm(
      model_parameters,
      agents = agents
    )
  )
}

trials <- run_trials(
  gen, 
  n_trials_per_param = 50, 
  stop = fixated, 
  syncfile = "sync_Lecture7_example_adoption-rate.RData",
  # overwrite = TRUE,
  learning_strategy = c(success_bias_learning_strategy,
                        frequency_bias_learning_strategy,
                        contagion_learning_strategy),
  # adaptive_fitness = c(0.8, 1.0, 1.2, 1.4, 2.0),
  # adaptive_fitness = seq(0.8, 2.4, 0.2),
  adaptive_fitness = 1.4,
  adoption_rate = c(0.05, 0.2, 0.4, 0.6, 0.8, 1.0),
  # adoption_rate = 0.6,
  drop_rate = 0.2
)

trials_summary <- summarise_by_parameters(
  trials, c("learning_strategy", "adoption_rate")
)

# trials_summary <- summarise_by_parameters(
#   trials, c("learning_strategy", "adaptive_fitness")
# )
