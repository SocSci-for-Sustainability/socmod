## -----------SETUP------------------
library(ggplot2)
# Model generation function used in both computational experiments below.
gen <- function(model_parameter_row) {
  
  # Extract adaptive_fitness to create agents.
  adaptive_fitness <- model_parameter_row$adaptive_fitness
  
  agent_1 <- socmod::Agent$new(1, behavior = "Legacy", 
                               fitness = 1.0, name = "a1")
  agent_2 <- socmod::Agent$new(2, behavior = "Adaptive", 
                               fitness = adaptive_fitness, name = "a2")
  agent_3 <- socmod::Agent$new(3, behavior = "Legacy", 
                               fitness = 1.0, name = "a3")
  agent_4 <- socmod::Agent$new(4, behavior = "Legacy", 
                               fitness = 1.0, name = "a4")
  
  agents <- list(agent_1, agent_2, agent_3, agent_4)
  graph <- igraph::make_graph(~ 1-2, 1-3, 1-4, 3-2)
  
  # Extract other necessary model parameters.
  learning_strategy <- model_parameter_row$learning_strategy
  drop_rate <- model_parameter_row$drop_rate
  adoption_rate <- model_parameter_row$adoption_rate
  
  # Make ModelParameters to encapsulate this model's parameters.
  model_parameters <- socmod::make_model_parameters(
    learning_strategy, graph, adaptive_fitness = adaptive_fitness, 
    adoption_rate = adoption_rate, drop_rate = drop_rate
  )
  
  return (
    socmod::make_abm(
      model_parameters,
      agents = agents
    )
  )
}


## -----------ADAPTIVE FITNESS EXPERIMENT----------

cat("\nRunning adaptive fitness experiment...\n")

if (!("trials_adaptive_fitness" %in% ls(all.names = TRUE))) {
  trials_adaptive_fitness <- socmod::run_trials(
    gen, 
    n_trials_per_param = 100, 
    stop = socmod::fixated, 
    syncfile = "trials-adaptive_fitness.RData",
    # overwrite = TRUE,
    learning_strategy = c(socmod::success_bias_learning_strategy,
                          socmod::frequency_bias_learning_strategy,
                          socmod::contagion_learning_strategy),
    adaptive_fitness = seq(0.8, 2.4, 0.2),
    adoption_rate = 0.6,
    drop_rate = 0.2
  )
}

trials_summary <- socmod::summarise_by_parameters(
  trials_adaptive_fitness, c("learning_strategy", "adaptive_fitness")
)

trials_success_rate <- dplyr::filter(trials_summary, 
                                     Measure == "success_rate")

p <- ggplot(trials_success_rate, 
              aes(x=adaptive_fitness, y=Value, 
                  color=learning_strategy)
) +
  geom_line(linewidth=1.0) + geom_point(size=2.15) +
  ggsci::scale_color_aaas() + ggsci::scale_fill_aaas() +
  xlab("Adaptive fitness") + ylab("Success rate") +
  scale_x_continuous(breaks = sort(
    unique(trials_summary$adaptive_fitness)
  )) +
  theme_classic(base_size=14) +
  ylim(0, 1.0) +
  guides(color = guide_legend(title = "Learning strategy")) +
  ggtitle("Adoption rate = 0.6, drop rate = 0.2")

ggsave("vignettes/resources/adaptive_fitness_experiment.png", p, 
       width = 5.75, height = 3)


## -----------ADOPTION RATE EXPERIMENT----------

cat("Running adoption rate experiment...")

if (!("trials_adoption" %in% ls(all.names = TRUE))) {
  trials_adoption <- socmod::run_trials(
    gen,
    n_trials_per_param = 100,
    stop = socmod::fixated, 
    syncfile = "trials-adoption-rate.RData",
    overwrite = TRUE,
    learning_strategy = c(socmod::success_bias_learning_strategy,
                          socmod::frequency_bias_learning_strategy,
                          socmod::contagion_learning_strategy),
    adaptive_fitness = 1.4,
    adoption_rate = c(0.05, 0.2, 0.4, 0.6, 0.8, 1.0),
    drop_rate = 0.2
  )
}
trials_summary <- socmod::summarise_by_parameters(
  trials_adoption, c("learning_strategy", "adoption_rate")
)

trials_success_rate <- dplyr::filter(
  trials_summary, Measure == "success_rate"
)

p <- ggplot(trials_success_rate, 
            aes(x=adoption_rate, y=Value, color=learning_strategy)) +
  geom_line(linewidth=1.0) + geom_point(size=2.15) +
  ggsci::scale_color_aaas() + ggsci::scale_fill_aaas() +
  xlab("Adoption rate") + ylab("Success rate") +
  scale_x_continuous(breaks = sort(unique(trials_summary$adoption_rate))) +
  theme_classic(base_size=14) +
  ylim(0, 1.0) +
  guides(color = guide_legend(title = "Learning strategy")) +
  ggtitle("Adaptive fitness = 1.4, drop rate = 0.2")

ggsave("vignettes/resources/adoption_rate_experiment.png", 
       p, width=5.75, height=3)
