#-----setup-------

library(magrittr)

if (mirai::status()$connections == 0) {
  mirai::daemons(parallel::detectCores() - 1)
}



##------------Run trials------------

# Model generating function
abm_gen_fA_experiment <- function(parameter_list) {
  # Initialize ABM with given parameters.
  abm <- do.call(make_abm, parameter_list)
  # Store parameter list w/ fewer characters
  pl <- parameter_list
  # Return the model with agents initialized as specified
  return (
    abm %>% 
      initialize_agents(
        initial_prevalence = pl$initial_prevalence,
        adaptive_fitness = pl$adaptive_fitness
      )
  )
}
# adaptive_fitness_vals <- c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0)

adaptive_fitness_vals <- c(0.8, 1.2, 2.0)

trials <-
  run_trials(
    abm_gen_fA_experiment,
    n_trials_per_param = 5,
    stop = socmod::fixated,
    n_agents = 100,
    initial_prevalence = 0.1,
    syncfile = "readme-example-trials-series.RData",
    overwrite = T,
    parallel = T,
    adaptive_fitness = adaptive_fitness_vals
  )

##------------Analyze prevalence series----------
# Select trials from just two adaptive fitness levels to plot for examples

summary <- summarise_prevalence(
  trials, across_trials = F
) %>%
  dplyr::filter(Behavior == "Adaptive") %>%
  dplyr::mutate(`Adaptive fitness` = factor(adaptive_fitness,
                                            adaptive_fitness_vals))

summary <- summarise_prevalence(
  trials, across_trials = F
) %>% 
  dplyr::filter(Behavior == "Adaptive") %>%
  dplyr::mutate(`Adaptive fitness` = factor(adaptive_fitness, 
                                            adaptive_fitness_vals))

p <- ggplot(
  summary, 
  aes(x=Step, y=Prevalence, 
      color=`Adaptive fitness`, 
      linetype=`Adaptive fitness`,
      group = trial_id)) +
  geom_line(linewidth=1.2) + theme_classic(base_size = 16) +
  ggplot2::scale_linetype_manual(values = c("dashed",  "dotdash", "solid")) +
  ggplot2::scale_color_manual(values = SOCMOD_PALETTE)

print(p)


##------------Analyze outcomes-------------------

adaptive_fitness_vals <- seq(0.6, 1.6, 0.1)
# 
# bigtrials <-
#   run_trials(
#     abm_gen_fA_experiment,
#     n_trials_per_param = 20,
#     stop = socmod::fixated,
#     n_agents = 100,
#     initial_prevalence = 0.1,
#     syncfile = "readme-example-bigtrials.RData",
#     overwrite = T,
#     adaptive_fitness = adaptive_fitness_vals
#   )


bigtrials <- run_trials(
 model_generator = abm_gen_fA_experiment,
 n_trials_per_param = 5,
 stop = 10,
 adaptive_fitness = c(1.0, 2.0),
 parallel = TRUE
)

# Sync the summarized trial observation data
summarybig <- summarise_outcomes(
  bigtrials, "adaptive_fitness", 
  outcome_measures = c("success_rate", "mean_fixation_steps")
)
