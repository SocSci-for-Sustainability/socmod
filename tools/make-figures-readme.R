##
# Script version of the README.Rmd file where we generate the plots included 
# as example outcomes in the README to have more control over its build.
#
# Date: 2025-05-12
# 


#---------Setup-------------
library(ggnetwork)
library(igraph)
library(magrittr) # Loads %>%
library(socmod)
library(purrr)


###------------------Example 1: single model trial and visualization-----------
make_example_abm <- function() {
  return (
    make_abm(graph = make_regular_lattice(10, 4)) %>%
      # Initialize 10% w/ Adaptive,
      # fitness 12.5% greater than Legacy
      initialize_agents(
        initial_prevalence = 0.2,  
        adaptive_fitness = 1.125
      )
  )
}

abm <- make_example_abm()

# Inspect the initialization
p <- plot_network_adoption(
  abm, layout = igraph::in_circle(), 
  plot_mod = 
    \(p) p + ggtitle("Adoption at t = 0"),
  edgewidth = 0.4
)
print(p)
ggsave(
  "man/figures/readme/network-adoption.png", plot = p, 
  width = 6, height = 4, dpi = 300
)

# Initialize fresh ABM for a new simulation each time
devtools::load_all()
trial <- make_example_abm() %>% run_trial
print(summarise_prevalence(trial))

print(p)
p <- plot_prevalence(trial, tracked_behaviors = c("Adaptive"))

p <- p + scale_x_continuous(breaks = 0:12)
print(p)
ggsave(
  "man/figures/readme/prevalence.png", plot = p, width = 6, height = 3, dpi = 300
)
###------------Example 2: computational experiment over adaptive fitness-------
####-----------Example 2A: plot several trials prevalence series---------------

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

# Run five trials per parameter setting, i.e., for each specified adaptive_fitness
adaptive_fitness_vals <- c(0.8, 1.15, 1.4)
trials <- 
  run_trials(
    abm_gen_fA_experiment, 
    n_trials_per_param = 5,
    stop = socmod::fixated,
    n_agents = 20,
    initial_prevalence = 0.1,
    adaptive_fitness = adaptive_fitness_vals
  )

summary <- summarise_prevalence(
  trials, across_trials = F
) %>%
  dplyr::filter(Behavior == "Adaptive") %>%
  dplyr::filter(adaptive_fitness %in% c(0.8, 1.15, 1.4)) %>%
  dplyr::mutate(`Adaptive fitness` = factor(adaptive_fitness,
                                            adaptive_fitness_vals))

p <- ggplot(
  summary, 
  aes(x=Step, y=Prevalence, 
      color=`Adaptive fitness`, 
      linetype=`Adaptive fitness`,
      group = trial_id)) +
  geom_line(linewidth=1.4, alpha = 0.7) + theme_classic(base_size = 16) +
  ggplot2::scale_color_manual(values = SOCMOD_PLOT_PALETTE)

ggsave(
  "man/figures/readme-summary-prevalence.png", plot = p, 
  width = 6, height = 4, dpi = 300
)


####-----------Example 2B: success rate over adaptive fitness---------------


# Run five trials per parameter setting, i.e., for each specified adaptive_fitness
# This may take a few minutes to run everything–reduce n_trials_per_param to
# start.
adaptive_fitness_vals <- c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5)
trials <- 
  run_trials(
    abm_gen_fA_experiment, 
    n_trials_per_param = 20,
    stop = socmod::fixated,
    syncfile = "man/build/readme-demo-cpu-experiment.RData",
    n_agents = 20,
    .progress = T,
    # overwrite = T,
    initial_prevalence = 0.1,
    adaptive_fitness = adaptive_fitness_vals
  )

outcomes <- summarise_outcomes(
  trials, 
  input_parameters = "adaptive_fitness", 
  outcome_measures = c("success_rate", "mean_fixation_steps")
) 

# Calculate the mean  
max_fixation_steps <- 
  (dplyr::filter(outcomes, Measure == "mean_fixation_steps") %>%
  dplyr::filter(Value == max(Value)))$Value
  
mean_fix_steps <- 
  dplyr::filter(outcomes, Measure == "mean_fixation_steps") %>%
  dplyr::group_by(Measure) %>%
  dplyr::mutate(Value = Value/max_fixation_steps)

outcomes %>% dplyr::filter(Measure == "success_rate") %>%
ggplot2::ggplot(aes(x = adaptive_fitness, y = Value)) +
  geom_line(color = SOCMOD_PLOT_PALETTE[1], linetype = "solid", linewidth=1.5) + 
  geom_line(
    mean_fix_steps, mapping = aes(x=adaptive_fitness, y = Value), 
    color = SOCMOD_PLOT_PALETTE[1], linetype = "dashed"
  ) + 
  theme_classic(base_size = 16) + 
  xlab("Adaptive fitness") + ylab("Success rate/norm. fix. time")

