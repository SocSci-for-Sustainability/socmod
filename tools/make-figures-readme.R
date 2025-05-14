##
# Script version of the README.Rmd file where we generate the plots included 
# as example outcomes in the README to have more control over its build.
#
# Date: 2025-05-12
# 


#---------Setup-------------
# devtools::load_all()
library(ggnetwork)
library(igraph)
library(magrittr) # Loads %>%
library(socmod)
library(purrr)

# Define helper function for saving to both required places for manpages and home 
.save_readme_fig <- function(root, p, width, height, dpi = 300) {
  # for (dir in c("man", file.path("docs", "reference"))) {
  #   save_path <- file.path(dir, "figures", "readme", root)
  #   ggsave(save_path, plot = p, width = width, height = height, dpi = dpi)
  # }
  save_path <- file.path("docs", "figures", "readme", root)
  ggsave(save_path, plot = p, width = width, height = height, dpi = dpi)
}


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
  edgewidth = 0.75
)
print(p)
# ***UNCOMMENT BELOW TO RE-GENERATE FIGURE***
.save_readme_fig("network-adoption.png", p, 4.15, 3.15)

# Run and visualize a single trial
p <- make_example_abm() %>% 
  run_trial %>%
  plot_prevalence(tracked_behaviors = c("Adaptive")) %>% 
  { . + ggplot2::ggtitle("Adaptive fitness = 1.125") + 
    ggplot2::theme_classic(base_size = 14) }; print(p)
# can adjust scale_x_continuous for customization
# p <- p + scale_x_continuous(breaks = 0:12)
# ***UNCOMMENT BELOW TO RE-GENERATE FIGURE***
.save_readme_fig("prevalence.png", p, width = 5, height = 2.8)






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
# 

# Run five trials per parameter setting, i.e., for each specified adaptive_fitness
adaptive_fitness_vals <- c(0.9, 1.1, 1.4)
trials <-
  run_trials(
    abm_gen_fA_experiment,
    n_trials_per_param = 5,
    stop = socmod::fixated,
    n_agents = 20,
    initial_prevalence = 0.1,
    adaptive_fitness = adaptive_fitness_vals
  )
# Summarize within trials only; rename and factor adaptive fitness for display
summary <- summarise_prevalence(
  trials, input_parameters = "adaptive_fitness", across_trials = F
) %>% 
  dplyr::mutate(
    `Adaptive fitness` = factor(adaptive_fitness, adaptive_fitness_vals)
  )
names(summary)
p <- ggplot(
  summary,
  aes(x=Step, y=Prevalence,
      color=`Adaptive fitness`,
      linetype=`Adaptive fitness`,
      group = trial_id)) +
  geom_line(linewidth=1.4, alpha = 0.875) + theme_classic(base_size = 16) +
  ggplot2::scale_color_manual(values = unname(SOCMOD_PALETTE)); print(p)
.save_readme_fig("multi-trial-prevalence.png", p, 7.25, 3.25)





####-----------Example 2B: success rate over adaptive fitness---------------

# Run twenty trials per parameter setting, i.e., for each specified 
# adaptive_fitness. This may take several minutes to run everything–reduce
# n_trials_per_param to start. There will be a progressbar with ETA, adjust
# as desired. Suggestion: start with n_agents = 20 and n_trials_per_param = 5.
adaptive_fitness_vals <- c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5)
trials_success_fixsteps <-
  run_trials(
    abm_gen_fA_experiment,
    n_trials_per_param = 20,
    stop = socmod::fixated,
    syncfile = "man/build/readme-demo-cpu-experiment.RData",
    n_agents = 100,
    .progress = T,
    # overwrite = T,
    initial_prevalence = 0.1,
    adaptive_fitness = adaptive_fitness_vals
  )
# Summarise outcomes over adaptive fitness
outcomes <- summarise_outcomes(
  trials_success_fixsteps,
  input_parameters = "adaptive_fitness", 
  outcome_measures = c("success_rate", "mean_fixation_steps")
) 
# Calculate maximum fixation time for normalization
max_fix_time <- max(outcomes$Value[outcomes$Measure == "mean_fixation_steps"])
# Normalize mean fixation steps so the 
outcomes_norm <- outcomes %>%
  dplyr::mutate(Value = dplyr::case_when(
    Measure == "mean_fixation_steps" ~ Value / max_fix_time,
    TRUE ~ Value
  ))
# Rename and set order of Measure factors to avoid messing with the legend in plotting
outcomes_norm$Measure[outcomes_norm$Measure == "success_rate"] <- "Success rate"
outcomes_norm$Measure[outcomes_norm$Measure == "mean_fixation_steps"] <- "Normalized fixation time"
outcomes_norm$Measure <- factor(outcomes_norm$Measure, levels = c(
  "Success rate", "Normalized fixation time"
))
# Use a custom socmod line color
line_color <- SOCMOD_PALETTE_CVD["pink"]
p_success_fixation <-
  outcomes_norm %>%
  ggplot2::ggplot(aes(x = adaptive_fitness, y = Value, linetype = Measure)) +
  geom_line(color = line_color, linewidth=1.5) + 
  scale_x_continuous(breaks = adaptive_fitness_vals) +
  theme_classic(base_size = 16) + 
  xlab("Adaptive fitness") + 
  ylab("Measure value")

.save_readme_fig("success_fixsteps.png", p_success_fixation, 7.8, 3.55)
