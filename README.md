
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

The goal of `socmod` is to simplify development of models of social
behavior for beginners and experts alike. The current focus is
agent-based models of adaptive behavior diffusion. The framework is
designed to be extensible to other contexts in social behavior modeling
and beyond.

## Quickstart examples

Below we present two examples of how `socmod` can be used for succinctly
defining models of social behavior.

1.  Initialize and run, then visualize adaptation diffusion over time
    for a single simulation trial
2.  Set up and run a computational experiment of simulated *adaptation
    success* (i.e., fixation of adaptive behavior) and time to fixation
    for different adaptive fitness values when agents learn using
    success-biased partner selection

### Prereq: load required libraries.

``` r
library(ggnetwork)
library(igraph)
library(magrittr) # Loads %>%
library(socmod)
library(purrr)
```

### Example 1: single model trial and visualization

``` r
# Helper function to create an agent-based model on a N=10, k=4 regular lattice
# with 10% of agents doing Adaptive behavior w/ fitness 1.125 (Legacy is 1.0 by default)
make_example_abm <- function() {
  return (
    make_abm(graph = make_regular_lattice(10, 4)) %>%
      # Initialize 
      initialize_agents(
        initial_prevalence = 0.2,  
        adaptive_fitness = 1.125
      )
  )
}

# Create a new instance of an ABM with example parameters
abm <- make_example_abm()

# Inspect the initialization to ensure 2 agents do Adaptive
plot_network_adoption(
  abm, layout = igraph::in_circle(), 
  plot_mod = 
    \(p) p + ggtitle("Adoption at t = 0"),
  edgewidth = 0.4
)
```

![](man/figures/readme/network-adoption.png)

``` r
# Initialize fresh ABM for a new simulation each time
trial <- make_example_abm() %>% run_trial
summarise_prevalence(trial)

plot_prevalence(trial, tracked_behavior = c("Adaptive"))
```

### Example 2: computational experiment over adaptive fitness

Success-biased learning is the default, so we do not need to specify the
learning strategy. We can just define the model generating function to
initialize a certain fraction with the adaptive behavior and vary the
adaptive fitness, the main outcome variable of this analysis. Note the
functional style: we pass two functions as arguments to `run_trials`:
`abm_gen_fA_experiment` and `socmod::fixated`.

``` r

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
  ggplot2::scale_color_manual(values = SOCMOD_PALETTE)
```

``` r
print(p)
```

``` r
# Run five trials per parameter setting, i.e., for each specified adaptive_fitness
adaptive_fitness_vals <- c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0)
trials <- 
  run_trials(
    abm_gen_fA_experiment, 
    n_trials_per_param = 5,
    # n_trials_per_param = 20,  # <- uncomment to try accumulating more simulation data
    stop = socmod::fixated,
    syncfile = "readme-demo-cpu-experiment.RData",
    n_agents = 20,
    .progress = T,
    # overwrite = T,
    initial_prevalence = 0.1,
    adaptive_fitness = adaptive_fitness_vals
  )
```

Now we have a total of 40 trials: 8 adaptive fitness values times five
trials per setting:

``` r
length(trials) == 40
```

``` r
outcomes <- summarise_outcomes(
  trials, 
  input_parameters = "adaptive_fitness", 
  outcome_measures = c("success_rate", "mean_fixation_steps")
) 

max_fix_time <- max(outcomes$Value[outcomes$Measure == "mean_fixation_steps"])

outcomes_norm <- outcomes %>%
  dplyr::mutate(Value = dplyr::case_when(
    Measure == "mean_fixation_steps" ~ Value / max_fix_time,
    TRUE ~ Value
  ))


print(outcomes_norm, n = Inf)
```

``` r
# Rename and set order of Measure factors to avoid messing with the legend in plotting
outcomes_norm$Measure[outcomes_norm$Measure == "success_rate"] <- "Success rate"
outcomes_norm$Measure[outcomes_norm$Measure == "mean_fixation_steps"] <- "Normalized fixation time"
outcomes_norm$Measure <- factor(outcomes_norm$Measure, levels = c(
  "Success rate", "Normalized fixation time"
))
# Use a custom socmod line color
line_color <- SOCMOD_PALETTE_CVD["pink"]
outcomes_norm %>%
  ggplot2::ggplot(aes(x = adaptive_fitness, y = Value, linetype = Measure)) +
    geom_line(color = line_color, linewidth=1.5) + scale_x_continuous(breaks = adaptive_fitness_vals) +
    theme_classic(base_size = 16) + xlab("Adaptive fitness") + ylab("Value")
```

### Installation

You can install the development version of socmod from
[GitHub](https://github.com/) with `devtools` or `pak`:

**devtools**

``` r
# Install this if you don't have devtools.
install.packages("devtools")
devtools::install_github("css4s/socmod")
```

**pak**

``` r
# Install this if you don't have pak.
install.packages("pak")
pak::pak("css4s/socmod")
```

## More information and the philosophy of socmod

Different models of social behavior are specified by the details of how
many individuals are in a population, what behaviors or opinions they do
or have, what benefits they accrue(d) through their behaviors, how they
learn or influence one another, and any environmental or other relevant
factors. This framework seeks to encapsulate different approaches to
modeling diverse social behaviors, such as those thoroughly reviewed in
Paul Smaldino’s (2023) textbook [*Modeling Social
Behavior*](https://press.princeton.edu/books/paperback/9780691224145/modeling-social-behavior?srsltid=AfmBOop2zNSsOtNlOMs6uaLTlAQs8saVMC_I6y_OnyklIKz-GUnoNapR).

Technically, `socmod` uses object-oriented programming, provided by
[`R6`](https://r6.r-lib.org/), and functional-style agent and model
behavior specification inspired by
[Agents.jl](https://juliadynamics.github.io/Agents.jl/stable/), which I
myself have enjoyed using. But, I still had to do my plotting in R, and
more beginning students across disciplines will tend to know R than
Julia. R also seems to have a great community with the `r-lib` project
that seems to be bringing a continuity to scientific programming that I
have not seen in any other programming language.
