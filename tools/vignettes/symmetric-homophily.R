##
# 
# Collection of helpers for creating figures for symmetric homophily vignette
# 
# Date: 2025-14-25
#

#--------------SETUP----------------
# devtools::load_all()
library(ggplot2)  # Import full library so we don't have to write ggplot2 in plot calls
library(magrittr)


#------ABM GENERATOR FUNCTION----------

gen_homophily_abm <- function(param_row) {
  
  # Extract all parameters used in ABM construction with short var names
  pr <- param_row; N <- pr$n_agents; m <- pr$minority_fraction; k <- pr$mean_degree; 
  h <- pr$homophily; f_A <- pr$adaptive_fitness; start_group <- pr$start_group
  
  # Calculate number of minorty and majority agents
  N_min <- round(N * m)
  N_maj <- N - N_min
  print(h)
  # Initialize agent-based model with the homophily graph, ensuring connected graph
  h_graph <- make_homophily_network(
    group_sizes = c(N_min, N_maj),
    mean_degree = k,
    homophily = h
  )
  # The network construction is random, doesn't always create connected network;
  # re-create h_graph until it's connected
  while (!igraph::is_connected(h_graph)) {
    print("Not yet connected, attempting to create connected homophily network")
    
    # Create the homophily network given the parameters
    h_graph <- make_homophily_network(
      group_sizes = c(N_min, N_maj),
      mean_degree = k,
      homophily = h
    )
  }
  # Create ABM with guaranteed-connected h_graph
  abm <- make_abm(graph = h_graph, mean_degree = k, homophily = h, 
                  group_sizes = c(N_min, N_maj), minority_fraction = m)
  
  minority_idxs <- which(igraph::V(abm$get_network())$group == 1)
  majority_idxs <- which(igraph::V(abm$get_network())$group == 2)
  abm$set_parameter("minority_idxs", minority_idxs)
  abm$set_parameter("majority_idxs", majority_idxs)
  
  # Set agent Group attributes
  purrr::walk(abm$agents[minority_idxs], \(a) a$set_attribute("Group", "Minority"))
  purrr::walk(abm$agents[majority_idxs], \(a) a$set_attribute("Group", "Majority"))
  
  # Initialize one agent from one or each group with adaptive behavior 
  params <- abm$get_parameters()$as_list()
  if (start_group == "Minority" || start_group == "Both") {
    a_min <- sample(abm$agents[minority_idxs], 1)[[1]]
    a_min$set_behavior("Adaptive")
    a_min$set_fitness(f_A) 
  }
  if (start_group == "Majority" || start_group == "Both") {
    a_maj <- sample(abm$agents[majority_idxs], 1)[[1]]
    a_maj$set_behavior("Adaptive")
    a_maj$set_fitness(f_A)
  }
  
  return (abm)
}

make_example_homophily_abm <- function(n_agents = 50, minority_fraction = 0.1,
                                       mean_degree = 5, homophily = 0.2,
                                       adaptive_fitness = 1.2, 
                                       start_group = "Both") {
  param_row <- tibble::as_tibble(as.list(environment()))
  return (gen_homophily_abm(param_row))                                       
}

abm <- make_example_homophily_abm()
devtools::load_all()
p <- plot_homophily_network_adoption(abm)
print(p)

trial <- run_trial(abm)

plot_prevalence(trial, tracked_behaviors = c("Adaptive"))

#------------- CPU EXPERIMENT OVER A FEW HOMOPHILY VALUES --------------

n_agents = 100
minority_fraction = 0.1
mean_degree = 10
homophily_vals = c(-0.5, -0.25, 0.0, 0.25, 0.5)
adaptive_fitness = 1.2
start_group = "Both"
# devtools::load_all()
trials <- run_trials(
  gen_homophily_abm, stop = socmod::fixated, 
  n_trials_per_param = 10, .progress = TRUE, 
  syncfile = "small-homoph.RData", 
  # overwrite = T, 
  n_agents = n_agents, 
  minority_fraction = minority_fraction, 
  mean_degree = mean_degree,
  adaptive_fitness = adaptive_fitness, 
  start_group = start_group, 
  homophily = homophily_vals
)

prev_summary <- 
  summarise_prevalence(trials, 
                       input_parameters = "homophily", 
                       across_trials = F) %>%
  dplyr::rename(Homophily = homophily) %>%
  dplyr::mutate(Homophily = factor(homophily, homophily_vals))

prev_summary %>% dplyr::filter(Homophily %in% c(-0.5, 0.0, 0.5)) %>%
  ggplot(
    aes(x = Step, y = Prevalence, 
        color = Homophily, linetype = Homophily, group = trial_id)
  ) +
  geom_line(linewidth = 1.4, alpha = 0.875) + 
  theme_classic(base_size = 16) +
  scale_color_manual(values = unname(SOCMOD_PALETTE))
