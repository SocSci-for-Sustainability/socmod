library(dplyr, include.only = c("count", "slice_max"))
library(purrr, include.only = c("map", "map_vec"))
library(tibble)

source("R/run.R")

# The iterating learning is the same in either case.

#' @export
iterate_learning_model <- function(model) {
  
  for (agent in model$agents) {
    agent$curr_behavior <- agent$next_behavior
    agent$curr_fitness <- agent$next_fitness
  }
}


### ------ FREQUENCY BIAS --------
# Frequency bias partner selection is just the default that returns NULL
# expecting to not be used for selecting interaction partner.

#' @export
frequency_bias_select_teacher <- partner_selection_default


#' Interaction function for frequency-biased adaptive learning.
#'
#' @param learner Agent currently selected as learner.
#' @param . No selected teacher with frequency-biased learning.
#' @param model Model variable will be the agent-based model.
#'
#' @return NULL
#' @export
#'
#' @examples
frequency_bias_interact <- function(learner, ., model) {
  if (is.null(model$learning_prob) || (runif(1) < model$learning_prob)) {
    
    neighbor_agents <- learner$neighbors$agents
    n_neighbors <- length(neighbor_agents)
    
    # Count neighbors doing behaviors.
    behavior_counts <- 
      dplyr::count(tibble(
        curr_behavior = 
          purrr::map_vec(learner$neighbors$agents, 
                  \(n) {n$curr_behavior}),
      ), curr_behavior)
    
    # Calculate the probability of selecting each one, appending to behavior_counts.
    behavior_counts$selection_prob <- behavior_counts$n / n_neighbors
    
    # Next behavior selected via weighted random sampling.
    learner$next_behavior <- 
      sample(behavior_counts$curr_behavior, 1, 
             prob = behavior_counts$selection_prob)[[1]]
  }
}

### ----- SUCCESS BIAS --------

#' @export
success_bias_select_teacher <- function(learner, model) {

  neighbor_agents <- learner$neighbors$agents

  neighbor_fitnesses <- purrr::map_vec(neighbor_agents, \(n) { n$curr_fitness })

  total_fitness <- sum(neighbor_fitnesses)

  neighbor_probs <- purrr::map(neighbor_fitnesses, \(f) { f / total_fitness })

  return (sample(neighbor_agents, 1, prob = neighbor_probs)[[1]])
}


#' @export
success_bias_interact <- function(learner, teacher, model) {
  if (is.null(model$learning_prob) || (runif(1) < model$learning_prob)) {
    learner$next_behavior <-  teacher$curr_behavior
    learner$next_fitness <- teacher$curr_fitness
  }
}



