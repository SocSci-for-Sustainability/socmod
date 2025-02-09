#' A generic method for iterating a learning model, setting the current 
#' behavior and fitness to be whatever was identified as the next behavior
#' and fitness.
#'
#' @examples
#' net <- regular_lattice(10, 4)
#' model <- AgentBasedModel$new(network = net)
#' output <- run(model, 10, interact = frequency_bias_interact, 
#'               iterate_model = iterate_learning_model)
#'
#' @return NULL Operates in-place to update all agent's behavior if necessary.
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

#' Frequency biased teacher selection does nothing
#' 
#' @description
#' Don't use teacher selection for frequency bias since this is a "what" bias,
#' not a "who" bias.
#' 
#' @return NULL
#' @export
frequency_bias_select_teacher <- function(agent, model) { return (NULL) }

#' Interaction function for frequency-biased adaptive learning.
#'
#' @param learner Agent currently selected as learner.
#' @param . No selected teacher with frequency-biased learning.
#' @param model Model variable will be the agent-based model.
#'
#' @examples
#' net <- regular_lattice(10, 4)
#' model <- AgentBasedModel$new(network = net)
#' output <- run(model, 10, interact = frequency_bias_interact)
#' @return NULL 
#' @export
frequency_bias_interact <- function(learner, ., model) {
  if (is.null(model$params$learning_prob) || (runif(1) < model$params$learning_prob)) {
    
    neighbor_agents <- learner$neighbors$agents
    n_neighbors <- length(neighbor_agents)
    
    # Count neighbors doing behaviors.
    behavior_counts <- 
      dplyr::count(tibble(
        curr_behavior = 
          map_vec(learner$neighbors$agents, 
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

#' Success-biased teacher selection
#' 
#' @description 
#' The probability of selection of
#' one of the agent's neighbors is proportional to their fitness relative to
#' other neighbors.
#'
#' @param learner The focal learner agent
#' @param model Agent-based model being run with this specification.
#' @return Agent selected from neighbors as teacher.
#' @examples 
#' net <- regular_lattice(10, 4)
#' model <- AgentBasedModel$new(network = net)
#' for (agent in model$agents) {
#'   # Agents need non-zero fitness for success bias to work
#'   agent$curr_fitness <- 1
#' }
#' output <- run(model, 10, 
#'               interact = success_bias_interact, 
#'               partner_selection = success_bias_select_teacher,
#'               iterate_model = iterate_learning_model)
#'
#' @export 
success_bias_select_teacher <- function(learner, model) {

  neighbor_agents <- learner$neighbors$agents

  neighbor_fitnesses <- purrr::map_vec(neighbor_agents, \(n) { n$curr_fitness })

  total_fitness <- sum(neighbor_fitnesses)

  neighbor_probs <- purrr::map(neighbor_fitnesses, \(f) { f / total_fitness })

  return (sample(neighbor_agents, 1, prob = neighbor_probs)[[1]])
}


#' Success-biased interaction to be paired with success_bias_teacher_selection
#' that sets the learner's next behavior and fitness to be the teacher's current
#' behavior and fitness.
#' 
#' @examples 
#' net <- regular_lattice(10, 4)
#' model <- AgentBasedModel$new(network = net)
#' for (agent in model$agents) {
#'   # Agents need non-zero fitness for success bias to work
#'   agent$curr_fitness <- 1
#' }
#' output <- run(model, 10, 
#'               interact = success_bias_interact, 
#'               partner_selection = success_bias_select_teacher,
#'               iterate_model = iterate_learning_model)
#' 
#' @return NULL
#' @export
success_bias_interact <- function(learner, teacher, model) {
  if (is.null(model$params$learning_prob) || (runif(1) < model$params$learning_prob)) {
    learner$next_behavior <-  teacher$curr_behavior
    learner$next_fitness <- teacher$curr_fitness
  }
}



