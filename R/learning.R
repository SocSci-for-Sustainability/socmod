library(purrr, include.only = c("map"))


# The iterative learning 
iterate_learning_model <- function(model) {
  
  for (agent in model$agents) {
    agent$curr_behavior <- agent$next_behavior
    agent$curr_fitness <- agent$next_fitness
  }
}



frequency_bias_select_partner <- function(learner, model) {
  
}
frequency_bias_interact <- function(learner, ., model) {}



###----- SUCCESS BIAS --------
success_bias_select_teacher <- function(learner, model) {

  neighbor_agents <- learner$neighbors$agents

  neighbor_fitnesses <- purrr::map_vec(neighbor_agents, \(n) { n$curr_fitness })

  total_fitness <- sum(neighbor_fitnesses)

  neighbor_probs <- map(neighbor_fitnesses, \(f) { f / total_fitness })

  return (sample(neighbor_agents, 1, prob = neighbor_probs)[[1]])
}


success_bias_interact <- function(learner, teacher, model) {
  if (is.null(model$learning_prob) || (runif(1) < model$learning_prob)) {
    learner$next_behavior <-  teacher$curr_behavior
    learner$next_fitness <- teacher$curr_fitness
  }
}



