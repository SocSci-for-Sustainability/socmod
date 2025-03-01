.agent_exposure_prob <- function(agent) {
  
  curr_behaviors <- agent$neighbors$map(\(a) { a$curr_behavior })
  
  n_neighbors_adopted <- sum(
    purrr::map_vec(curr_behaviors, \(b) { ifelse(b == "Adaptive", 1, 0) })
  )
  
  return (n_neighbors_adopted / agent$neighbors$n)
}


#' Agent for use with AgentBasedModel instances
#' 
#' @description
#' Agent attributes include information about previous, current, and next behaviors, 
#' neighbors (an instance of the Neighbors class). Use class methods 
#' to calculate exposure probability, add neighbors, or set agent fitness.
#' @export
Agent <- R6Class(classname="Agent", public = list(
  
  #' @field prev_behavior Previous agent behavior.
  prev_behavior = "",
  #' @field curr_behavior Current agent behavior.
  curr_behavior = "",
  #' @field next_behavior Next behavior the agent could do, depending on iterate_model.
  next_behavior = "",
  #' @field neighbors Initial set of neighbors.
  neighbors = c(),
  #' @field prev_fitness Previous agent fitness.
  prev_fitness = 0.0,
  #' @field curr_fitness Current agent fitness.
  curr_fitness = 0.0,
  #' @field next_fitness Next fitness.
  next_fitness = 0.0,
  #' @field name Agent's name.
  name = NULL,

  #' @description
  #' Create a new Agent instance. 
  #' @param behavior Initial agent behavior.
  #' @param fitness Agent fitness.
  #' @param name Agent name; should be unique or maybe face unexpected problems.
  #' @param neighbors Initialize neighbors; typically done in ABM initialization.
  #' @return A new `Agent` object.
  initialize = 
    function(behavior = "", fitness = 0.0, name = NULL, neighbors = c()) {
      
      self$prev_behavior <- behavior
      self$curr_behavior <- behavior
      self$next_behavior <- behavior
      # print(fitness)
      self$prev_fitness <- fitness
      self$curr_fitness <- fitness
      self$next_fitness <- fitness
      
      # print(self$curr_fitness)
      
      self$name <- name
      self$add_neighbors(neighbors)  
  },
  
  #' Add agents to this agent's `neighbor` field
  #'
  #' @param new_neighbors List of Agents to add as neighbors.
  #' @return self (Agent)
  add_neighbors = function(new_neighbors) {
    
    self$neighbors <- Neighbors$new(c(self$neighbors$agents, new_neighbors))

    invisible(self)
  },  

  #' Calculate the exposure probability for this agent. 
  #' 
  #' @return Float ≥ 0 and ≤ 1 representing the probability of a non-trivial 
  #' exposure to the adaptive behavior, where *non-trivial* exposure means the learner
  #' is doing the legacy behavior and selects an agent doing the adaptive behavior.
  exposure_prob = function() {
    return (.agent_exposure_prob(self))
  },

  #' Set the agent's current fitness value.
  #' 
  #' @param fitness The fitness to assign.
  #' @return self
  set_fitness = function(fitness) {
    
    self$curr_fitness = fitness
    
    invisible(self)
  }
))

#' Encapsulation of an agent's neighbors.
#'
#' @description
#' Encapsulate neighbors to easily access them by name, know how many there are,
#' check if an agent is among another's neighbors, or map a function across
#' all the agents.
#'
#' @export
Neighbors <- R6Class(classname = "Neighbors", public = list(
  
  #' @field agents Get neighbors as list of agents.
  agents = c(),
  
  #' @field n Number of neighbors
  n = 0,
  
  #' Create a new instance of Neighbors using a list of Agents.
  #' @param agents The agents to be included as neighbors.
  initialize = function(agents) { 
    self$agents <- agents 
    self$n <- length(agents)
    invisible(self)
  },
  
  #' Get neighbor by name, returning NULL if named agent is not a neighbor.
  #' @param name Agent name
  get = function(name) { 
    return (self$agents[[name]]) 
  },
  
  #' Wrap purrr::map to apply function to all neighbor agents.
  #'
  #' @param f Function to apply to all neighbors.
  #' @return the result of the mapping, *not* self
  map = function(f) { 
    
    return(purrr::map(self$agents, f))
  },

  #' Check by name if an agent is one of the neighbors.
  #'
  #' @param name Name of the agent to check.
  #' @return boolean indicating whether named agent is a neighbor
  contains = function(name) {
     
    return (name %in% names(self$agents))
  }
))
