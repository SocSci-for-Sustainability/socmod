library(R6)

#' Agent for use with AgentBasedModel instances
#' 
#' @description
#' Agent attributes include information about previous, current, and next behaviors, 
#' neighbors (an instance of the Neighbors class). Methods include 
#' exposure probability,
Agent <- R6Class(classname="Agent", public = list(
  prev_behavior = "",
  curr_behavior = "",
  next_behavior = "",
  neighbors = c(),
  curr_fitness = 0.0,
  next_fitness = 0.0,
  name = "",

  #' @description
  #' Create a new Agent instance. 
  #' @param behavior Initial agent behavior.
  #' @param fitness Agent fitness.
  #' @param name Agent name; should be unique or maybe face unexpected problems.
  #' @param neighbors Initialize neighbors; typically done in ABM initialization.
  #' @return A new `Agent` object.
  initialize = 
    function(behavior = "", fitness = 0.0, name = "", neighbors = c()) {

      self$curr_behavior <- behavior
      self$prev_behavior <- behavior
      self$name <- name
      self$add_neighbors(neighbors)  
    },
  add_neighbors = function(new_neighbors) {
    self$neighbors <- Neighbors$new(c(self$neighbors$agents, new_neighbors))
  },  
  exposure_prob = function() {
    return (.agent_exposure_prob(self))
  },
  set_fitness = function(fitness) {
    
    self$curr_fitness = fitness
    
    invisible(self)
  }
))


Neighbors <- R6Class(classname = "Neighbors", public = list(
  
  # Track neighbor Agents.
  agents = c(),
  
  initialize = function(agents) { 
    self$agents <- agents 
    invisible(self)
  },
  
  # Get neighbor by name.
  get = function(name) { 
    return (self$agents[[name]]) 
  },
  
  # Wrap purrr::map to apply function to all neighbor agents.
  map = function(f) { 
    purrr::map(self$agents, f) 
    invisible(self)
  }
))
