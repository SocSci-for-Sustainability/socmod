#' Agent class for socmod
#'
#' Represents an individual agent in a social network. Each agent has access to
#' a shared igraph-based network object and exposes methods for interacting with
#' behavioral and fitness attributes, social connections, and graph metadata.
#'
#' Advanced note: This class uses igraph's custom `vertex_attr(...) <<- value` setter
#' syntax internally to ensure robust and consistent assignment of dynamic vertex
#' attributes. This detail is abstracted away from the user-facing API.
#'
#' @field id The internal vertex ID of the agent in the igraph object
#' @field graph The igraph network shared across all agents
#' @field name A unique string name for the agent
#' @field neighbors A Neighbors object managing local social connections
#'
#' @export
Agent <- R6::R6Class(
  "Agent",
  public = list(
    id = NULL,
    graph = NULL,
    name = NULL,
    neighbors = NULL,
    
    #' @description
    #' Initialize an Agent
    #' @param id The vertex ID in the graph
    #' @param graph An igraph object shared by all agents
    #' @param name Optional string name for the agent
    initialize = function(id, graph, name = NULL) {
      stopifnot(igraph::is.igraph(graph))
      self$id <- id
      self$graph <- graph
      self$name <- name
      self$neighbors <- Neighbors$new()
    },
    
    #' @description
    #' Get the agent's name
    #' @return A character string representing the agent's name
    get_name = function() {
      self$name
    },
    
    #' @description
    #' Set the agent's name
    #' @param value A string name
    set_name = function(value) {
      self$name <- value
    },
    
    #' @description
    #' Get the current behavior
    #' @return A string representing the agent's current behavior
    get_behavior = function() {
      igraph::vertex_attr(self$graph, "behavior_current", index = self$id)
    },
    
    #' @description
    #' Set the current behavior
    #' @param value A string behavior type
    set_behavior = function(value) {
      self$set_attr("behavior_current", value)
    },
    
    #' @description
    #' Get the next behavior
    #' @return A string representing the agent's next behavior
    get_next_behavior = function() {
      self$get_attr("behavior_next")
    },
    
    #' Set the next behavior
    #' @param value A string behavior type
    set_next_behavior = function(value) {
      igraph::set_vertex_attr(self$graph, name = "behavior_next", 
                              index = self$id, value = value)
    },
    
    #' @description
    #' Advance to the next behavior
    advance_behavior = function() {
      self$set_behavior(self$get_next_behavior())
    },
    
    #' @description
    #' Get the current fitness value
    #' @return A numeric fitness value
    get_fitness = function() {
      igraph::vertex_attr(self$graph, "fitness_current", index = self$id)
    },
    
    #' @description
    #' Set the current fitness
    #' @param value A numeric fitness score
    set_fitness = function(value) {
      self$set_vertex_attr(self$graph, name = "fitness_current", 
                           index = self$id, value = value)
    },
    
    #' @description
    #' Get the next fitness value
    #' @return A numeric fitness value
    get_next_fitness = function() {
      self$get_attr("fitness_next")
    },
    
    #' @description
    #' Set the next fitness value
    #' @param value A numeric fitness score
    set_next_fitness = function(value) {
      igraph::set_vertex_attr(self$graph, name = "fitness_next", 
                              index = self$id, value = value)
    },
    
    #' @description
    #' Advance to the next fitness value
    advance_fitness = function() {
      self$set_fitness(self$get_next_fitness())
    },
    
    #' @description
    #' Get a graph vertex attribute by name
    #' @param name Name of the attribute
    #' @return The value of the attribute
    get_attr = function(name) {
      igraph::vertex_attr(self$graph, name, index = self$id)
    },
    
    #' @description
    #' Set a graph vertex attribute
    #' @param name Name of the attribute
    #' @param value Value to assign
    set_attr = function(name, value) {
      igraph::set_vertex_attr(self$graph, name, index = self$id, value)
    },
    
    #' @description
    #' Add one or more neighbors
    #' @param ... Agent instances to add as neighbors
    add_neighbors = function(...) {
      if (is.null(self$neighbors))
        self$neighbors <- Neighbors$new()
      self$neighbors$add(...)
    },
    
    #' @description
    #' Remove one or more neighbors
    #' @param ... Agent instances to remove from neighbors
    remove_neighbors = function(...) {
      if (!is.null(self$neighbors)) {
        self$neighbors$remove(...)
      }
    },
    
    #' @description
    #' Get the degree of the agent in the graph
    #' @param mode Degree mode passed to igraph::degree()
    #' @return Integer degree count
    degree = function(mode = "all") {
      igraph::degree(self$graph, v = self$id, mode = mode)
    },
    
    #' @description Compute the probability of exposure to Adaptive behavior
    #' @return A numeric value between 0 and 1
    exposure_prob = function() {
      if (is.null(self$neighbors) || self$neighbors$length() == 0) return(0)
      
      adaptive <- self$neighbors$filter(\(a) a$get_behavior() == "Adaptive")
      adaptive$length() / self$neighbors$length()
    }
  )
)
