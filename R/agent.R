#' Agent class for socmod
#'
#' Represents an individual agent in a social network. Each agent stores behavioral,
#' fitness, and neighbor information. This class is designed to be encapsulated,
#' with all data access via getter/setter methods.
#' 
#' @examples
#' # Create a basic agent with default settings
#' a1 <- Agent$new(1)
#'
#' # Create a named agent with specific behavior and fitness
#' a2 <- Agent$new(id = 1, name = "Mia", behavior = "1", fitness = 0.75)
#'
#' # Access ID and name
#' a2$get_id()
#' a2$get_name()
#'
#' # Check and update behavior
#' a2$get_behavior()
#' a2$set_behavior("0")
#' a2$advance_behavior()
#'
#' # Set and retrieve custom attributes
#' a2$set_attribute("group", "treatment")
#' a2$get_attribute("group")
#'
#' @export
Agent <- R6::R6Class(
  "Agent",
  private = list(
    id = NULL,
    name = NULL,
    behavior_current = NULL,
    behavior_next = NULL,
    fitness_current = NULL,
    fitness_next = NULL,
    neighbors = NULL,
    attributes = list()
  ),
  public = list(
    
    #' @description
    #' Initialize an Agent
    #'
    #' Sets up a new Agent instance with an ID, optional name, initial behavior,
    #' and fitness values. Behavior and fitness are copied into both the current
    #' and next state fields. A Neighbors container is also initialized.
    #'
    #' @param id Integer or string ID for the agent (used internally, not necessarily name)
    #' @param name Optional character name for the agent. Defaults to "a{id}".
    #' @param behavior Initial behavior (character). Default is "0".
    #' @param fitness Initial fitness (numeric). Default is 0.
    initialize = function(id, name = NULL, behavior = "0", fitness = 0) {
      private$id <- id
      self$set_name(name %||% paste0("a", id))
      self$set_behavior(behavior)
      self$set_next_behavior(behavior)
      self$set_fitness(fitness)
      self$set_next_fitness(fitness)
      private$neighbors <- Neighbors$new()
    },
    
    
    #' @description
    #' Get the agent's vertex ID
    get_id = function() {
      private$id
    },
    
    #' @description
    #' Get the agent's name
    get_name = function() {
      private$name
    },
    
    #' @description
    #' Set the agent's name
    #' @param name New name to assign
    set_name = function(name) {
      private$name <- name
    },
    
    #' @description
    #' Get current behavior
    get_behavior = function() {
      private$behavior_current
    },
    
    #' @description
    #' Set current behavior
    #' @param value New current behavior
    set_behavior = function(value) {
      private$behavior_current <- as.character(value)
      invisible (self)
    },
    
    #' @description
    #' Get next behavior
    get_next_behavior = function() {
      private$behavior_next
    },
    
    #' @description
    #' Set next behavior
    #' @param value New next behavior
    set_next_behavior = function(value) {
      private$behavior_next <- as.character(value)
    },
    
    #' @description
    #' Get current fitness
    get_fitness = function() {
      private$fitness_current
    },
    
    #' @description
    #' Set current fitness
    #' @param value Numeric value to assign
    set_fitness = function(value) {
      private$fitness_current <- value
      invisible (self)
    },
    
    #' @description
    #' Get next fitness
    get_next_fitness = function() {
      private$fitness_next
    },
    
    #' @description
    #' Set next fitness
    #' @param value Numeric value to assign
    set_next_fitness = function(value) {
      private$fitness_next <- value
    },
    
    #' @description
    #' Advance to the next behavior
    advance_behavior = function() {
      private$behavior_current <- private$behavior_next
    },
    
    #' @description
    #' Advance to the next fitness
    advance_fitness = function() {
      private$fitness_current <- private$fitness_next
    },
    
    #' @description
    #' Get neighbors object
    get_neighbors = function() {
      private$neighbors
    },
    
    #' @description
    #' Set neighbors object
    #' @param nbrs A Neighbors instance
    set_neighbors = function(nbrs) {
      if (inherits(nbrs, "Neighbors")) {
        private$neighbors <- nbrs
      } else if (is.list(nbrs)) {
        private$neighbors <- Neighbors$new(nbrs)
      } else {
        stop("New neighbors to set must be either a list of Agent instances or a Neighbors instance")
      }
    },
    
    #' @description
    #' Add one or more neighbors
    #' @param ... Agent instances to add as neighbors
    add_neighbors = function(...) {
      private$neighbors$add(...)
    },
    
    #' @description
    #' Remove one or more neighbors
    #' @param ... Agent instances to remove
    remove_neighbors = function(...) {
      private$neighbors$remove(...)
    },
    
    #' @description
    #' Get agent's degree (via neighbors)
    #' @return Integer degree
    degree = function() {
      private$neighbors$length()
    },
    
    #' @description
    #' Set an arbitrary named attribute
    #' @param key Attribute name
    #' @param value Value to assign
    set_attribute = function(key, value) {
      private$attributes[[key]] <- value
    },
    
    #' @description
    #' Get the value of a named attribute
    #' @param key Attribute name
    get_attribute = function(key) {
      private$attributes[[key]]
    },
    
    #' @description
    #' Set multiple attributes
    #' @param attr_list A named list of attributes
    set_attributes = function(attr_list) {
      stopifnot(is.list(attr_list))
      private$attributes <- modifyList(private$attributes, attr_list)
    },
    
    #' @description
    #' Get all custom attributes as a named list
    get_attributes = function() {
      private$attributes
    }
  )
)
