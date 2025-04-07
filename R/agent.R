#' Agent class for socmod
#'
#' Represents an individual agent in a social network with cognitive and behavioral attributes.
#'
#' @export
Agent <- R6::R6Class(
  "Agent",
  public = list(
    id = NULL,
    graph = NULL,
    name = NULL,
    neighbors = NULL,
    
    initialize = function(id, graph, name = NULL) {
      stopifnot(igraph::is.igraph(graph))
      self$id <- id
      self$graph <- graph
      self$name <- name
      self$neighbors <- Neighbors$new()
    },
    
    get_name = function() {
      self$name
    },
    
    set_name = function(value) {
      self$name <- value
    },
    
    get_behavior = function() {
      igraph::vertex_attr(self$graph, "behavior_current", index = self$id)
    },
    
    set_next_behavior = function(value) {
      igraph::vertex_attr(self$graph, "behavior_next", index = self$id) <<- value
    },
    
    advance_behavior = function() {
      next_val <- self$get_attr("behavior_next")
      self$set_attr("behavior_current", next_val)
    },
    
    get_fitness = function() {
      igraph::vertex_attr(self$graph, "fitness_current", index = self$id)
    },
    
    set_next_fitness = function(value) {
      igraph::vertex_attr(self$graph, "fitness_next", index = self$id) <<- value
    },
    
    advance_fitness = function() {
      next_val <- self$get_attr("fitness_next")
      self$set_attr("fitness_current", next_val)
    },
    
    get_attr = function(name) {
      igraph::vertex_attr(self$graph, name, index = self$id)
    },
    
    set_attr = function(name, value) {
      igraph::vertex_attr(self$graph, name, index = self$id) <<- value
    },
    
    add_neighbors = function(...) {
      if (is.null(self$neighbors))
        self$neighbors <- Neighbors$new()
      self$neighbors$add(...)
    },
    
    remove_neighbors = function(...) {
      if (!is.null(self$neighbors)) {
        self$neighbors$remove(...)
      }
    },
    
    degree = function(mode = "all") {
      igraph::degree(self$graph, v = self$id, mode = mode)
    }
  )
)
