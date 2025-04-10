#' Neighbors class
#'
#' A container for an agent's neighbors, with utilities for iteration, filtering,
#' sampling, and modification.
#'
#' @export
Neighbors <- R6::R6Class(
  "Neighbors",
  public = list(
    agents = NULL,
    
    initialize = function(agents = list()) {
      self$agents <- agents
    },
    
    map = function(f) {
      # Just apply the function without forcing the result to be a specific type.
      # This allows the function to return whatever is appropriate, like a string.
      lapply(self$agents, f)
    },
    
    length = function() {
      length(self$agents)
    },
    
    #' @description
    #' Get a neighbor by index or name
    #'
    #' @param key An integer index or character name of the neighbor
    #' @return An Agent instance
    get = function(key) {
      if (is.numeric(key)) {
        return(self$agents[[key]])
      } else if (is.character(key)) {
        idx <- which(vapply(self$agents, \(a) a$get_name(), character(1)) == key)
        if (length(idx) == 0)
          stop("No neighbor found with that name.")
        return(self$agents[[idx]])
      } else {
        stop("key must be numeric or character")
      }
    },
    
    #' @description
    #' Filter neighbors using a predicate function
    #'
    #' @param predicate A function taking an Agent and returning TRUE or FALSE
    #' @return A new Neighbors object containing only the filtered agents
    #' @examples
    #' \dontrun{
    #' nbrs$filter(\(a) a$get_fitness() > 1)
    #' }
    filter = function(predicate) {
      Neighbors$new(Filter(predicate, self$agents))
    },
    
    #' @description
    #' Sample one or more neighbors
    #'
    #' @param n Number of neighbors to sample
    #' @param replace Logical: sample with replacement?
    #' @param weights Either a numeric vector or a function returning weights for each agent
    #' @return A Neighbors object or a single Agent depending on `n`
    #' @examples \dontrun{
    #' nbrs$sample(2)
    #' nbrs$sample(weights = \(a) a$get_fitness())
    #' }
    sample = function(n = 1, replace = FALSE, weights = NULL) {
      if (is.function(weights)) {
        prob <- vapply(self$agents, \(a) {
          val <- weights(a)
          if (is.numeric(val) && length(val) == 1 && !is.na(val)) val else 0
        }, numeric(1))
      } else if (is.null(weights)) {
        prob <- rep(1, length(self$agents))
      }
      
      # Only keep neighbors with positive probability.
      valid_idxs <- which(prob > 0)
      
      if (length(valid_idxs) < n) {
        stop("Too few neighbors with positive weights to sample from.")
      }
      
      sampled <- base::sample(
        self$agents[valid_idxs],
        size = n,
        replace = replace,
        prob = prob[valid_idxs]
      )
      
      if (n == 1) {
        return(sampled[[1]])
      } else {
        return(Neighbors$new(sampled))
      }
    }
    ,
    
    #' @description
    #' Add one or more agents to the neighbors list
    #'
    #' @param ... Agent objects to add
    #' @examples
    #' \dontrun{
    #' nbrs$add(agent1, agent2)
    #' }
    add = function(...) {
      new_agents <- list(...)
      self$agents <- unique(c(self$agents, new_agents))
    },
    
    #' @description
    #' Remove one or more agents from the neighbors list
    #'
    #' @param ... Agent objects to remove
    #' @examples
    #' \dontrun{
    #' nbrs$remove(agent2)
    #' }
    remove = function(...) {
      to_remove <- list(...)
      self$agents <- self$filter(\(a) ! any(vapply(
        to_remove, \(b) identical(a, b), logical(1)
      )))$agents
    }
  )
)
