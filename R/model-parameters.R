ModelParameters <- R6::R6Class(
  
  "model-parameters",
  
  public = list(
    initialize = function(learning_strategy = success_biased_strategy, 
                          graph = NULL, n_agents = NULL,
                          auxiliary = list()) {
      private$.learning_strategy <- learning_strategy
      private$.graph <- graph
      private$.n_agents <- n_agents
      private$.auxiliary <- auxiliary
    },
    
    get_learning_strategy = function() {
      return (private$.learning_strategy)
    },
    
    get_graph = function() {
      return (private$.graph)
    },
    
    get_n_agents = function() {
      return (private$.n_agents)
    },
    
    get_auxiliary = function() {
      return (private$.auxiliary)
    },
    
    as_list = function() {
      return (list(
        learning_strategy = self$get_learning_strategy(),
        graph = self$get_graph(),
        n_agents = self$get_n_agents(),
        auxiliary = self$get_auxiliary() # <- a list itself, so we're concatenating all.
      ))
    }
  ),
  
  private = list(
    .learning_strategy = NULL,
    .graph = NULL,
    .n_agents = NULL,
    .auxiliary = list()
  )
)


#' Wrapper for initializing new ModelParameters instance.
#' @export
model_parameters <- function(learning_strategy = NULL, 
                             graph = NULL,
                             n_agents = NULL,
                             ...)   {
  auxiliary <- list(...)
  return (
    ModelParameters$new(learning_strategy, graph, n_agents, auxiliary)  
  )
}


#' Default parameters to create an agent-based model.
DEFAULT_PARAMETERS <- model_parameters(learning_strategy = NULL, 
                                       graph = NULL,
                                       n_agents = 10,
                                       auxiliary = list()) 
