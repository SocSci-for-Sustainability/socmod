ModelParameters <- R6::R6Class(
  
  "ModelParameters",
  
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

    set_graph = function(graph) {
      private$.graph <- graph
    },
    
    get_n_agents = function() {
      return (private$.n_agents)
    },

    set_n_agents = function(n_agents) {
      private$.n_agents <- n_agents
    },
    
    get_auxiliary = function() {
      return (private$.auxiliary)
    },

    set_auxiliary = function(params) {
      private$.auxiliary <- params
    },
    
    as_list = function() {
      return (
        modifyList(
          list(
            learning_strategy = self$get_learning_strategy(),
            graph = self$get_graph(),
            n_agents = self$get_n_agents()
          ),
          self$get_auxiliary() 
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
make_model_parameters <- function(learning_strategy = NULL, 
                             graph = NULL,
                             n_agents = NULL,
                             ...)   {
  return (
    ModelParameters$new(learning_strategy, graph, n_agents, list(...))  
  )
}


#' Default parameters to create an agent-based model.
DEFAULT_PARAMETERS <- make_model_parameters(learning_strategy = NULL, 
                                       graph = NULL,
                                       n_agents = 10,
                                       auxiliary = list()) 
