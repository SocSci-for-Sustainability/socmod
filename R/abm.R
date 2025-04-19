#' AgentBasedModel class for socmod
#'
#' This class represents the main simulation container, holding agents and a social network.
#' It ensures synchronization between the agent states and the underlying igraph network.
#'
#' @export
AgentBasedModel <- R6::R6Class(
  "AgentBasedModel",
  public = list(
    agents = NULL,
    graph = NULL,
    
    #' @description Initialize a model with either a graph, agents, or n_agents
    #' @param graph An igraph object (optional)
    #' @param agents A list of Agent objects (optional)
    #' @param n_agents Integer number of agents to create (optional)
    initialize = function(model_parameters = DefaultParameters, agents = NULL) {
      
      # Handle logic of setting ABM graph to user-provided, or create 
      # a fully-connected one with `n_agents` vertices.
      
      # First create a short-name variable for parameters.
      if (!is.null(model_parameters)) {
        assert_that(
          inherits(model_parameters, "ModelParameters"),
          msg = "model_parameters must be an instance of ModelParameters"
        )
      }

      # If the graph is defined, check it's an igraph and read it in.
      graph <- model_parameters$get_graph()
      n_agents <- model_parameters$get_n_agents()
      if (!is.null(graph)) {
        stopifnot(igraph::is_igraph(graph))
        self$graph <- graph

      # If n_agents is defined when the graph is not, make a full graph for agents.
      } else if (!is.null(n_agents)) {

        self$graph <- igraph::make_full_graph(n_agents, directed = FALSE)

        # Update model parameters with newly-created graph.
        model_parameters$set_graph(self$graph)

      } else {
        stop("Either 'graph' or 'n_agents' must be provided.")
      }
      
      # Initialize model parameters, stored as key-value list.
      # private$.parameters <- model_parameters
      
      if (is.null(igraph::V(self$graph)$name)) {
        igraph::V(self$graph)$name <- 
          paste0("a", seq_len(igraph::vcount(self$graph)))
      }
      
      # If agents are provided, make sure names sync
      if (!is.null(agents)) {
        self$agents <- agents
        names(self$agents) <- purrr::map_chr(self$agents, \(a) a$get_name())
        
        graph_names <- igraph::V(self$graph)$name
        agent_names <- names(self$agents)
        
        if (!all(agent_names %in% graph_names)) {
          # Overwrite igraph vertex names to match agent names.
          igraph::V(self$graph)$name <- agent_names  
        }
        
        self$sync_network("neighbors_only")

        model_parameters$set_n_agents(length(agents))

      } else {

        legacy_fitness <- model_parameters$as_list()$legacy_fitness

        if (is.null(legacy_fitness)) {
          legacy_fitness <- 1.0
        }
        
        self$agents <- 
          purrr::map2(
            seq_len(igraph::vcount(self$graph)),
            igraph::V(self$graph)$name,
            \(i, nm) {
              Agent$new(id = i, name = nm, 
                        behavior = "Legacy", fitness = legacy_fitness)
            }
          )
        
        names(self$agents) <- purrr::map_chr(self$agents, \(a) a$get_name())
        
        self$sync_network("to_graph")
        self$sync_network("from_graph")
      }

      # Set the ABM parameters once it contains all parameters.
      self$set_parameters(model_parameters)
    },
    
    #' @description Synchronize agent and network fields
    #' @param direction "to_graph", "from_graph", or "neighbors_only"
    sync_network = function(direction = c("to_graph", "from_graph", "neighbors_only")) {
      direction <- match.arg(direction)
      for (agent in self$agents) {
        vname <- agent$get_name()
        if (direction == "from_graph") {
          igv <- igraph::V(self$graph)
          agent$set_name(igv[vname]$name)
          agent$set_behavior(igv[vname]$behavior_current)
          agent$set_next_behavior(igv[vname]$behavior_next)
          agent$set_fitness(igv[vname]$fitness_current)
          agent$set_next_fitness(igv[vname]$fitness_next)
        } else if (direction == "to_graph") {
          # Ensure graph vertex names match agent names before syncing
          igraph::V(self$graph)$name <- names(self$agents)
          vid <- agent$get_id()
          self$graph <- igraph::set_vertex_attr(self$graph, "behavior_current", index = vid, value = agent$get_behavior())
          self$graph <- igraph::set_vertex_attr(self$graph, "behavior_next",    index = vid, value = agent$get_next_behavior())
          self$graph <- igraph::set_vertex_attr(self$graph, "fitness_current",  index = vid, value = agent$get_fitness())
          self$graph <- igraph::set_vertex_attr(
            self$graph, "fitness_next", 
            index = vid, 
            value = agent$get_next_fitness()
          )
        }
      }
      
      if (direction %in% c("from_graph", "neighbors_only")) {
        for (agent in self$agents) {

          nbr_ids <- igraph::neighbors(self$graph, v = agent$get_name())
          neighbors <- purrr::map(
            nbr_ids,
            \(v) self$get_agent(igraph::V(self$graph)[v]$name)
          )
          agent$set_neighbors(Neighbors$new(neighbors))
        }
      }
    },
    
    #' @description Get the agent associated with a given ID or name
    #' @param key Integer index or character name
    get_agent = function(key) {
      if (is.character(key)) {
        return(self$agents[[key]])
      } else {
        return(self$agents[[key]])
      }
    },
    
    #' @description Return the igraph network (after syncing from agents)
    get_network = function() {
      # Ensure vertex names are consistent before syncing
      if (is.null(igraph::V(self$graph)$name) || 
          !all(names(self$agents) %in% igraph::V(self$graph)$name)) {
        igraph::V(self$graph)$name <- names(self$agents)
      }
      self$sync_network("to_graph")
      return(self$graph)
    },

    #' @description Get the of model parameters
    get_parameters = function() {
      return(private$.parameters)
    },
    
    #' @description Set multiple model parameters
    #' @param params Named list of parameters to set
    set_parameters = function(params) {
      stopifnot(inherits(params, "ModelParameters"))
      private$.parameters <- modifyList(private$.parameters, params$as_list())
    },
    
    #' @description Set a single model parameter
    #' @param key Parameter name
    #' @param value Parameter value
    set_parameter = function(key, value) {
      private$.parameters[[key]] <- value
    },
    
    #' @description Get a single model parameter
    #' @param key Parameter name
    get_parameter = function(key) {
      return(private$.parameters[[key]])
    }
  ),
  
  private = list(
    .parameters = list()
  )
)


#' Initialize an ensemble of agent-based models.
#' 
#' @export
abm_ensemble <- function(model_parameters_by_variable) {
  
}


#' @export
make_abm <- function(model_parameters = DEFAULT_PARAMETERS, agents = NULL) {

  assertthat::assert_that(
    inherits(model_parameters$get_learning_strategy(), "LearningStrategy"),
    msg = "Learning strategy must be an instance of class LearningStrategy."
  )
  
  return (
    AgentBasedModel$new(
      model_parameters = model_parameters,
      agents = agents
    )
  )
}

