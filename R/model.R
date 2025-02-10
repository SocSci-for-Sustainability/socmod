#' `socmod` implementation of an agent-based model 
#' 
#' @description
#' This class encapsulates the model's agents, the network that structures
#' their interactions, static or dynamic model parameters, model
#' state, and model outputs. 
#' 
#' @export
AgentBasedModel <- R6Class(classname="AgentBasedModel",
  public = list(

    #' @field agents Named list of agents 
    agents = list(), 
    
    #' @field n_agents Number of agents
    n_agents = 0,

    #' @field step Time step integer index
    step = 0,

    #' @field network Social network that structures agent interactions
    network = NULL,

    #' @field params Model parameters for use in partner selection, 
    #' interaction functions, and 
    params = list(),

    #' @field output Model output, which is either initiated or appended to 
    #' depending on if the model has been run yet.
    output = NULL,
    
    #' Create a new agent-based model.
    #' 
    #' @description
    #' Agent-based models may be initialized one of three ways: (1) 
    #' by providing the agents and the social network that structures their
    #' interaction; (2) providing only the social network; or (3) provide
    #' the number of agents only, which will create and add them to a 
    #' complete graph.
    #' 
    #' @param agents List of agents in agent-based model.
    #' @param network igraph Graph instance representing the ABM social network
    #' @param n_agents Number of agents.]
    #' @param ... Additional kwargs that become entries in the AgentBasedModel$params field
    #' 
    #' @return self New agent-based model initialized as specified.
    initialize = 
      function(agents = NULL, network = NULL, n_agents = NULL, ...) {

        # Initialize agents and network if the agents were provided. Do nothing
        # if no agents provided. If agents provided but not network, 
        # make a complete, undirected network.
        if (!is.null(agents)) {

          if (is.null(network)) {
            self$network <- igraph::make_full_graph(length(agents))
          } else {
            self$network <- network
          }
          
          self$agents <- agents
          self$n_agents <- length(agents)
          
          names(self$agents) <- purrr::map_vec(1:self$n_agents, \(a_idx) { 
            agent <- agents[[a_idx]]
            
            if (is.null(agent$name)) {
              agent$name <- a_idx
              
              return (a_idx)
            } else {
              return (agent$name)
            }
          })
          
          a_idx <- 1
          if (is.null(V(network)$name)) {
            igraph::V(network)$name <- names(self$agents)
          }
          for (agent in self$agents) {
            
            net_neighbors <- neighbors(network, agent$name)
            
            agent_neighbors <- purrr::map(net_neighbors, 
                                          \(n) { self$get_agent(n$name) })
            
            agent$add_neighbors(agent_neighbors)
            
            a_idx <- a_idx + 1
          }


        } else if (!is.null(network)) {
        # Now dealing with the case where `agents` is null, but network not.
            
            if (is.null(igraph::V(network)$name)) {
              igraph::V(network)$name <- seq(1, length(igraph::V(network)))
            }
            
            self$network <- network
            # Create new agents defaulting to Legacy behavior on init.
            self$add_agents(
              purrr::map(
                V(network), \(n) { 
                  Agent$new("Legacy", name=n$name)
                }
              )
            )
          
          names <- V(network)$name
          
          if (is.null(names)) {
            names(self$agents) <- 1:length(self$agents)
          } else {
            names(self$agents) <- names  
          }
          
          for (agent in self$agents) {
            net_neighbors <- neighbors(network, agent$name)
            agent_neighbors <- purrr::map(net_neighbors, 
                                          \(n) { self$get_agent(n$name) })
            agent$add_neighbors(agent_neighbors)
          }

        # Finally deal with the case where only number of agents is provided. 
        }  else if (!is.null(n_agents)) {
          # Initialize default complete network.
          self$network <- igraph::make_full_graph(n_agents)
          
          # Initialize agents.
          self$add_agents(
            purrr::map(
              1:n_agents, \(ii) { 
                Agent$new("Legacy", name=ii)
              }
            )
          )
           
          # After all agents added to the model, set network neighbor agents.
          for (agent in self$agents) { 
            agent$add_neighbors(
              purrr::map(igraph::neighbors(self$network, agent$name), 
                  \(n) { self$get_agent(n) })
            )
          }
        }

        # Keyword arguments in ... become model parameters in named list.
        self$params = list(...)

        invisible(self)
      },

    #' Add agents to the model.
    #' 
    #' @param agents_to_add Insert a list of new agents into list of existing agents.
    add_agents = function(agents_to_add) {
      self$agents <- unlist(c(self$agents, agents_to_add))
      invisible(self)
    },

    #' Get an agent from the model by name.
    #' 
    #' @param name Name of agent, usually an integer or string
    get_agent = function(name) {
      return (self$agents[[name]])
    }

  )
)
