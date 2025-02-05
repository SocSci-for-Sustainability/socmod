library(dplyr)
library(R6, quietly = TRUE)
library(purrr, include.only = "map", quietly = TRUE)

#' @export
AgentBasedModel <- R6Class(classname="AgentBasedModel",
  public = list(
    agents = c(), 
    step = 0,
    network = NULL,
    params = list(),
    output = NULL,
    add_agents = function(agents_to_add) {
      self$agents <- unlist(c(self$agents, agents_to_add))
      invisible(self)
    },                    
    get_agent = function(name) {
      invisible (self$agents[[name]])
    },
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

          for (agent_idx in 1:length(agents)) {
            agents[[agent_idx]]$add_neighbors(
              igraph::neighbors(self$network, agent_idx)
            )
          }

          self$agents <- agents
        } else if (!is.null(network)) {
        # Now dealing with the case where `agents` is null, but network not.
            self$network <- network
            
            # Create new agents defaulting to Legacy behavior on init.
            self$add_agents( 
              map(
                V(network), \(n) { 
                  Agent$new("Legacy", name=n$name)
                }
              )
            )
          # Neighbors$new(neighbors(network, n))
          names(self$agents) <- V(network)$name
          for (agent in self$agents) {
            net_neighbors <- neighbors(network, agent$name)
            agent_neighbors <- map(net_neighbors, \(n) { self$get_agent(n$name) })
            agent$add_neighbors(agent_neighbors)
          }
            
            # map(self$agents, \(a) { a$add_neighbors()

            # Make agents into named list so graph names can look up agents.
            
            

        # Finally deal with the case where only number of agents is provided. 
        }  else if (!is.null(n_agents)) {
          # Initialize default complete network.
          self$network <- igraph::make_full_graph(n_agents)
          
          # Initialize agents.
          self$add_agents(
            map(
              1:n_agents, \(ii) { 
                Agent$new("Legacy", name=ii)
              }
            )
          )
           
          # After all agents added to the model, set network neighbor agents.
          for (agent in self$agents) { 
            agent$add_neighbors(
              map(neighbors(self$network, agent$name), 
                  \(n) { self$get_agent(n) })
            )
          }
        }

        # Keyword arguments in ... become model parameters in named list.
        self$params = list(...)

        invisible(self)
      }
  )
)
