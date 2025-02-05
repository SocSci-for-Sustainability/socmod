library(R6, quietly = TRUE)
library(purrr, include.only = "map", quietly = TRUE)


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
      return (self$agents[[name]])
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
            agents[[agent_idx]]$neighbors <- 
              igraph::neighbors(self$network, agent_idx)
          }

          self$agents <- agents
        } else if (!is.null(network)) {
        # Now dealing with the case where `agents` is null, but network not.

            # Create new agents defaulting to Legacy behavior on init.
            self$agents <- 
              purrr::map(
                V(network)$name, \(n) { 
                  Agent$new("Legacy", 
                            name=n, 
                            neighbors=neighbors(network, n))
                }
              )

            # Make agents into named list so graph names can look up agents.
            names(self$agents) <- V(network)$name
        }
        # Finally deal with the case where only number of agents is provided.
        else if (!is.null(n_agents)) {
          
          # Initialize default complete network.
          self$network <- igraph::make_full_graph(n_agents)
          
          # Initialize agents.
          self$add_agents(
            map(
              1:n_agents, \(ii) { 
                Agent$new("DEFAULT", name=ii)
              }
            ))
           
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
  ), 
  private = list()
)
