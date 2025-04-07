#' AgentBasedModel class
#'
#' Manages agent initialization, graph structure, and social connections.
#'
#' @export
AgentBasedModel <- R6::R6Class(
  "AgentBasedModel",
  public = list(
    agents = NULL,
    network = NULL,
    
    initialize = function(n_agents = NULL, graph = NULL) {
      if (!is.null(n_agents)) {
        graph <- igraph::make_empty_graph(n_agents, directed = FALSE)
      } else if (is.null(graph)) {
        stop("Must provide either n_agents or graph")
      }
      
      # Ensure graph has names
      if (is.null(igraph::V(graph)$name)) {
        igraph::V(graph)$name <- paste0("a", seq_along(igraph::V(graph)))
      }
      
      self$network <- graph
      
      # Create agents
      self$agents <- purrr::map(igraph::V(graph), \(v) {
        Agent$new(id = as.integer(v),
                  graph = graph,
                  name = v$name)
      })
      
      # Sync agent names back to graph if needed
      for (i in seq_along(self$agents)) {
        agent <- self$agents[[i]]
        if (is.null(agent$name)) {
          agent$name <- paste0("a", i)
        }
        igraph::V(graph)$name[i] <- agent$name
      }
      
      # Assign neighbors
      for (agent in self$agents) {
        neighbor_ids <- igraph::neighbors(graph, agent$id)
        neighbors <- lapply(neighbor_ids, \(v) self$agents[[as.integer(v)]])
        do.call(agent$add_neighbors, neighbors)
      }
    },
    
    get_agent = function(key) {
      if (is.numeric(key)) {
        return(self$agents[[key]])
      } else if (is.character(key)) {
        idx <- which(vapply(self$agents, \(a) a$get_name(), character(1)) == key)
        if (length(idx) == 0)
          stop("No agent found with that name.")
        return(self$agents[[idx]])
      } else {
        stop("key must be numeric or character")
      }
    }
  )
)
