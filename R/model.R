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
    output = NULL,
    initialize = function(n_agents = NULL, graph = NULL) {
      if (!is.null(n_agents)) {
        graph <- igraph::make_full_graph(n_agents, directed = FALSE)
      } else if (is.null(graph)) {
        stop("Must provide either n_agents or graph")
      }
      
      # Ensure graph has names
      if (is.null(igraph::V(graph)$name)) {
        igraph::V(graph)$name <- paste0("a", seq_along(igraph::V(graph)))
      }
      
      self$network <- graph
      
      # Create agents
      self$agents <- purrr::map(seq_along(igraph::V(graph)), \(i) {
        v <- igraph::V(graph)[i]
        Agent$new(id = i, graph = graph, name = v$name)
      })
      
      # Sync agent names back to graph
      for (i in seq_along(self$agents)) {
        agent <- self$agents[[i]]
        igraph::V(graph)$name[i] <- agent$name
      }
      
      # Assign neighbors
      for (agent in self$agents) {
        neighbor_vertices <- igraph::neighbors(graph, agent$id)
        neighbor_ids <- as.integer(neighbor_vertices)
        neighbors <- lapply(neighbor_ids, \(idx) self$agents[[idx]])
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
