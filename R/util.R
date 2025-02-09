##
# Computational social science modeling tools focused on social behavior.
#
# Author: Matthew A. Turner <maturner01@gmail.com>
# Date: 2025-01-29


# Behavior <- R6Class(classname = "Behavior", public = list(
#   payoff = NA,
#   initialize = function(payoff = 1.0) {self$payoff = payoff}
# ))





#' Create a regular lattice graph.
#'
#' @description
#' Adapted from 
#' https://github.com/USCCANA/netdiffuseR/blob/1efc0be4539d23ab800187c73551624834038e00/src/rgraph.cpp#L90
#' Difference here is we'll only use undirected for now, so need to adjust by 
#' default (see also NetLogo routine in Smaldino Ch. 9 p. 266). 
#' Beacaus igraph is flexible, it will add duplicate edges, so we have to check
#' to make sure an edge does not exist between two nodes before adding it, using
#' the `igraph::are_adjacent` function ("adjacent" means there is an edge between two
#' nodes in an undirected graph–in a directed graph the definition is subjective,
#' i.e., v1 and v2 are sometimes defined as adjacent if there's an edge from
#' v1 to v2, and others define adjacency as an edge from v2 to v1). 
#' 
#' @param N number of nodes
#' @param k node degree
#' @param directed Whether the graph should be directed
#' @examples 
#' # Make a 10-node lattice with nodes degree 4.
#' net <- regular_lattice(10, 4)
#' plot(net)
#' @return igraph Graph
#' @export
regular_lattice <- function(N, k, directed = FALSE) {

  # Check that lattice parameters satisfy listed conditions below.
  assert_that(N - 1 >= k, msg = "Lattice degree, k, can be at most N-1.")
  assert_that(k %% 2 == 0, msg = "Lattice degree, k, must be even.")
  assert_that(!directed, msg = "Directed lattice not yet implemented")

  # Initialize an empty graph to which we add edges.
  ret_lattice <- igraph::make_empty_graph(N, directed = directed)

  # Iterate over all agents, making links between k/2 neighbors with lesser
  # agent_idx and k/2 neighbors with greater agent_idx.
  k_per_side <- as.integer(k/2)
  for (a_idx in 1:N) {
    for (jj in 1:k_per_side) {

      # The neighbor on the first side.
      neighbor_side_1 <- a_idx + jj 
      if (neighbor_side_1 > N) {
        neighbor_side_1 <- neighbor_side_1 - N
      }

      # The neighbor on the second side.
      neighbor_side_2 <- a_idx - jj
      if (neighbor_side_2 <= 0) {
        neighbor_side_2 <- neighbor_side_2 + N
      }

      # Add first edge if not already present.
      ret_lattice <- add_unique_edge(ret_lattice, a_idx, neighbor_side_1)
      # Add second edge if not already present.
      ret_lattice <- add_unique_edge(ret_lattice, a_idx, neighbor_side_2)
    }
  }

  return (ret_lattice)
}


#' Check two vertices are not adjacent.
#'
#' @param g Graph
#' @param v1 Vertex/agent/node 1
#' @param v2 Vertex/agent/node 2
#' @return bool indicating whether two vertices v1 and v2 are *not* adjacent in g
#' @export
not_adjacent <- function(g, v1, v2) { 
  return (!igraph::are_adjacent(g, v1, v2)) 
}


#' Add an undirected edge from v1 to v2 to graph g if it does not already exist.
#'
#' @param g Graph representing social network
#' @param v1 First node in edge pair
#' @param v2 Second node in edge pair
#'
#' @examples 
#' # Add one unique edge between nodes 1 and 4 to empty ten-node network
#' g <- igraph::make_empty_graph(n = 10)
#' g <- add_unique_edge(g, 1, 4)
#' 
#' @return igraph Graph
#' @export
add_unique_edge <- function(g, v1, v2) {
  
  if (not_adjacent(g, v1, v2)) {
      g <- igraph::add_edges(g, c(v1, v2))
  }

  return (g)
}

#' Erdős-Rényi random graph G(N, M). 
#'
#' @param N number of nodes/agents
#' @param M number of edges to be randomly assigned 
#' @examples 
#' # Create a 10-node network with 10 randomly-assigned edges
#' library(igraph)
#' net <- G_NM(10, 10)
#' plot(net)
#' 
#' @export
#' @return igraph Graph instance
G_NM <- function(N, M) {

  g <- make_empty_graph(n = N, directed = FALSE)

  all_edges <- get_all_possible_edges(N) 
  
  edges <- all_edges[sample(1:nrow(all_edges), M), ]

  for (row_idx in 1:nrow(edges)) {
    row <- edges[row_idx, ]
    v1 <- row$v1
    v2 <- row$v2

    g <- add_edges(g, c(v1, v2))
  }

  return (g)
}


# TODO: 
# Erdős-Rényi random graph G(N, p). 
# See Smaldino (2023) *Modeling Social Behavior* p. 267.


#' Get all possible edges between node indices 1 to N for either directed or 
#' undirected networks.
#'
#' @param N number of nodes
#' @param directed Whether or not the possible edges are for directed graphs
#' @examples 
#' # Get a table of vertex pairs representing possible edges with ten vertices.
#' Epossible <- get_all_possible_edges(10)
#' 
#' @export
#' @return table of node pairs representing edges
get_all_possible_edges <- function(N, directed = FALSE) {

  vert_idxs <- 1:N
  if (directed)
    return (expand.grid(v1 = vert_idxs, v2 = vert_idxs) %>% dplyr::filter(v1 != v2))
  else
    return (
      bind_rows(
        map(1:(N-1), \(ii) {tibble(v1 = vert_idxs[1:(N-ii)], v2 = vert_idxs[(1+ii):N])})
      )
    )
}





