#' Create a regular lattice graph.
#'
#' @description
#' Adapted from 
#' https://github.com/USCCANA/netdiffuseR/blob/1efc0be4539d23ab800187c73551624834038e00/src/rgraph.cpp#L90
#' Difference here is we'll only use undirected for now, so need to adjust by 
#' default (see also NetLogo routine in Smaldino Ch. 9 p. 266). 
#' Because igraph is flexible, it will add duplicate edges, so we have to check
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
#' net <- make_regular_lattice(10, 4)
#' plot(net)
#' @return igraph Graph
#' @export
make_regular_lattice <- function(N, k, directed = FALSE) {
  
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
  
  selected_edges <- 1:M %>% 
    map_dfr(\(idxs){ 
      verts <- sort(sample(1:N, 2))
      return (tibble(v1 = verts[1], v2 = verts[2]))
    })
  
  dup_edge_rows <- which(duplicated(selected_edges))
  n_prev_to_replace <- length(dup_edge_rows)
  
  for (rr in dup_edge_rows) {
    # Initialize check if new random edge already exists in tbl...
    edge_exists <- TRUE
    # ...and keep going until an edge that doesn't already exist is found.
    while (edge_exists) {
      # Try adding a row...
      selected_edges[rr, ] <- as.list(sample(1:N, 2))
      # ...which we keep if the 
      n_new_to_replace <- length(which(duplicated(selected_edges)))
      if (n_new_to_replace == (n_prev_to_replace - 1)) {
        edge_exists <- FALSE
      }
      n_prev_to_replace <- n_new_to_replace
    }    
  }
  
  return (
    make_empty_graph(N, directed = FALSE) %>%
        add_edges(
          purrr::reduce(t(selected_edges), \(v1, v2) { c(v1, v2) })
        )
  )
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
#' 
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

#' Make a small world network.
#'
#' @description
#' Create a small-world network by rewiring a lattice of size N, degree k.
#' 
#' @param N Population size
#' @param k Seed lattice degree
#' @param p Rewire probability
#' 
#' @export
#'
#' @return igraph::graph
make_small_world <- function(N, k, p) {
  return (
    igraph::rewire(make_regular_lattice(N, k), igraph::each_edge(p))
  )
}


#' Make a preferential attachment network.
#'
#' @description
#' Make a simple preferential attachment network with N nodes, starting from
#' two nodes and adding one node per network construction step.
#' 
#' @param N population size
#'
#' @export
#' @return igraph::graph
make_preferential_attachment <- function(N) {
  
  pa_net <- make_empty_graph(2, directed = FALSE)
  pa_net <- add_edges(pa_net, c(1, 2))
  for (next_idx in 3:N) {
    
    # The chance existing vertices connect to the new one is proportional to degree.
    connect_weights <- degree(pa_net)
    
    # Weighted random selection of neighbor (vertex index). Sample normalizes for us.
    neigh_idx <- sample(1:(next_idx - 1), 1, prob = connect_weights)
    
    pa_net <- add_vertices(pa_net, 1)
    pa_net <- add_edges(pa_net, c(next_idx, neigh_idx))
  }
  
  return (pa_net)
}


#' Command that operates like ggplot
#'
#' @param net Network to plot
#' @param layout Optional igraph-style plotting layout 
#' @param ... Additional key-value aesthetics to pass to aes() in ggplot call
#'
#' @returns ggplot()-like command for initiating ggplot2 network plots
#' 
#' @export
#'
#' @examples
#' library(ggnetwork)
#' socnet <- igraph::make_graph(~ 1-2:3:4,2:4)
#' igraph::V(socnet)$name <- c("Mateus", "Marcos", "Lucas", "João")
#' ggnetplot(socnet) + 
#'   geom_edges(linewidth=0.1) + 
#'   geom_nodes(color = "#008566", size=3) + 
#'   geom_nodelabel_repel(aes(label = name), size = 1.5) + 
#'   theme_blank()
ggnetplot <- function(net, layout = NULL, ...) {
  
  assert_that(
    inherits(net, "igraph"),
    msg = "net must be an igraph object"
  )
  
  if (is.null(layout)) {
    ret <- ggplot(ggnetwork(net), aes(x=x, y=y, xend=xend, yend=yend, ...))
  } else {
    ret <- ggplot(ggnetwork(net, layout = layout(net)), 
                  aes(x=x, y=y, xend=xend, yend=yend, ...))
  }

  return (ret)
}


#' Plot homophily network where groups color nodes.
#'
#' @param net Homophily network to plot.
#' @param node_size Node size in plot.
#' @param line_width Edge line width.
#' @param theme_base_size Base font size to pass to ggplot theme.
#'
#' @returns ggplot 
#' @export
#'
#' @examples
#' hnet_5grp <- make_homophily_network(rep(5, 5), mean_degree = 2, homophily = -0.5)
#' plot_homophilynet(hnet_5grp)
plot_homophilynet <- function(net, node_size = 3, line_width = 0.1, theme_base_size = 10) {
  ggnetplot(net) + 
    geom_edges(color = "grey", linewidth = line_width) + 
    geom_nodes(aes(color = group), size = node_size) + 
    theme_blank(base_size = theme_base_size)
}


#' Create an undirected asymmetric homophily network. 
#' 
#' @description
#' Creates a network with an arbitrary number of groups of arbitrary size
#' with arbitrary homophily levels. Homophily can take values from -1 (totally
#' anti-homophilous) to +1 (totally homophilous), and 0 indicates equal 
#' probability of connecting within group as between groups. The algorithm builds the
#' network by first assigning all within-group edges.
#' @param group_sizes The population (size) of each group
#' @param mean_degree Desired mean degree
#' @param homophily Singleton or vector; if vector must be length of group_sizes
#' @param group_names Optional parameter to specify group names
#' @param add_to_complete Boolean to specify whether to complete the network if there's only one group left needing out-edges
#' @examples
#' # Two groups size 5 and 10.
#' hnet_2grp <- make_homophily_network(c(5, 10), mean_degree = 3, homophily = 0.5)
#' plot_homophilynet(hnet_2grp)
#' 
#' # Five groups all size 5 with out-group preference (neg. homophily).
#' hnet_5grp <- make_homophily_network(rep(5, 5), mean_degree = 2, homophily = -0.5)
#' plot_homophilynet(hnet_5grp)
#' @return igraph Graph
#' @export
make_homophily_network <- function(group_sizes = c(3, 7), 
                                   mean_degree = 2,
                                   homophily = c(0.0), 
                                   group_names = NULL,
                                   add_to_complete = FALSE) {
  
  N <- sum(group_sizes)
  
  assert_that(
    (length(homophily) == 1) || (length(homophily) == length(group_sizes)), 
    msg = 
      "Homophily must be singleton or of the same length as the number of groups"
  )
  
  assert_that(mean_degree < N, msg = "Mean degree can be at most N - 1")
  
  assert_that(all(-1 <= homophily) && all(homophily <= 1), 
              msg = "Homophily must be between -1 and 1")
  
  net <- make_empty_graph(N, directed = FALSE)
  
  if (is.null(group_names)) {
    group_names <- map_vec(1:length(group_sizes), as.factor)
  } else {
    group_names <- map_vec(group_names, as.factor)
  }
  
  a_idx = 1
  g_idx = 1
  for (group_size in group_sizes) {
    final_a_idx <- a_idx + group_size - 1
    igraph::V(net)[a_idx:final_a_idx]$group <- group_names[g_idx]
    g_idx <- g_idx + 1
    a_idx <- final_a_idx + 1
  }
  
  # The number of edges per group is the group size times user-specified mean degree.
  edges_per_group <- (group_sizes * mean_degree)
  
  # Scale by homophily to get edges per group, divided by two since each edge
  # adds additional degree per connected node and rounded for a whole number.
  edges_within_per_group <- round(edges_per_group * ((1 + homophily)/2) * 0.5)

  # The number of between-group edges starting from each group.
  edges_between_per_group <- edges_per_group - edges_within_per_group
  
  # Add in-group edges.
  n_groups = length(group_sizes)
  rm(g_idx)
  for (g_idx in 1:n_groups) {
    
    n_edges <- edges_within_per_group[g_idx]
    
    in_vertices <- V(net)[V(net)$group == group_names[g_idx]]
    
    for (e_idx in 1:n_edges) {
    
      # Draw new vertices and add to graph if they are not already connected.
      edge_exists <- TRUE 
      while (edge_exists) {
        edge_verts <- sample(in_vertices, 2, replace = FALSE)
        edge_exists <- are_adjacent(net, edge_verts[1], edge_verts[2])
      }
      
      net <- add_edges(net, edge_verts)
  
      # Need to re-fetch these since the graph "changed". 
      # (CHANGED: now trying to just use the names, so only have to look up )
      in_vertices <- V(net)[V(net)$group == group_names[g_idx]]
    }
  }
  
  # Now add all between-group edges, selecting vertices from groups at random
  # for building the edges biased by remaining between-group edges to add.
  edges_remain <- TRUE

  while (edges_remain) {
    
    # Check if there's only one group lacking between-group edges.
    if (sum(edges_between_per_group > 0) == 1) {
      
      last_group <- which(edges_between_per_group > 0)
      other_groups <- group_names[group_names != last_group]
      
      edges_left <- edges_between_per_group[last_group]
      if (add_to_complete) {
        
        for (edge_idx in 1:edges_left) {
          # If there is more than one edge left, select two vertices
          # from the group with edges remaining.
          if (edge_idx - edges_left > 1) {
            # Select two from group needing edges.
            needing_group_verts <- V(net)[V(net)$group == last_group]
            # Select a group not needing edge at random...
            # ...and select two vertices to connect to and create edges.
            outgroup <- sample(other_groups, 1)
            outgroup_vertices <- V(net)[V(net)$group == outgroup]
            ovs <- sample(outgroup_vertices, 2)
            nvs <- sample(needing_group_verts, 2)
            
            # Heads-up that this may not create an edge, but 
            # ignore it if it doesn't work.
            net <- add_unique_edges(net, ovs[1], nvs[1])
            net <- add_unique_edges(net, ovs[2], ovs[2])
            
            # Then to keep homophily the same in the `outgroup`,
            # add a random edge within that group (one edge in-group increases)
            # total group degree by two. We use add_unique_edges
            # so if the edge already exists nothing will happen.
            ognewv <- sample(outpgroup_vertices, 2)
            add_unique_edges(net, ognewv)
            
          } else {
            
            needing_group_verts <- 
              V(net)[V(net)$group == last_group]
            
            # Select a group not needing edge at random...
            # ...and select two vertices to connect to and create edges.
            outgroup <- sample(other_groups, 1)
            outgroup_vertices <- V(net)[V(net)$group == outgroup]
            ovs <- sample(outgroup_vertices, 1)
            nvs <- sample(needing_group_verts, 1)
            add_unique_edge(net, ovs, nvs)
          }
        }
        
      }
      
      edges_remain <- FALSE
    
    } else {
      # If more than one group needs between edges, sample and add new edge.
      groups_for_adding <- sample(group_names, 2, prob = edges_between_per_group)
      group1 <- groups_for_adding[1]
      group2 <- groups_for_adding[2]
      
      group1verts <- V(net)[V(net)$group == group1]
      group2verts <- V(net)[V(net)$group == group2]
      # Draw new vertices and add to graph if they are not already connected.
      edge_exists <- TRUE 
      while (edge_exists) {
        new_edge_verts <- c(sample(group1verts, 1), 
                            sample(group2verts, 1))
        edge_exists <- are_adjacent(net, new_edge_verts[1], new_edge_verts[2])
      }
      
      net <- add_edges(net, new_edge_verts)
      
      # Reduce remaining edges between per group for the two selected groups.
      edges_between_per_group[group1] = edges_between_per_group[group1] - 1
      edges_between_per_group[group2] = edges_between_per_group[group2] - 1
      
      edges_remain <- sum(edges_between_per_group) > 0 
    }
  }
  
  return (net)
}


#' Simulate Feld's 1991 Marketville Network
#'
#' Creates an undirected network using the degree distribution from
#' Feld (1991) based on the "Marketville" high school data. Uses
#' the configuration model to generate the network structure.
#'
#' The friendship paradox—where most individuals have fewer friends
#' than their friends do—emerges naturally due to degree heterogeneity
#' in this network.
#'
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#'
#' @return An igraph object representing the simulated network.
#'
#' @examples
#' # Generate the network
#' g <- simulate_feld_1991(seed = 42)
#'
#' # Plot degree vs. mean degree of friends to show the paradox
#' deg <- igraph::degree(g)
#' mean_neighbor_deg <- sapply(igraph::V(g), function(v) {
#'   nbs <- igraph::neighbors(g, v)
#'   if (length(nbs) == 0) return(NA)
#'   mean(igraph::degree(g, v = nbs))
#' })
#'
#' plot(deg, mean_neighbor_deg,
#'      xlab = "Individual's Friends",
#'      ylab = "Mean Friends of Friends")
#' abline(0, 1, col = "red")
#'
#' # Proportion of individuals with fewer friends than their friends
#' mean(deg < mean_neighbor_deg, na.rm = TRUE)
#'
#' @export
simulate_feld_1991 <- function(seed = 42) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Please install the 'igraph' package to use this function.")
  }
  
  # Degree distribution from Feld (1991), Fig. 3a
  degree_dist <- data.frame(
    degree = 1:7,
    count = c(20, 29, 35, 20, 14, 20, 8)
  )
  
  # Create degree sequence
  degrees <- rep(degree_dist$degree, degree_dist$count)
  
  # Generate network
  set.seed(seed)
  g <- igraph::sample_degseq(degrees, method = "simple.no.multiple")
  
  return(g)
}

#' Compare Friendship Paradox in a Network
#'
#' For each node, compares the number of friends (degree) to the mean number
#' of friends among their neighbors. Calculates the proportion of nodes
#' that have fewer friends than the average of their friends (the friendship paradox).
#'
#' Supports both `igraph` and `tidygraph::tbl_graph` inputs.
#'
#' @param graph An `igraph` or `tidygraph::tbl_graph` object representing an undirected network.
#' @param return_node_data Logical. If TRUE, includes a data frame with node-level results.
#'
#' @return A list with:
#'   - `paradox_proportion`: Proportion of nodes experiencing the friendship paradox
#'   - `summary`: A data frame with average degree and average neighbor degree
#'   - `nodes`: (Optional) A data frame with node-level metrics if `return_node_data = TRUE`
#'
#' @examples
#' \dontrun{
#' # Use with an igraph network
#' library(igraph)
#' g_ig <- simulate_feld_1991()
#' compare_friendship_paradox(g_ig)
#'
#' # Return node-level metrics too
#' result2 <- compare_friendship_paradox(g, return_node_data = TRUE)
#' head(result2$nodes)
#' }
#' @export
compare_friendship_paradox <- function(graph, return_node_data = FALSE) {
  if (inherits(graph, "tbl_graph")) {
    g <- graph
  } else if (inherits(graph, "igraph")) {
    g <- tidygraph::as_tbl_graph(graph)
  } else {
    stop("Input must be an igraph or tbl_graph object.")
  }
  
  g <- g %>%
    tidygraph::mutate(
      degree = tidygraph::centrality_degree(),
      mean_neighbor_degree = tidygraph::map_local(~ mean(.x$degree, na.rm = TRUE)),
      paradox = degree < mean_neighbor_degree
    )
  
  paradox_proportion <- mean(g$paradox, na.rm = TRUE)
  degree_mean <- mean(g$degree, na.rm = TRUE)
  neighbor_mean <- mean(g$mean_neighbor_degree, na.rm = TRUE)
  
  out <- list(
    paradox_proportion = paradox_proportion,
    summary = data.frame(
      mean_degree = degree_mean,
      mean_neighbor_degree = neighbor_mean,
      paradox_proportion = paradox_proportion
    )
  )
  
  if (return_node_data) {
    out$nodes <- as.data.frame(g)
  }
  
  return(out)
}


#' Plot Friendship Paradox Comparison
#'
#' Plots each node's number of friends (degree) against the mean number
#' of friends among their friends (mean neighbor degree), with a 1:1 reference line.
#' Useful for visualizing the strength and spread of the friendship paradox.
#'
#' @param graph An `igraph` or `tidygraph::tbl_graph` object.
#' @param label Logical. If TRUE, adds node labels.
#' @param point_size Size of points in the scatterplot.
#' @param ... Additional arguments passed to `ggplot2::geom_point()`.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' library(tidygraph)
#' library(ggplot2)
#'
#' g <- simulate_feld_1991_tbl()
#' plot_friendship_paradox(g)
#'
#' @export
plot_friendship_paradox <- function(graph, label = FALSE, point_size = 2, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required.")
  }
  
  if (inherits(graph, "tbl_graph")) {
    g <- graph
  } else if (inherits(graph, "igraph")) {
    g <- tidygraph::as_tbl_graph(graph)
  } else {
    stop("Input must be an igraph or tbl_graph object.")
  }
  
  g <- g %>%
    tidygraph::mutate(
      degree = tidygraph::centrality_degree(),
      mean_neighbor_degree = tidygraph::map_local(~ mean(.x$degree, na.rm = TRUE))
    )
  
  df <- as.data.frame(g)
  df$id <- seq_len(nrow(df))  # fallback label
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = degree, y = mean_neighbor_degree)) +
    ggplot2::geom_point(size = point_size, ...) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      x = "Individual's Number of Friends",
      y = "Mean Number of Friends of Friends",
      title = "Friendship Paradox: Degree vs. Friends-of-Friends"
    ) +
    ggplot2::theme_minimal()
  
  if (label && "name" %in% names(df)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = name), hjust = 1.2, vjust = 1.2, size = 3)
  }
  
  return(p)
}

#' Load Feld's 1991 data.
#' 
#' @example 
#' feld_net <- get_feld_1991_network()
#' ggnetplot(fnet, layout_with_fr) + 
#'   geom_edges(linewidth=0.7) + 
#'   geom_nodes(color = "#008566", size=9) + 
#'   geom_nodetext(aes(label = name), color = "white") + 
#'   theme_blank()
#' @export
get_feld_1991_network <- function() {
 
  return (
  # Read included CSV and return graph.
  load_igraph_from_csv(
    system.file("extdata", "marketville-friends-coleman-feld.csv", 
                package = "socmod")
  )
 )
}


#' Load an Undirected igraph Object from a CSV Edge List
#'
#' Loads a graph from a CSV file containing a two-column edge list.
#' Assumes columns are either `from` and `to`, or `source` and `target`.
#'
#' @param csv_file Path to a CSV file with two columns representing edges.
#' @return An undirected igraph object.
#'
#' @examples
#' fnet <- load_igraph_from_csv("marketville-friends-coleman-feld.csv")
#'
#' @export
load_igraph_from_csv <- function(csv_file) {
  if (!file.exists(csv_file)) {
    stop("File does not exist: ", csv_file)
  }
  
  # Load csv that has two columns of node names defining edges between them.
  edges <- read.csv(csv_file, colClasses = c("character", "character"))
  
  # Force first two columns to be character.
  edges[[1]] <- as.character(edges[[1]])
  edges[[2]] <- as.character(edges[[2]])
  
  # igraph expects this in call below.
  colnames(edges)[1:2] <- c("from", "to")
  
  return (igraph::graph_from_data_frame(edges, directed = FALSE))
}
