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
#' @return igraph Graph
#' @export
make_homophily_network <- function(group_sizes = c(3, 7), mean_degree = 6,
                                   homophily = c(0.0), group_names = NULL,
                                   add_to_complete = FALSE) {
  
  N <- sum(group_sizes)
  
  assert_that(
    (length(homophily) == 1) || (length(homophily) == length(group_sizes)), 
    msg = 
      "Homophily must be singleton or of the same length as the number of groups"
  )
  
  assert_that(mean_degree < N, msg = "Mean degree can be at most N - 1")
  
  assert_that((-1 <= homophily) && (homophily <= 1), 
              msg = "Homophily must be between -1 and 1")
  
  net <- make_empty_graph(N, directed = FALSE)
  
  if (is.null(group_names)) {
    group_names <- 
      map_vec(1:length(group_sizes),
              \(ii) { as.factor(ii) })
  }
  
  a_idx = 1
  g_idx = 1
  for (group_size in group_sizes) {
    final_a_idx <- a_idx + group_size - 1
    V(net)[a_idx:final_a_idx]$group <- group_names[g_idx]
    g_idx <- g_idx + 1
    a_idx <- final_a_idx + 1
  }
  
  # The number of edges per group is the group size times user-specified mean degree.
  edges_per_group <- (group_sizes * mean_degree)
  
  # Scale by homophily to get edges per group, divided by two since each edge
  # adds additional degree per connected node and rounded for a whole number.
  edges_within_per_group <- round(edges_per_group * ((1 + homophily)/2) * 0.5)

  # The number of between-group edges starting from each group 
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
            
            needing_group_verts <- V(net)[V(net)$group == last_group]
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


