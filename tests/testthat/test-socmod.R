library(purrr)
library(dplyr)
test_that("Network construction helpers work as expected", {

  # Get all possible undirected edges for graph vertices.
  N <- 10
  directed <- FALSE
  g <- make_empty_graph(N, directed)

  # Helper functions to calculate expected size of possible edge set.
  expected_E_size_undirected <- \(n_verts) { ((n_verts - 1) * n_verts / 2) }
  # Need to subtract $N$ because we don't want loop edges.
  expected_E_size_directed <- \(n_verts) { n_verts^2 - N }

  all_possible_edges <- get_all_possible_edges(N, directed)
  expect_equal(nrow(all_possible_edges), expected_E_size_undirected(N))

  directed <- TRUE
  g <- make_empty_graph(N, directed)
  verts <- V(g)

  all_possible_directed_edges <- get_all_possible_edges(N, directed)
  expect_equal(nrow(all_possible_directed_edges), expected_E_size_directed(N))
})


test_that("Erdős-Rényi random network constructions yield correct network stats",
{
  N <- 10
  M <- 20
  g <- G_NM(N, M)

  expected_edge_count <- 20

  expect_equal(ecount(g), expected_edge_count)  
}
)





