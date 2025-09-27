test_that("AgentBasedModel assigns sequential names if graph vertices unnamed", {
  g <- igraph::make_ring(3) # no names by default
  params <- make_model_parameters(graph = g)
  abm <- AgentBasedModel$new(parameters = params)
  
  expect_true(all(!is.na(igraph::V(abm$graph)$name)))
  expect_equal(igraph::V(abm$graph)$name, c("a1", "a2", "a3"))
})


test_that("AgentBasedModel creates empty graph when only n_agents is given", {
  params <- make_model_parameters(n_agents = 4) # no graph
  abm <- AgentBasedModel$new(parameters = params)
  g <- abm$graph
  
  expect_equal(igraph::vcount(g), 4)
  expect_equal(igraph::ecount(g), 0) # empty graph, not full
  expect_equal(igraph::V(g)$name, c("a1", "a2", "a3", "a4"))
})


test_that("AgentBasedModel creates empty graph with names a1,...,a4 when n_agents given", {
  
  abm <- make_abm(n_agents = 4)
  
  g <- abm$graph
  
  expect_equal(igraph::vcount(g), 4)
  expect_equal(igraph::ecount(g), 0) # empty graph
  expect_equal(igraph::V(g)$name, c("a1", "a2", "a3", "a4"))
  expect_equal(names(abm$agents), c("a1", "a2", "a3", "a4"))
})


test_that("AgentBasedModel creates empty graph, keeps agent names in agent list param", {
  
  agents <- c(Agent$new(id = 1, name = "alpha"), Agent$new(id = 2, name = "omega"))
  abm <- make_abm(agents = agents) 
  
  g <- abm$graph
  
  expect_equal(igraph::vcount(g), 2)
  expect_equal(igraph::ecount(g), 0) # empty graph
  expect_equal(igraph::V(g)$name, c("alpha", "omega"))
})