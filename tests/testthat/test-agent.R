test_that("Exposure probability works as expected",
{
  # Use Florentine network for fun, check it loaded as expected.
  florentine_m <- netrankr::florentine_m
  florentine_m <-
    delete_vertices(florentine_m, which(igraph::degree(florentine_m) == 0))
  fl_verts <- igraph::V(florentine_m)
  n_verts <- length(fl_verts)
  expect_equal(n_verts, 15)
  
  # Initialize the model with the Florentine network.
  m <- AgentBasedModel$new(network = florentine_m)
  alb <- m$get_agent("Albizzi")
  
  alb$curr_behavior <- "Adaptive"

  gin <- m$get_agent("Ginori")
  gua <- m$get_agent("Guadagni")
  med <- m$get_agent("Medici")
  
  expect_equal(gin$exposure_prob(), 1)
  expect_equal(gua$exposure_prob(), 1./4.)
  expect_equal(med$exposure_prob(), 1./6.)
  
  total_exposure <- sum(map_vec(m$agents, \(a) { a$exposure_prob() }))
  
  expect_equal(total_exposure, 1 + (1/4.) + (1/6.))
})
