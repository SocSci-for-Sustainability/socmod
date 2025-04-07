test_that("Agent exposure probability works as expected", {
  florentine_m <- netrankr::florentine_m
  florentine_m <- igraph::delete_vertices(florentine_m, which(igraph::degree(florentine_m) == 0))
  
  m <- AgentBasedModel$new(graph = florentine_m)
  
  alb <- m$get_agent("Albizzi")
  alb$set_behavior("Adaptive")
  
  gin <- m$get_agent("Ginori")
  gua <- m$get_agent("Guadagni")
  med <- m$get_agent("Medici")
  
  expect_equal(gin$exposure_prob(), 1)
  expect_equal(gua$exposure_prob(), 1 / 4)
  expect_equal(med$exposure_prob(), 1 / 6)
  
  total_exposure <- sum(purrr::map_dbl(m$agents, \(a) a$exposure_prob()))
  expect_equal(total_exposure, 1 + 1 / 4 + 1 / 6)
})

test_that("Agent behavior and fitness lifecycle works", {
  g <- igraph::make_ring(1)
  igraph::V(g)$name <- "solo"
  m <- AgentBasedModel$new(graph = g)
  a <- m$get_agent("solo")
  
  # Name
  expect_equal(a$get_name(), "solo")
  a$set_name("renamed")
  expect_equal(a$get_name(), "renamed")
  
  # Behavior
  a$set_next_behavior("Curious")
  a$advance_behavior()
  expect_equal(a$get_behavior(), "Curious")
  
  # Fitness
  a$set_next_fitness(0.42)
  a$advance_fitness()
  expect_equal(a$get_fitness(), 0.42)
  
  # Generic attr
  a$set_attr("flag", TRUE)
  expect_true(a$get_attr("flag"))
})

test_that("Agent behavior and fitness accessors are symmetric", {
  g <- igraph::make_empty_graph(1)
  igraph::V(g)$name <- "a1"
  m <- AgentBasedModel$new(graph = g)
  a <- m$get_agent("a1")
  
  # Behavior: set, get, set_next, advance
  a$set_behavior("Adaptive")
  expect_equal(a$get_behavior(), "Adaptive")
  
  a$set_next_behavior("Imitative")
  a$advance_behavior()
  expect_equal(a$get_behavior(), "Imitative")
  
  # Fitness: set, get, set_next, advance
  a$set_fitness(0.25)
  expect_equal(a$get_fitness(), 0.25)
  
  a$set_next_fitness(0.5)
  a$advance_fitness()
  expect_equal(a$get_fitness(), 0.5)
})

test_that("Agent next behavior and fitness getters work", {
  g <- igraph::make_ring(1)
  igraph::V(g)$name <- "solo"
  m <- AgentBasedModel$new(graph = g)
  a <- m$get_agent("solo")
  
  a$set_next_behavior("Learning")
  expect_equal(a$get_next_behavior(), "Learning")
  
  a$set_next_fitness(0.75)
  expect_equal(a$get_next_fitness(), 0.75)
})

test_that("Agent neighbors field is a Neighbors object", {
  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("a1", "a2", "a3")
  m <- AgentBasedModel$new(graph = g)
  
  a2 <- m$get_agent("a2")
  nbrs <- a2$neighbors
  
  expect_true(inherits(nbrs, "Neighbors"))
  expect_equal(nbrs$length(), 2)
  expect_equal(nbrs$map(\(a) a$get_name()), c("a1", "a3"))
})