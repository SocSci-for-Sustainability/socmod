test_that("Agent neighbor add/remove integrates with Neighbors class", {
  g <- igraph::make_ring(3)
  
  model <- AgentBasedModel$new(graph = g)
  
  a1 <- model$get_agent("a1")
  a2 <- model$get_agent("a2")
  a3 <- model$get_agent("a3")
  
  # Manually clear and assign neighbors to test add/remove
  a1$neighbors <- Neighbors$new()
  a1$add_neighbors(a2, a3)
  
  expect_equal(a1$neighbors$length(), 2)
  expect_equal(a1$neighbors$map(\(a) a$get_name()), c("a2", "a3"))
  
  a1$remove_neighbors(a3)
  expect_equal(a1$neighbors$length(), 1)
  expect_equal(a1$neighbors$get(1)$get_name(), "a2")
})


test_that("Neighbors$get() works by index and by name", {
  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("a1", "a2", "a3")
  model <- AgentBasedModel$new(graph = g)
  
  a2 <- model$get_agent("a2")
  neighbors <- a2$neighbors
  
  expect_true(inherits(neighbors$get(1), "R6"))
  expect_equal(neighbors$get("a1")$get_name(), "a1")
  
  expect_error(neighbors$get("nonexistent"), "No neighbor found with that name")
  expect_error(neighbors$get(list()), "key must be numeric or character")
})
