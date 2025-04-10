test_that("AgentBasedModel initializes from graph and generates agents with correct neighbors", {
  g <- igraph::make_ring(4)
  model <- AgentBasedModel$new(graph = g)
  
  expect_equal(length(model$agents), 4)
  
  a1 <- model$get_agent("a1")
  expect_equal(a1$get_id(), 1)
  expect_true(inherits(a1$get_neighbors(), "Neighbors"))
  expect_equal(a1$get_neighbors()$length(), 2)
  
  neighbor_names_named_vec <- sort(
    unlist(a1$get_neighbors()$map(\(a) a$get_name()), use.names = FALSE)
  )
  expect_equal(neighbor_names_named_vec, c("a2", "a4"))
})


test_that("AgentBasedModel prioritizes Agent field values over graph attributes", {
  g <- igraph::make_ring(3)
  igraph::V(g)$behavior_current <- rep("Legacy", 3)
  igraph::V(g)$fitness_current <- rep(0, 3)
  
  agents <- list(
    Agent$new(id = 1, name = "a1"),
    Agent$new(id = 2, name = "a2"),
    Agent$new(id = 3, name = "a3")
  )
  
  agents[[2]]$set_behavior("Adaptive")
  agents[[2]]$set_fitness(42)
  names(agents) <- purrr::map_chr(agents, \(a) a$get_name())
  
  model <- AgentBasedModel$new(graph = g, agents = agents)
  
  expect_equal(model$get_agent("a2")$get_behavior(), "Adaptive")
  expect_equal(model$get_agent("a2")$get_fitness(), 42)
})


test_that("sync_network keeps agents and graph consistent", {
  model <- AgentBasedModel$new(n_agents = 3)
  
  for (a in model$agents) {
    a$set_behavior(sample(c("Adaptive", "Legacy"), 1)[[1]])
    a$set_next_behavior(sample(c("Adaptive", "Legacy"), 1)[[1]])
    a$set_fitness(0.0)
    a$set_next_fitness(0.0)
  }
  a2 <- model$get_agent("a2")
  a2$set_behavior("Legacy")
  a2$set_next_behavior("Adaptive")
  a2$set_fitness(3.5)
  a2$set_next_fitness(4.0)
  
  g <- model$get_network()
  v <- igraph::V(g)["a2"]
  
  expect_equal(v$behavior_current, "Legacy")
  expect_equal(v$behavior_next, "Adaptive")
  expect_equal(v$fitness_current, 3.5)
  expect_equal(v$fitness_next, 4.0)
  expect_equal(v$name, "a2")
})


test_that("Model parameters can be set and retrieved individually and in bulk", {
  model <- AgentBasedModel$new(n_agents = 5)
  
  model$set_parameter("learning_prob", 0.75)
  model$set_parameter("seed_strategy", "friendship")
  
  expect_equal(model$get_parameter("learning_prob"), 0.75)
  expect_equal(model$get_parameter("seed_strategy"), "friendship")
  
  model$set_parameters(list(learning_prob = 0.5, intervention = TRUE))
  expect_equal(model$get_parameter("learning_prob"), 0.5)
  expect_equal(model$get_parameter("intervention"), TRUE)
  
  all_params <- model$get_parameters()
  expect_true("seed_strategy" %in% names(all_params))
  expect_equal(all_params$seed_strategy, "friendship")
})


test_that("AgentBasedModel initializes with n_agents only", {
  model <- AgentBasedModel$new(n_agents = 100)
  
  expect_s3_class(model, "AgentBasedModel")
  expect_equal(length(model$agents), 100)
  expect_true(igraph::is_igraph(model$get_network()))
  expect_equal(igraph::vcount(model$get_network()), 100)
  expect_equal(names(model$agents), igraph::V(model$get_network())$name)
})