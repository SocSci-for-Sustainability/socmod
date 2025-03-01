test_that("ABM initialized by n_agents keyword all Legacy and with N-1 neighbors",
{
  N <- 10
  model <- AgentBasedModel$new(n_agents = N)
 
  expect_true(
    all(
      map_vec(model$agents, \(a) { 
        (a$curr_behavior == "Legacy") && (a$neighbors$n == 9)
      })
    )
  )
})


test_that("ABM initialized by agents and network",
{
  N <- 10

  behaviors <-
    sample(
      c(rep("Legacy", as.integer(N/2)),
        rep("Adaptive", as.integer(N/2)))
    )

  agents <- map(behaviors, \(b) { Agent$new(b) })

  model <- AgentBasedModel$new(agents = agents,
                               network = make_regular_lattice(N, 4),
                               switch_prob = 0.1)

  n_legacy <- sum(map_vec(
    model$agents,
    \(a) { ifelse(a$curr_behavior == "Legacy", 1, 0) }
  ))

  n_adaptive <- sum(map_vec(
    model$agents,
    \(a) { ifelse(a$curr_behavior == "Adaptive", 1, 0) }
  ))

  expect_equal(n_legacy, 5)
  expect_equal(n_adaptive, 5)

  expect_true(
    all(
      map_vec(model$agents, \(a) { a$neighbors$n == 4 })
    )
  )
})


test_that("ABM initialized by Florentine marriage network",
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

  # Check that all agents are present and doing Legacy.
  agents <- m$agents
  n_agents <- length(agents)
  expect_equal(n_agents, n_verts)

  n_doing_legacy <-
    sum(
      map_vec(
        agents,
        \(a) { ifelse(a$curr_behavior == "Legacy", 1, 0) }
      )
    )

  # Check that all agents initialized with legacy behavior.
  expect_equal(n_doing_legacy, n_agents)

  # Spot check some neighbors are as expected.
  expect_true(m$agents[["Tornabuon"]]$neighbors$contains("Medici"))
  expect_true(m$agents[["Medici"]]$neighbors$contains("Tornabuon"))

  expect_true(m$agents[["Bischeri"]]$neighbors$contains("Peruzzi"))

  expect_true(m$get_agent("Albizzi")$neighbors$contains("Guadagni"))
  expect_true(m$get_agent("Albizzi")$neighbors$contains("Medici"))

  expect_false(m$get_agent("Acciaiuol")$neighbors$contains("Pazzi"))
})


test_that("ABM initialized by regular lattice",
{

  # Initialize the model with a regular lattice social network.
  N <- 10
  k <- 4
  net <- make_regular_lattice(N, k)
  m <- AgentBasedModel$new(network = net)
  
  # Check that all agents are present and doing Legacy.
  agents <- m$agents
  n_agents <- length(agents)
  expect_equal(n_agents, N)
  
  n_doing_legacy <-
    sum(
      map_vec(
        agents,
        \(a) { ifelse(a$curr_behavior == "Legacy", 1, 0) }
      )
    )
  
  # Check that all agents initialized with legacy behavior.
  expect_equal(n_doing_legacy, n_agents)
  
  # Spot check some neighbors are as expected.
  expect_true(all(map_vec(c(2,3,9,10), \(ii) { m$agents[[1]]$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(3,4,10,1), \(ii) { m$agents[[2]]$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(1,2,4,5), \(ii) { m$get_agent(3)$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(2,3,5,6), \(ii) { m$get_agent(4)$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(3,4,6,7), \(ii) { m$get_agent(5)$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(4,5,7,8), \(ii) { m$get_agent(6)$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(5,6,8,9), \(ii) { m$agents[[7]]$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(6,7,9,10), \(ii) { m$get_agent(8)$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(7,8,10,1), \(ii) { m$agents[[9]]$neighbors$contains(ii) })))
  expect_true(all(map_vec(c(8,9,1,2), \(ii) { m$get_agent(10)$neighbors$contains(ii) })))

})
