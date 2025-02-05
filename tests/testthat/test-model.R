library(purrr)

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

  agents <- purrr:::map(behaviors, \(b) { Agent$new(b) })

  model <- AgentBasedModel$new(agents = agents,
                               network = regular_lattice(N, 4),
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


test_that("ABM initialized by network",
{
  # Use Florentine network for fun, check it loaded as expected.
  library(netrankr)
  florentine_m <-
    delete_vertices(florentine_m, which(degree(florentine_m) == 0))
  fl_verts <- V(florentine_m)
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
      purrr::map_vec(
        agents,
        \(a) { ifelse(a$curr_behavior == "Legacy", 1, 0) }
      )
    )

  expect_equal(n_doing_legacy, n_agents)

  # Spot check some neighbors are as expected.
  expect_true(m$agents[["Tornabuon"]]$neighbors$contains("Medici"))
  expect_true(m$agents[["Medici"]]$neighbors$contains("Tornabuon"))

  expect_true(m$agents[["Bischeri"]]$neighbors$contains("Peruzzi"))

  expect_true(m$get_agent("Albizzi")$neighbors$contains("Guadagni"))
  expect_true(m$get_agent("Albizzi")$neighbors$contains("Medici"))

  expect_false(m$get_agent("Acciaiuol")$neighbors$contains("Pazzi"))
})
