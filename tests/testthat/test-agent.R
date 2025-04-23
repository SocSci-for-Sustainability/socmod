test_that("Agent initialization and name management works", {
  a <- Agent$new(id = 1, name = "a1")
  
  expect_equal(a$get_id(), 1)
  expect_equal(a$get_name(), "a1")
  
  a$set_name("alpha")
  expect_equal(a$get_name(), "alpha")
})

test_that("Agent behavior and fitness getters/setters work", {
  a <- Agent$new(id = 1, name = "a1")
  
  a$set_behavior("Legacy")
  a$set_next_behavior("Adaptive")
  expect_equal(a$get_behavior(), "Legacy")
  expect_equal(a$get_next_behavior(), "Adaptive")
  
  a$advance_behavior()
  expect_equal(a$get_behavior(), "Adaptive")
  
  a$set_fitness(1.0)
  a$set_next_fitness(2.0)
  expect_equal(a$get_fitness(), 1.0)
  expect_equal(a$get_next_fitness(), 2.0)
  
  a$advance_fitness()
  expect_equal(a$get_fitness(), 2.0)
})

test_that("Agent custom attributes work", {
  a <- Agent$new(id = 2, name = "a2")
  
  a$set_attribute("learning_type", "success-biased")
  expect_equal(a$get_attribute("learning_type"), "success-biased")
  
  a$set_attributes(list(strategy = "risk-averse", group = "X"))
  attrs <- a$get_attributes()
  expect_equal(attrs$strategy, "risk-averse")
  expect_equal(attrs$group, "X")
})

test_that("Agent neighbor management works", {
  a <- Agent$new(id = 2, name = "a2")
  b <- Agent$new(id = 3, name = "a3")
  c <- Agent$new(id = 4, name = "a4")
  
  a$add_neighbors(b, c)
  
  neighbors <- a$get_neighbors()
  expect_equal(neighbors$length(), 2)
  expect_equal(
    sort(unlist(neighbors$map(\(n) n$get_name()), use.names = FALSE)), 
    c("a3", "a4")
  )
  
  a$remove_neighbors(c)
  expect_equal(neighbors$length(), 1)
  expect_equal(neighbors$get(1)$get_name(), "a3")
})

test_that("Agent$set_neighbors() can only take a list of Agents or a Neighbors instance", {
  
  a1 <- socmod::Agent$new(
    1, name = "Matt", behavior = "Adaptive", fitness=1e6
  )
  a1$set_neighbors(Neighbors$new(c(
    socmod::Agent$new(id = 2, name = "n2"),
    socmod::Agent$new(id = 3, name = "n3")
  )))
  expect_equal(a1$get_neighbors()$map(\(n) n$get_name()), 
               list("n2", "n3"))
  
  a1 <- socmod::Agent$new(
    1, name = "Matt", behavior = "Adaptive", fitness=1e6
  )
  a1$set_neighbors(c(
    socmod::Agent$new(id = 2, name = "n2"),
    socmod::Agent$new(id = 3, name = "n3")
  ))
  expect_equal(a1$get_neighbors()$map(\(n) n$get_name()), 
               list("n2", "n3"))
  
  expect_error(a1$set_neighbors(c("a", "b")))
})