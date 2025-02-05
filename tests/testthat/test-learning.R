# test_that("Success-bias selects teachers as expected", 
# {
#   # Initialize a model with fully-connected network (default).
#   model <- AgentBasedModel$new(n_agents = 4)
# 
#   # Extract agents for convenience.
#   a1 <- model$get_agent(1)
#   a2 <- model$get_agent(2)
#   a3 <- model$get_agent(3)
#   a4 <- model$get_agent(4)
# 
#   # Set fitness for agents 2-4, with 3 having fitness 1, others 0.
#   a2$set_fitness(0.0)
#   a3$set_fitness(1.0)
#   a4$set_fitness(0.0)
# 
#   # Then we expect that teacher selection will select agent 3.
#   expect_equal(success_bias_select_teacher(a1, model)$name, 3)
# 
#   # Now set the fitnesses to be multiples of 1 so the probabilities of selection
#   # are 1/6, 1/3, and 1/2; expect observe this fraction of 10000 selections.
#   a2$set_fitness(1.0)
#   a3$set_fitness(2.0)
#   a4$set_fitness(3.0)
#   
#   n_selections <- 20000
#   # n_selections <- 50
#   selected_idxs <- 
#     purrr::map_vec(1:n_selections, 
#                    \(.) { success_bias_select_teacher(a1, model)$name })
#   # print(selected_idxs)
#   # print(rle(sort(selected_idxs)))
#   select_frequency <- rle(sort(selected_idxs))$lengths / n_selections
#   
#   expect_equal(select_frequency[1], 1.0 / 6.0, tolerance = 1e-2)
#   expect_equal(select_frequency[2], 1.0 / 3.0, tolerance = 1e-2)
#   expect_equal(select_frequency[3], 1.0 / 2.0, tolerance = 1e-2)
# })

test_that("Success-biased learning results in expected learned behaviors",
{
  # Initialize a default-networked model as above, but now testing behavior learned.
  model <- AgentBasedModel$new(n_agents = 4)
  
  # Extract agents for convenience.
  a1 <- model$get_agent(1)
  a2 <- model$get_agent(2)
  a3 <- model$get_agent(3)
  a4 <- model$get_agent(4)
  
  # Set fitness for agents 2-4, with 3 having fitness 1, others 0.
  a1$set_fitness(1.0)  # Need this so we don't have NA for others' learning.
  a2$set_fitness(1.0)
  a3$set_fitness(1e9)
  a4$set_fitness(1.0)
  
  # Check that a3 is the one that a1 learned from.
  b3 <- "Behavior #3"
  a3$curr_behavior <- b3
  
  out <- run(model,
             partner_selection = success_bias_select_teacher,
             interaction = success_bias_interact, 
             iterate_m = iterate_learning_model)$output
  
  expect_equal(a1$curr_behavior, b3)
  expect_equal(nrow(out), 2)
  expect_equal(out$t, c(0, 1))
  expect_equal(out$t, c(0, 1))
  
  # Now set the fitnesses to be multiples of 1 so the probabilities of selection
  # are 1/6, 1/3, and 1/2; expect observe this fraction of 10000 selections.
  do_one <- function() {
    
    # Initialize a new model every time to only test first learning step.
    model <- AgentBasedModel$new(n_agents = 4)
    
    # Extract agents for convenience.
    a1 <- model$get_agent(1)
    a2 <- model$get_agent(2)
    a3 <- model$get_agent(3)
    a4 <- model$get_agent(4)
    
    a2$set_fitness(1.0)
    a3$set_fitness(2.0)
    a4$set_fitness(3.0)
    
    a2$curr_behavior <- "B-2"
    a3$curr_behavior <- "B-3"
    a4$curr_behavior <- "B-4"
    
    run(model, 
        partner_selection = success_bias_select_teacher,
        interaction = success_bias_interact, 
        iterate_m = iterate_learning_model)
    
    return (a1$curr_behavior)
  }
  
  n_reps <- 1000
  a1_learned_behaviors <- purrr::map_vec(1:n_reps, \(.){ do_one() })
  
  select_count <- rle(sort(a1_learned_behaviors))
  select_frequency <- select_count$lengths / n_reps
  select_labels <- select_count$values
  names(select_frequency) <- select_labels
  
  expect_equal(select_frequency[[select_labels[1]]], 1.0 / 6.0, tolerance = 1e-1)
  expect_equal(select_frequency[[select_labels[2]]], 1.0 / 3.0, tolerance = 1e-1)
  expect_equal(select_frequency[[select_labels[3]]], 1.0 / 2.0, tolerance = 1e-1)
})