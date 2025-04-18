test_that("LearningStrategy functions are correctly set and working as expected", {
  
  # Dummy learning functions
  dummy_select <- function(learner, model) { NULL }
  dummy_interact <- function(learner, teacher, model) { NULL }
  
  # Create LearningStrategy instance
  dummy_strategy <- make_learning_strategy(
    partner_selection = dummy_select,
    interaction = dummy_interact,
    label = "Dummy Strategy"
  )
  
  # Check if the strategy is correctly set.
  expect_equal(dummy_strategy$get_partner_selection(), dummy_select)
  expect_equal(dummy_strategy$get_interaction(), dummy_interact)
  expect_equal(dummy_strategy$get_label(), "Dummy Strategy")
  
  # Check if the strategy can be called.
  expect_equal(typeof(dummy_strategy$get_partner_selection()), "closure")
  expect_equal(typeof(dummy_strategy$get_partner_selection()), "closure")
}
)




