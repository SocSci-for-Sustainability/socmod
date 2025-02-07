# Set up empty stubs for default model subroutines.
#' @export
partner_selection_default = function(agent, model) { return (NULL) }

#' @export
interaction_default       = function(agent1, agent2, model) {}

#' @export
iterate_model_default     = function(model) {}

#' @export
stop_cond_default <- function(model) { return (FALSE) }

#' @export
run <- function(model, max_its = 1, 
                partner_selection = partner_selection_default,
                interaction = interaction_default, 
                iterate_model = iterate_model_default, 
                stop_cond = stop_cond_default) {

  # Check that there are some agents.
  assert_that(length(model$agents) > 0)

  # Initialize output tibble for this run, starting with default tmin, tmax.
  tmin <- 0
  tmax <- max_its
  
  # Set up function to calculate total_adoption of the adaptive behavior.
  total_adoption <- function(agents) {
    
    # TODO: provide Agents wrapper with $map(), $as_list(), $as_tibble() methods.
    sum(purrr::map_vec(
      agents, 
      \(a) { ifelse(
        !is.null(a$curr_behavior) && (a$curr_behavior == "Adaptive"), 1, 0) 
      }
    ))
  }
  
  if (is.null(model$output)) {
    # If there is no output yet for the model, initialize a new output tibble.
    tmax <- max_its
    model$output <- tibble::tibble(t = 0:tmax,
                           A = rep(0.0, tmax + 1))

    model$output[1, "t"] <- 0
    model$output[1, "A"] <- total_adoption(model$agents)

  } else {
    # If the model has an output tibble, figure out the last time step and
    # initialize a new, properly-indexed new tibble to concatenate with previous.
    prev_tvec <- model$output$t
    prev_tmax <- model$output$t[length(prev_tvec)]
    tmin <- prev_tmax + 1
    tmax <- tmin + max_its 

    # Allocate more space for new output data.
    model$output <- 
      rbind(model$output, tibble::tibble(t = tmin:tmax, A = rep(0.0, max_its)))
  }

  model$step <- 1
  while ((model$step <= tmax)) { # stop_cond(model)) {
    
    for (learner in sample(model$agents)) {
      teacher <- partner_selection(learner, model)
      interaction(learner, teacher, model)
    }

    iterate_model(model)

    # Need to add one to current step, i.e., output[1,] was row 1, but tstep 0,
    # and so when model$step <- model$step + 1 runs for the first time, 
    # model$step increments from 0 to 1. Without +1, the last 
    # time step in the output would just be 0 from the initialization of output.
    model$output[model$step + 1, ] <- 
      list(model$step, total_adoption(model$agents))
    
    # Increment time step.
    model$step <- model$step + 1
  }

  return (model)
}
