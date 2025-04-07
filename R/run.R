# Set up empty stubs for default model subroutines.

#' @export
partner_selection_default = function(agent, model) { return (NULL) }

#' @export
interaction_default       = function(agent1, agent2, model) {}

#' @export
iterate_model_default     = function(model) {}

#' @export
stop_cond_default <- function(model) { return (FALSE) }

#' Run an agent-based model for either a number of iterations (`max_its`) or 
#' until some stop condition is met (e.g., fixation).
#' @param model 
#'
#' @param max_its 
#' @param partner_selection 
#' @param interaction 
#' @param iterate_model 
#' @param stop_cond 
#'
#' @export
run <- function(model, max_its = 1, 
                partner_selection = partner_selection_default,
                interaction = interaction_default, 
                iterate_model = iterate_model_default, 
                stop_cond = stop_cond_default) {
  
  assert_that(length(model$agents) > 0)
  
  tmin <- 0
  tmax <- max_its
  
  total_adoption <- function(agents) {
    sum(purrr::map_vec(
      agents, 
      \(a) { ifelse(
        !is.null(a$get_behavior()) && (a$get_behavior() == "Adaptive"), 1, 0) 
      }
    ))
  }
  
  if (is.null(model$output)) {
    model$output <- tibble::tibble(t = 0:tmax, A = rep(0.0, tmax + 1))
    model$output[1, "A"] <- total_adoption(model$agents)
  } else {
    prev_tvec <- model$output$t
    prev_tmax <- max(prev_tvec)
    tmin <- prev_tmax + 1
    tmax <- tmin + max_its - 1
    
    model$output <- rbind(
      model$output,
      tibble::tibble(t = tmin:tmax, A = rep(0.0, max_its))
    )
  }
  
  for (t in tmin:tmax) {
    for (learner in sample(model$agents)) {
      teacher <- partner_selection(learner, model)
      interaction(learner, teacher, model)
    }
    
    iterate_model(model)
    model$output[model$output$t == t, "A"] <- total_adoption(model$agents)
  }
  
  return(model)
}
