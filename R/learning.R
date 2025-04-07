#' A generic method for iterating a learning model, setting the current 
#' behavior and fitness to be whatever was identified as the next behavior
#' and fitness.
#'
#' @return NULL Operates in-place to update all agent's behavior if necessary.
#' @export
iterate_learning_model <- function(model) {
  for (agent in model$agents) {
    agent$set_behavior(agent$get_next_behavior())
    agent$set_fitness(agent$get_next_fitness())
  }
}

### ------ FREQUENCY BIAS --------

#' Frequency biased teacher selection does nothing
#'
#' @return NULL
#' @export
frequency_bias_select_teacher <- function(agent, model) { return (NULL) }

#' Interaction function for frequency-biased adaptive learning.
#'
#' @param learner Agent currently selected as learner.
#' @param . Not used (no teacher for frequency bias).
#' @param model The ABM instance
#' @return NULL 
#' @export
frequency_bias_interact <- function(learner, ., model) {
  if (is.null(model$params$learning_prob) || (runif(1) < model$params$learning_prob)) {
    behavior_counts <- learner$neighbors$map(\(a) a$get_behavior()) |>
      table() |>
      as.data.frame()
    
    names(behavior_counts) <- c("curr_behavior", "n")
    
    behavior_counts$selection_prob <- behavior_counts$n / sum(behavior_counts$n)
    
    selected <- sample(behavior_counts$curr_behavior, 1, prob = behavior_counts$selection_prob)[[1]]
    learner$set_next_behavior(selected)
  }
}

### ----- SUCCESS BIAS --------

#' Success-biased teacher selection
#'
#' @return Agent selected from neighbors as teacher.
#' @export 
success_bias_select_teacher <- function(learner, model) {
  learner$neighbors$sample(
    weights = \(a) {
      val <- a$get_fitness()
      if (is.numeric(val) && length(val) == 1 && !is.na(val)) val else 0
    },
    n = 1
  )
}


#' Success-biased interaction function
#'
#' @return NULL
#' @export
success_bias_interact <- function(learner, teacher, model) {
  if (is.null(model$params$learning_prob) || (runif(1) < model$params$learning_prob)) {
    learner$set_next_behavior(teacher$get_behavior())
    learner$set_next_fitness(teacher$get_fitness())
  }
}
