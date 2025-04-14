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
  
  # If learner is not stubborn (i.e., receptive) this round, 
  # skip social learning.
  stubbornness <- learner$get_attribute("stubbornness")
  receptive <- TRUE
  
  if (!is.null(stubbornness)) {
    receptive <- 
      runif(1) > learner$get_attribute("stubbornness")
  }
  
  if (receptive) {
    
    behaviors <- 
      learner$get_neighbors()$map(\(a) a$get_behavior())
    
    behavior_counts <- 
      table(as.character(behaviors)) |> as.data.frame()
    
    names(behavior_counts) <- c("curr_behavior", "n")
    
    behavior_counts$selection_prob <- 
      behavior_counts$n / sum(behavior_counts$n)
    
    selected <- 
      sample(behavior_counts$curr_behavior, 1, 
             prob = behavior_counts$selection_prob)[[1]]
  
    learner$set_next_behavior(as.character(selected))
  }
}


### ----- SUCCESS BIAS --------

#' @title Success-biased teacher selection
#' @description Selects the most successful neighbor (highest fitness) to learn from. Ties are broken at random.
#' @param learner An Agent instance evaluating neighbors.
#' @param model An AgentBasedModel instance (not used directly here, but included for consistency with other learning functions).
#' @return An Agent object: the selected teacher.
#' @examples
#' model <- ?
#' learner <- model$get_agent(1)
#' teacher <- success_bias_select_teacher(learner, model)
#' @export
success_bias_select_teacher <- function(learner, model) {
  
  stubbornness <- learner$get_attribute("stubbornness")
  
  # If stubbornness is not used, ignore it and proceed with learning as usual, or
  # check if the learner is stubborn this time, i.e., if a random uniform draw is
  # greater than stubbornness.
  if (is.null(stubbornness) || (runif(1) > stubbornness)) {
  
    learner$get_neighbors()$sample(
      weights = \(a) {
        fitness <- a$get_fitness()
        
        if (!is.numeric(fitness) || length(fitness) != 1 || is.na(fitness)) {
          return (0.0) 
        } else {
          return (fitness)
        }
      }
    )
  } else {
    
    return (NULL)
  }
}


#' Success-biased interaction function
#'
#' @return NULL
#' @export
success_bias_interact <- function(learner, teacher, model) {
  
  # Teacher will be null if learner is stubborn this time, nothing will happen.
  if (!is.null(teacher)) {
    
    learner$set_next_behavior(teacher$get_behavior())
    learner$set_next_fitness(teacher$get_fitness())
  }
}

### -------- CONTAGION -----------

#' @title Contagion-based partner selection
#' @description Selects one neighbor at random for potential contagion interaction.
#' @param learner An Agent instance.
#' @param model An AgentBasedModel instance.
#' @return A single neighbor Agent.
#' @examples
#' model <- example_model_with_params()
#' learner <- model$get_agent(1)
#' contagion_partner_selection(learner, model)
#' @export
contagion_partner_selection <- function(learner, model) {
  return (learner$get_neighbors()$sample(1))
}

#' @title Contagion-based interaction
#' @description Updates learner behavior based on interaction with an Adaptive neighbor.
#' @param learner An Agent instance.
#' @param teacher An Agent instance.
#' @param model An AgentBasedModel instance with parameter "adopt_rate".
#' @return None. Modifies the learner's next behavior and fitness.
#' @examples
#' model <- example_model_with_params(list(adopt_rate = 1.0))
#' learner <- model$get_agent(1)
#' teacher <- model$get_agent(2)
#' learner$set_behavior("Legacy")
#' teacher$set_behavior("Adaptive")
#' contagion_interaction(learner, teacher, model)
#' @export
contagion_interaction <- function(learner, teacher, model) {
  adopt_rate <- model$get_parameter("adopt_rate")
  
  if ((learner$get_behavior() == "Legacy") && 
      (teacher$get_behavior() == "Adaptive") && 
      (runif(1) < model$get_parameter("adopt_rate"))) {
    
    learner$set_next_behavior("Adaptive")
    learner$set_next_fitness(2.0)
  }
}

#' @title Contagion model step
#' @description Updates all agents for dropping behavior and advances model state.
#' @param model An AgentBasedModel instance with parameter "drop_rate".
#' @return None. Updates agent behaviors.
#' @examples
#' model <- example_model_with_params(list(drop_rate = 0.5))
#' contagion_model_step(model)
#' @export
contagion_model_step <- function(model) {
  drop_rate <- model$get_parameter("drop_rate")
  if (drop_rate > 0) {
    for (i in seq_along(model$agents)) {
      agent <- model$get_agent(i)
      if ((agent$get_behavior() == "Adaptive") && (runif(1) < drop_rate)) {
        agent$set_next_behavior("Legacy")
        agent$set_next_fitness(1.0)
      }
    }
  }
  
  iterate_learning_model(model)
}
