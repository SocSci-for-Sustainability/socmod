# Trial class for socmod

#' Trial class for running a single simulation
#'
#' Represents a single run of an AgentBasedModel over time, with customizable
#' interaction and update logic. Tracks time-series observations and outcome
#' measures such as adaptation success and steps to fixation.
#'
#' @examples
#' # Setup a simple model where Adaptive pays off more than Legacy
#' model <- AgentBasedModel$new(n_agents = 3)
#' model$get_agent(1)$set_behavior("Adaptive")
#' model$get_agent(1)$set_fitness(1.0)
#' model$get_agent(2)$set_behavior("Legacy")
#' model$get_agent(2)$set_fitness(0.0)
#' model$get_agent(3)$set_behavior("Legacy")
#' model$get_agent(3)$set_fitness(0.0)
#' 
#' # Run a trial using success-biased learning
#' trial <- Trial$new(
#'   model,
#'   partner_selection = success_bias_select_teacher,
#'   interaction = success_bias_interact,
#'   iterate = iterate_learning_model
#' )
#' trial$run(stop = fixated)
#' 
#' # Get outcome summary and time series
#' trial$get_outcomes()  # Expect adaptation_success = TRUE, fixation_steps = 1
#' trial$get_observations()  # Tibble with t, agent, behavior, fitness
#' 
#' # Visualize behavior over time
#' obs <- trial$get_observations()
#' ggplot2::ggplot(obs, ggplot2::aes(x = t, fill = behavior)) +
#'   ggplot2::geom_bar(position = "fill") +
#'   ggplot2::ylab("Proportion of agents") +
#'   ggplot2::theme_minimal()
Trial <- R6::R6Class(
  "Trial",
  public = list(
    model = NULL,
    observations = NULL,
    outcomes = NULL,
    partner_selection = NULL,
    interaction = NULL,
    iterate = NULL,
    
    #' @description Initialize a Trial with a model and functions
    #' @param model An AgentBasedModel instance
    #' @param partner_selection Optional partner selection function
    #' @param interaction Function defining interaction logic
    #' @param iterate Optional iteration update function
    initialize = function(model, partner_selection = NULL, interaction, iterate = NULL) {
      self$model <- model
      self$partner_selection <- partner_selection
      self$interaction <- interaction
      self$iterate <- iterate
      self$observations <- tibble::tibble()
      self$outcomes <- list()
    },
    
    #' @description Run the model and collect results
    #' @param stop Either integer for max steps, or predicate function
    run = function(stop = 50) {
      step <- 0
      while (TRUE) {
        step <- step + 1
        
        for (agent in self$model$agents) {
          teacher <- NULL
          if (!is.null(self$partner_selection)) {
            teacher <- self$partner_selection(agent, self$model)
          }
          self$interaction(agent, teacher, self$model)
        }
        
        if (!is.null(self$iterate)) self$iterate(self$model)
        
        self$observations <- dplyr::bind_rows(
          self$observations,
          tibble::tibble(
            t = step,
            agent = unlist(purrr::map(self$model$agents, \(a) a$get_name()), use.names = FALSE),
            behavior = unlist(purrr::map(self$model$agents, \(a) as.character(a$get_behavior())), use.names = FALSE),
            fitness = unlist(purrr::map(self$model$agents, \(a) a$get_fitness()), use.names = FALSE)
          )
        )
        
        if (is.function(stop)) {
          if (stop(self$model)) break
        } else if (step >= stop) {
          break
        }
      }
      
      behaviors <- unlist(purrr::map(self$model$agents, \(a) as.character(a$get_behavior())), use.names = FALSE)
      self$outcomes$adaptation_success <- length(unique(behaviors)) == 1 && unique(behaviors) == "Adaptive"
      self$outcomes$fixation_steps <- step
    },
    
    #' @description Return the observation data
    get_observations = function() {
      return(self$observations)
    },
    
    #' @description Return outcome measures
    get_outcomes = function() {
      return(self$outcomes)
    }
  )
)

#' @description Predicate function that returns TRUE if the model has fixated on one behavior
#' @param model An AgentBasedModel
fixated <- function(model) {
  behaviors <- unlist(purrr::map(model$agents, \(a) as.character(a$get_behavior())), use.names = FALSE)
  length(unique(behaviors)) == 1
}
