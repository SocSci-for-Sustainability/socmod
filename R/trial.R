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
#' @export
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
#' @export
fixated <- function(model) {
  behaviors <- unlist(purrr::map(model$agents, \(a) as.character(a$get_behavior())), use.names = FALSE)
  length(unique(behaviors)) == 1
}


#' Run a Trial on an AgentBasedModel with standard learning loop
#'
#' @param model An AgentBasedModel
#' @param partner_selection Function for selecting interaction partner
#' @param interaction Function for modifying agents based on partner
#' @param iterate Optional function for updating model state
#' @param stop Stopping condition: max steps (int) or predicate function (default: 50)
#' @return A Trial object
#' @export
#' @examples
#' agents <- list(
#'   Agent$new(name = "1", behavior = "Legacy", fitness = 1),
#'   Agent$new(name = "2", behavior = "Adaptive", fitness = 4),
#'   Agent$new(name = "3", behavior = "Legacy", fitness = 1),
#'   Agent$new(name = "4", behavior = "Legacy", fitness = 2)
#' )
#' net <- igraph::make_graph(~ 1-2, 1-3, 1-4, 3-2)
#' model <- AgentBasedModel$new(agents = agents, network = net)
#' trial <- run_trial(model, stop = 10)
run_trial <- function(model,
                      partner_selection = success_bias_select_teacher,
                      interaction = success_bias_interact,
                      iterate = iterate_learning_model,
                      stop = 50) {
  trial <- Trial$new(
    model = model,
    partner_selection = partner_selection,
    interaction = interaction,
    iterate = iterate
  )
  print(trial$model$get_agent(1)$get_neighbors()$agents)
  trial$run(stop = stop)
  return (trial)
}


#' Plot adoption counts of selected behaviors over time
#'
#' @param trial A Trial object
#' @param behaviors Character vector of behaviors to track (e.g., c("Adaptive", "Legacy"))
#' @return A ggplot object
#' @export
#' @examples
#' # Assuming you have run a trial as in the run_trial() example:
#' plot_adoption(trial, behaviors = c("Adaptive", "Legacy"))
plot_adoption <- function(trial, behaviors = c("Adaptive")) {
  obs <- trial$get_observations()
  
  obs_filtered <- obs %>%
    dplyr::filter(behavior %in% behaviors) %>%
    dplyr::group_by(t, behavior) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
  
  ggplot2::ggplot(obs_filtered, ggplot2::aes(x = t, y = count, color = behavior)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::xlab("Time step") +
    ggplot2::ylab("Agent count") +
    ggplot2::theme_classic() +
    ggplot2::scale_color_brewer(palette = "Set1")
}
