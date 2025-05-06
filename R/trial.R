#' Trial class for running a single simulation
#'
#' Represents a single run of an AgentBasedModel over time, with customizable
#' interaction and update logic. Tracks time-series observations and outcome
#' measures such as adaptation success and steps to fixation.
#'
#' @export
Trial <- R6::R6Class(
  "Trial",
  public = list(
    
    model = NULL,
    observations = NULL,
    outcomes = NULL,
    metadata = list(),
    
    #' @description Initialize a Trial with a model and functions
    #' @param model An AgentBasedModel instance
    #' @param metadata Label-value metadata store for trial information
    initialize = function(model, metadata = list()) {

      # Sync internal variables with user-provided.
      self$model <- model
      self$metadata <- metadata

      # Initialize outputs: observations and outcomes.
      self$observations <- tibble::tibble()
      self$outcomes <- list()

      invisible (self)
    },
    
    #' @description Run the model and collect results
    #' @param stop Either integer for max steps, or predicate function
    #' @param legacy_behavior The maladaptive behavior treated as "adaptation failure"
    #' @param adaptive_behavior The behavior treated as "adaptation success"
    run = function(
        stop = 50, legacy_behavior = "Legacy", adaptive_behavior = "Adaptive") {
      
      step <- 0

      self$model$set_parameter("legacy_behavior", legacy_behavior)
      self$model$set_parameter("adaptive_behavior", adaptive_behavior)
      
      # Record t = 0 before any updates.
      self$observations <- dplyr::bind_rows(
        self$observations,
        tibble::tibble(
          Step = 0,
          agent = unlist(
            purrr::map(
              self$model$agents, 
              \(a) a$get_name()
            ), 
            use.names = FALSE
          ),
          Behavior = unlist(
            purrr::map(
              self$model$agents, 
              \(a) as.character(a$get_behavior())
            ), 
            use.names = FALSE
          ),
          Fitness = unlist(
            purrr::map(
              self$model$agents, 
              \(a) a$get_fitness()
            ), 
            use.names = FALSE
          ),
          label = self$label
        )
      )
      
      # Get learning and iteration functions from the model's learning strategy.
      lstrat <- self$model$get_parameter("learning_strategy")
      partner_selection <- lstrat$get_partner_selection()
      interaction <- lstrat$get_interaction()
      model_step <- lstrat$get_model_step()

      # Main iteration loop.
      while (TRUE) {
        
        step <- step + 1
        
        # Partner selection and interaction with selected partner.
        for (agent in self$model$agents) {
          partner <- NULL
          if (!is.null(partner_selection)) {
            partner <- partner_selection(agent, self$model)
          }
          interaction(agent, partner, self$model)
        }
        
        # Run model step function if provided.
        if (!is.null(model_step)) {
          model_step(self$model)
        }
        
        # Update observations. 
        self$observations <- dplyr::bind_rows(
          
          self$observations,
          
          tibble::tibble(
            Step = step,
            agent = unlist(
              purrr::map(
                self$model$agents, 
                \(a) a$get_name()
              ), 
              use.names = FALSE
            ),
            Behavior = unlist(
              purrr::map(
                self$model$agents, 
                \(a) as.character(a$get_behavior())
              ), 
              use.names = FALSE
            ),
            Fitness = unlist(
              purrr::map(
                self$model$agents, 
                \(a) a$get_fitness()
              ), 
              use.names = FALSE
            ),
            label = self$label
          )
        ) # End observation update.
        
        # Stop when stop function returns TRUE or max steps reached.
        if (is.function(stop)) {
          if (stop(self$model)) {
            break
          }
        } else if (step >= stop) {
          break
        }
      }
      
      behaviors <- unlist(
        purrr::map(
          self$model$agents, \(a) as.character(a$get_behavior())
        ), 
        use.names = FALSE
      )
      
      self$outcomes$adaptation_success <- 
        length(unique(behaviors)) == 1 && 
          unique(behaviors) == adaptive_behavior
      
      self$outcomes$fixation_steps <- step

      invisible (self)
    },
    
    #' Add or update metadata in a Trial object
    #'
    #' @param self The Trial object
    #' @param new_metadata A named list to merge into existing metadata
    add_metadata = function(new_metadata) {
      self$metadata <- modifyList(self$metadata, new_metadata)
      invisible(self)
    },
    
    #' @description Return the trial's metadata as a named list.
    #' @return A named list containing metadata values for this trial, including
    #' any scalar or function-valued inputs specified during setup.
    get_metadata = function() {
      return (self$metadata)
    },
    
    #' @description Return the observation data
    get_observations = function() {
      return (self$observations)
    },
    
    #' @description Return outcome measures
    get_outcomes = function() {
      return (self$outcomes)
    },
    
    #' @description Return the label for this trial (if set)
    get_label = function() {
      return (self$label)
    }
  )
)


#' Predicate function: has the model fixated on a single behavior?
#'
#' @param model An AgentBasedModel
#' @return TRUE if all agents have the same behavior
#' @export
#' @examples
#' agents <- list(
#'   Agent$new(name = "1", behavior = "Legacy", fitness = 1),
#'   Agent$new(name = "2", behavior = "Adaptive", fitness = 4)
#' )
#' net <- igraph::make_graph(~ 1-2)
#' model <- AgentBasedModel$new(agents = agents, graph = net)
#' trial <- run_trial(model, stop = fixated) # <- "stop trial when fixated"
fixated <- function(model) {
  behaviors <- unlist(
    purrr::map(model$agents, 
               \(a) as.character(a$get_behavior())), 
               use.names = FALSE)
  
  length(unique(behaviors)) == 1
}


#' Trial runner helper function
#'
#' @param model An AgentBasedModel
#' @param stop Stopping condition: max steps (int) or predicate function
#' @param adaptive_behavior The behavior treated as "adaptation success". Default is "Adaptive".
#' @param adaptive_behavior The behavior treated as "adaptation success". Default is "Adaptive".
#' @return A Trial object
#' @examples
#' agents <- list(
#'   Agent$new(name = "1", behavior = "Legacy", fitness = 1),
#'   Agent$new(name = "2", behavior = "Adaptive", fitness = 4)
#' )
#' net <- igraph::make_graph(~ 1-2)
#' model <- AgentBasedModel$new(agents = agents, graph = net)
#' trial <- run_trial(model, stop = 10)
#' @export
run_trial <- function(model,
                      stop = socmod::fixated,
                      legacy_behavior = "Legacy",
                      adaptive_behavior = "Adaptive",
                      metadata = list()) {
  
  # Initialize, run, and return a new Trial object.
  return (
    Trial$new(model = model, metadata = metadata)$run(
        stop = stop, legacy_behavior = legacy_behavior,
        adaptive_behavior = adaptive_behavior
      )
  )
}


#' Run a grid of trial ensembles with parameter metadata
#'
#' Runs trial ensembles across a parameter grid. All scalar and function-valued parameters
#' used in model construction or trial dynamics are included in metadata for transparency.
#' @param model_generator Function that returns a new AgentBasedModel instance according to model_parameters, a named list of parameter label-value pairs.
#' @param n_trials_per_param Number of trials per parameter combination.
#' @param stop Stopping condition (number or function).
#' @param .progress Whether to show progressbar when running the trials.
#' @param ... List of parameter label-value pairs; vector or singleton values.
#'
#' @return A list of Trial objects 
#' @examples
#' agents = c(Agent$new(1), Agent$new(2))
#' mod_gen <- function(mparam_list) { 
#'   return (
#'     make_abm(
#'       make_model_parameters(
#'         # The first three positional ModelParameters fields go first.
#'         success_biased_learning_strategy, graph,
#'         # Then any auxiliary label-value pairs may be flexibly added here.
#'         adaptive_fitness = mparam_list$adaptive_fitness
#'       ), 
#'       agents = agents
#'     )
#'   )
#' }
#' # Run 2 trials per parameter setting, stopping after 10 time steps. 
#' trials <- run_trials(mod_gen, n_trials_per_param = 2, stop = 10,
#'   learning_strategy = success_bias_learning_strategy,
#'   adaptive_fitness = c(0.8, 1.0, 1.2)
#' )  # With this we'll have six total trials, two for each adaptive_fitness.
#' @export
run_trials <- function(model_generator, n_trials_per_param = 10,
                       stop = 10, .progress = TRUE, 
                       syncfile = NULL, overwrite = FALSE, ...) {
  
  # Check if syncfile is given...
  if (!is.null(syncfile)) {
    # ...and load it if it exists and we aren't overwriting existing
    if (file.exists(syncfile) && !overwrite) {
      cat("\nLoading trials from syncfile:", syncfile, "\n\n")
      load(syncfile)
      return (trials)
    }
  }

  # Initialize dataframe where each row is a set of model parameters
  model_parameters <- c(list(...), 
                        list(replication_id = 1:(n_trials_per_param)))
  
  parameter_grid <- tidyr::crossing(!!!model_parameters)
  
  legacy_behavior <- "Legacy"
  adaptive_behavior <- "Adaptive"
  if ("legacy_behavior" %in% model_parameters) {
    legacy_behavior <- model_parameters$legacy_behavior
  }
  if ("adaptive_behavior" %in% model_parameters) {
    adaptive_behavior <- model_parameters$adaptive_behavior
  }

  # Create a list of trials, each trial initialized with a param list from the grid 
  trials <- purrr::pmap(
    parameter_grid, function(...) {
      param_row <- list(...)
      model <- model_generator(param_row)
      run_trial( 
        model, stop, legacy_behavior, adaptive_behavior, 
        metadata = list(replication_id = param_row$replication_id)
      )
    }, 
    .progress = .progress
  )

  # Check if syncfile is given...
  if (!is.null(syncfile)) {
    # ...and write if it hasn't been synced or overite is TRUE
    if (!file.exists(syncfile) || overwrite) {
      save(trials, file = syncfile)
    }
  }

  return (trials)
}


