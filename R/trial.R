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
          t = 0,
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
            t = step,
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
                      stop = 50,
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
    # ...and load it if it exists and we aren't overwriting existing.
    if (file.exists(syncfile) && !overwrite) {
      cat("\nLoading trials from syncfile:", syncfile, "\n\n")
      load(syncfile)
      return (trials)
    }
  }

  # Initialize dataframe where each row is a set of model parameters.
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

  # Create a list of trials, each trial initialized with a param list from the grid. 
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
    # ...and write if it hasn't been synced or overite is TRUE.
    if (!file.exists(syncfile) || overwrite) {
      save(trials, file = syncfile)
    }
  }

  return (trials)
}


#' Summarize behavior adoption over time from multiple trials
#'
#' @param trials A list of Trial objects
#' @param tracked_behaviors Optional vector of behaviors to include
#' @return A tibble with columns: trial, t, behavior, count, label, adaptation_success, fixation_steps
#' @export
summarise_prevalence <- function(trials, tracked_behaviors = NULL) {
  
  # Calculate the behavior counts for each trial's observations:
  count_observations <- purrr::map_dfr(trials, function(trial) {
    
    # Extract observations. 
    observations <- trial$get_observations()
    
    # If tracked_behaviors is provided, select 
    # observations for only those behaviors.
    if (!is.null(tracked_behaviors)) {
      observations <- dplyr::filter(observations, Behavior %in% tracked_behaviors)
    }
    
    # Count number of agents doing tracked behaviors.
    return (
      dplyr::group_by(observations, t, Behavior) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop") 
    )
  })
  
  # Now calculate and return the mean prevalence series of tracked behaviors.
  # 
  # First get n_agents model param.
  trial$model$get_parameter("n_agents")
  
  # Return mean prevalence from count observations grouped by time and behavior.
  return (
    count_observations %>%
      dplyr::group_by(t, Behavior) %>%
      dplyr::summarise(`Mean prevalence` = mean(count) / n_agents)
  )
}


#' Summarise trials by metadata fields
#'
#' @param trials A list of Trial objects to summarise
#' @param input_parameters Character vector of model parameter fields to group by
#' @param outcome_measures A character vector of outcome measures of interest
#' @return A data frame with group means of success and steps
#' @export
summarise_by_parameters <- function(trials, 
                                    input_parameters, 
                                    outcome_measures = 
                                      c("success_rate", 
                                        "mean_fixation_steps")) {
  
  df <- purrr::map_dfr(trials, function(trial) {
    
    # Initialize "row" (actually a list for now) of desired input model params.
    row <- as.list(trial$model$get_parameters()[input_parameters])
    
    # Convert learning_strategy parameter to its label and add to the "row".
    if ("learning_strategy" %in% names(row)) {
      row$learning_strategy <- row$learning_strategy$get_label()
    }
    
    # Add the adaptation success for this row from the trial outcomes.
    row$adaptation_success <- trial$get_outcomes()$adaptation_success
    
    # Add the fixation steps for this row from the trial outcomes
    row$fixation_steps <- trial$get_outcomes()$fixation_steps
    tibble::as_tibble(row)
  })
  
  return (
    df %>%
      dplyr::group_by(across(all_of(input_parameters))) %>%
      dplyr::summarise(
        success_rate = mean(adaptation_success),
        mean_fixation_steps = mean(fixation_steps),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(all_of(outcome_measures), 
                          names_to = "Measure", 
                          values_to = "Value")
  )
}


#' Plot adoption counts of selected behaviors over time
#' Plot adoption counts of selected behaviors 
#' (`tracked_behaviors`) over time.
#'
#' @param trial A Trial object
#' @param tracked_behaviors Character vector of behaviors to track (e.g., c("Adaptive", "Legacy"))
#' @return A ggplot object
#' @export
#' @examples
plot_adoption <- function(trial, tracked_behaviors = c("Adaptive")) {
  obs <- trial$get_observations()
  
  obs_filtered <- obs %>%
    dplyr::filter(Behavior %in% tracked_behaviors) %>%
    dplyr::group_by(t, Behavior) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(t, Behavior, fill = list(count = 0))
  
  ggplot2::ggplot(obs_filtered, 
                  ggplot2::aes(x = t, y = count, color = Behavior)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::xlab("Time step") +
    ggplot2::ylab("Agent count") +
    ggplot2::theme_classic() +
    ggplot2::scale_color_brewer(palette = "Set1")
}


# ' Plot a summary of behavior adoption over time across trials
# '
# ' @param summary_df A tibble from summarise_adoption()
# ' @return A ggplot object
# ' @export
# plot_summary <- function(summary_df) {
#   ggplot2::ggplot(summary_df, ggplot2::aes(x = t, y = count, color = Behavior)) +
#     ggplot2::geom_line(ggplot2::aes(group = interaction(trial, Behavior)), alpha = 0.3) +
#     ggplot2::facet_wrap(~ label) +
#     ggplot2::xlab("Time step") +
#     ggplot2::ylab("Agent count") +
#     ggplot2::theme_minimal() +
#     ggplot2::scale_color_brewer(palette = "Set1")
# }
