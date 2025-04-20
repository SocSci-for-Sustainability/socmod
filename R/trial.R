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
    # partner_selection = NULL,
    # interaction = NULL,
    # iterate = NULL,
    # label = NULL,
    
    
    #' @description Initialize a Trial with a model and functions
    #' @param model An AgentBasedModel instance
    #' @param partner_selection Optional partner selection function
    #' @param interaction Function defining interaction logic
    #' @param iterate Optional iteration update function
    #' @param label Optional character label for trial group
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
    run = function(stop = 50, legacy_behavior = "Legacy", adaptive_behavior = "Adaptive") {
      
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
      
      # Get learning and iteration functions from the model's
      # learning strategy.
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
#' # fixated(model) — typically used as a stopping rule in Trial$run(stop = fixated)
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
#' @param partner_selection Function for selecting interaction partner
#' @param interaction Function for modifying agents based on partner
#' @param iterate Optional function for updating model state
#' @param stop Stopping condition: max steps (int) or predicate function
#' @param label Optional label to tag the trial (e.g. "success", "frequency")
#' @param adaptive_behavior The behavior treated as "adaptation success". Default is "Adaptive".
#' @return A Trial object
#' @export
#' @examples
#' agents <- list(
#'   Agent$new(name = "1", behavior = "Legacy", fitness = 1),
#'   Agent$new(name = "2", behavior = "Adaptive", fitness = 4)
#' )
#' net <- igraph::make_graph(~ 1-2)
#' model <- AgentBasedModel$new(agents = agents, graph = net)
#' trial <- run_trial(model, stop = 10)
run_trial <- function(model,
                      stop = 50,
                      legacy_behavior = "Legacy",
                      adaptive_behavior = "Adaptive",
                      metadata = list()) {
  
  # If learning_strategy is not null, add the strategy label to the metadata and
  # override the interaction and partner_selection functions passed as arguments.
  # if (!is.null(model$get_learning_strategy())) {
  #   metadata$learning_strategy <- learning_strategy$get_label()
  #   partner_selection <- learning_strategy$get_partner_selection()
  #   interaction <- learning_strategy$get_interaction()
  # }
  
  # 
  

  return (
    Trial$new(model = model, metadata = metadata)$run(
        stop = stop, legacy_behavior = legacy_behavior, 
        adaptive_behavior = adaptive_behavior
      )
  )
}


#' Run multiple trials with a model generator
#'
#' @param n Number of trials
#' @param model_generator A function that returns a fresh AgentBasedModel
#' @param label Optional label to attach to each trial
#' @param ... Additional arguments passed to each run_trial()
#' @return A list of Trial objects
#' @export
#' @examples
#' gen <- function() {
#'   agents <- list(
#'     Agent$new(name = "1", behavior = "Legacy", fitness = 1),
#'     Agent$new(name = "2", behavior = "Adaptive", fitness = 4)
#'   )
#'   net <- igraph::make_graph(~ 1-2)
#'   AgentBasedModel$new(agents = agents, graph = net)
#' }
#' trials <- run_trials(3, gen, label = "success", stop = 10)
run_trials <- function(n_trials, model_generator, ...) {
  purrr::map(seq_len(n_trials), function(i) {
    model <- model_generator()
    run_trial(model, label = label, ...)
  })
}


#' Run a grid of trial ensembles with parameter metadata
#'
#' Runs trial ensembles across a parameter grid. All scalar and function-valued parameters
#' used in model construction or trial dynamics are included in metadata for transparency.
#'
#' @param n_trials_per_param Number of trials per parameter combination.
#' @param model_generator Returns a new AgentBasedModel instance according to model_parameters.
#' @param stop Stopping condition (number or function).
#' 
#' @param partner_selection Function or list of functions to select learning partners.
#' @param interaction Function or list of functions to apply during agent interaction.
#' @param model_iterate Function to iterate the model.
#'
#' @return A list of Trial objects 
#' @export
run_trials_grid <- function(model_generator, n_trials_per_param = 10,
                            stop = 10, ...) {
  auxiliary_parameters <- list(...)
  # Initialize dataframe where each row is a set of model parameters.
  model_parameters <- c(auxiliary_parameters, 
                        list(replication_id = 1:(n_trials_per_param)))
  
  parameter_grid <- tidyr::crossing(!!!model_parameters)
  
  legacy_behavior <- "Legacy"
  adaptive_behavior <- "Adaptive"
  if ("legacy_behavior" %in% auxiliary_parameters) {
    legacy_behavior <- auxiliary_parameters$legacy_behavior
  }
  if ("adaptive_behavior" %in% auxiliary_parameters) {
    adaptive_behavior <- auxiliary_parameters$adaptive_behavior
  }
  # Create a list of trials 
  trials <- purrr::pmap(parameter_grid, function(...) {
    run_trial(
      model_generator(model_parameters), stop, 
      legacy_behavior, adaptive_behavior 
    )
  })
  
  # # Currently trials is a list of lists. 
  # return (purrr::flatten(trials))

  return (trials)
}


#' Summarize behavior adoption over time from multiple trials
#'
#' @param trials A list of Trial objects
#' @param tracked_behaviors Optional vector of behaviors to include
#' @return A tibble with columns: trial, t, behavior, count, label, adaptation_success, fixation_steps
#' @export
summarise_adoption <- function(trials, tracked_behaviors = NULL) {
  purrr::map2_dfr(trials, seq_along(trials), function(trial, i) {
    obs <- trial$get_observations()
    outcome <- trial$get_outcomes()
    
    if (!is.null(tracked_behaviors)) {
      obs <- dplyr::filter(obs, Behavior %in% tracked_behaviors)
    }
    label <- trial$get_label()
    dplyr::group_by(obs, t, Behavior) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(
        trial = i,
        label = label,
        adaptation_success = outcome$adaptation_success,
        fixation_steps = outcome$fixation_steps
      )
  })
}


#' Summarise trials by metadata fields
#'
#' @param trials A list of Trial objects
#' @param fields Character vector of metadata fields to group by
#'
#' @return A data frame with group means of success and steps
#' @export
summarise_by_metadata <- function(trials, fields) {
  df <- purrr::map_dfr(trials, function(trial) {
    row <- as.list(trial$metadata[fields])
    row$adaptation_success <- 
      trial$get_outcomes()$adaptation_success
    row$fixation_steps <- trial$outcomes$fixation_steps
    tibble::as_tibble(row)
  })
  
  return (
    df %>%
      dplyr::group_by(across(all_of(fields))) %>%
      dplyr::summarise(
        success_rate = mean(adaptation_success),
        mean_fixation_steps = mean(fixation_steps),
        .groups = "drop"
      )
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
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
  
  ggplot2::ggplot(obs_filtered, ggplot2::aes(x = t, y = count, color = Behavior)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::xlab("Time step") +
    ggplot2::ylab("Agent count") +
    ggplot2::theme_classic() +
    ggplot2::scale_color_brewer(palette = "Set1")
}


#' Plot a summary of behavior adoption over time across trials
#'
#' @param summary_df A tibble from summarise_adoption()
#' @return A ggplot object
#' @export
plot_summary <- function(summary_df) {
  ggplot2::ggplot(summary_df, ggplot2::aes(x = t, y = count, color = Behavior)) +
    ggplot2::geom_line(ggplot2::aes(group = interaction(trial, Behavior)), alpha = 0.3) +
    ggplot2::facet_wrap(~ label) +
    ggplot2::xlab("Time step") +
    ggplot2::ylab("Agent count") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_brewer(palette = "Set1")
}
