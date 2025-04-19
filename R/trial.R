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
    partner_selection = NULL,
    interaction = NULL,
    iterate = NULL,
    label = NULL,
    metadata = list(),
    
    #' @description Initialize a Trial with a model and functions
    #' @param model An AgentBasedModel instance
    #' @param partner_selection Optional partner selection function
    #' @param interaction Function defining interaction logic
    #' @param iterate Optional iteration update function
    #' @param label Optional character label for trial group
    initialize = function(model, 
                          partner_selection = NULL, 
                          interaction, iterate = NULL, 
                          label = NULL, metadata = list()) {
      self$model <- model
      self$partner_selection <- partner_selection
      self$interaction <- interaction
      self$iterate <- iterate
      self$label <- label
      self$metadata <- metadata
      self$observations <- tibble::tibble()
      self$outcomes <- list()
    },
    
    #' @description Run the model and collect results
    #' @param stop Either integer for max steps, or predicate function
    #' @param target_behavior The behavior treated as "adaptation success"
    run = function(stop = 50, target_behavior = "Adaptive") {
      step <- 0
      # Record t = 0 before any updates
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
      
      while (TRUE) {
        
        step <- step + 1
        
        for (agent in self$model$agents) {
          teacher <- NULL
          if (!is.null(self$partner_selection)) {
            teacher <- self$partner_selection(agent, self$model)
          }
          self$interaction(agent, teacher, self$model)
        }
        
        if (!is.null(self$iterate)) {
          self$iterate(self$model)
        }
        
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
        )
        
        if (is.function(stop)) {
          if (stop(self$model)) {
            break
          }
        } else if (step >= stop) {
          break
        }
      }
      
      behaviors <- unlist(
        purrr::map(self$model$agents, \(a) as.character(a$get_behavior())), 
        use.names = FALSE
      )
      
      self$outcomes$adaptation_success <- 
        length(unique(behaviors)) == 1 && unique(behaviors) == target_behavior
      
      self$outcomes$fixation_steps <- step
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


#' Run a Trial on an AgentBasedModel with standard learning loop
#'
#' @param model An AgentBasedModel
#' @param partner_selection Function for selecting interaction partner
#' @param interaction Function for modifying agents based on partner
#' @param iterate Optional function for updating model state
#' @param stop Stopping condition: max steps (int) or predicate function
#' @param label Optional label to tag the trial (e.g. "success", "frequency")
#' @param target_behavior The behavior treated as "adaptation success". Default is "Adaptive".
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
                      partner_selection = success_bias_teacher_selection,
                      interaction = success_bias_interact,
                      learning_strategy = NULL,
                      iterate = iterate_learning_model,
                      stop = 50,
                      label = NULL,
                      target_behavior = "Adaptive",
                      metadata = list()) {
  
  # If learning_strategy is not null, add the strategy label to the metadata and
  # override the interaction and partner_selection functions passed as arguments.
  if (!is.null(learning_strategy)) {
    metadata$learning_strategy <- learning_strategy$get_label()
    partner_selection <- learning_strategy$get_partner_selection()
    interaction <- learning_strategy$get_interaction()
  }
  
  # 
  trial <- Trial$new(
    model = model,
    partner_selection = partner_selection,
    interaction = interaction,
    iterate = iterate,
    metadata = metadata
  )
  trial$run(stop = stop, target_behavior = target_behavior)
  return (trial)
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
run_trials_grid <- function(n_trials_per_param,
                            model_generator,
                            stop = 10,
                            learning_strategies = NULL,
                            auxlilary_parameters = list(),
                            partner_selection = 
                              success_biased_teacher_selection,
                            interaction = success_biased_interaction,
                            model_iterate = iterate_learning_model) {
  
  # Set up learning_strategies and include in model_parameters if given.
  if (!is.null(learning_strategies)){
    # If learning_strategies is a singleton, put it in a list for crossing.
    if (inherits(learning_strategies, "LearningStrategy")) {
      learning_strategies <- list(LearningStrategy = learning_strategies)
    } else {
        # Otherwise first check that it's a list, stop if it's not.
        assertthat::assert_that(
          is.list(learning_strategies),
          msg = "learning_strategies must be a single LearningStrategy instance or a list of LearningStrategy instances."
        )
    }
    
    model_parameters$LearningStrategy <- learning_strategies
  }
  
  # Initialize dataframe where each row is a set of model parameters.
  parameter_grid <- tidyr::crossing(!!!model_parameters)
  parameter_grid$id <- 1:nrow(parameter_grid)
  
  # Create a list of trials 
  trials <- purrr::pmap(parameter_grid, function(...) {
    run_trial(
      model = model_generator(model_parameters),
      id = id
    )
  })
  
  # # Currently trials is a list of lists. 
  # return (purrr::flatten(trials))
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
