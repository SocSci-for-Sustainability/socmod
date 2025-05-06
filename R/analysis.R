#' Custom color palette for scientific plots
#'
#' This palette was extracted from a provided source image and refined for use in scientific plots,
#' emphasizing high contrast and perceptual separability. Recommended for use in `scale_color_manual()`.
#'
#' @return A character vector of hex color codes
#' @export
SOCMOD_PLOT_PALETTE <- c(
  "#E24B4A",  # bright red
  "#007F7D",  # teal
  "#428BCA",  # blue
  "#A3A843",  # olive green
  "#F0C04D",  # yellow
  "#D0743C",  # orange
  "#5E3B68",  # plum
  "#BF2A40",  # deep red
  "#7D766F"   # neutral gray
)


#' Plot adoption counts of selected behaviors over time
#' Plot adoption counts of selected behaviors 
#' (`tracked_behaviors`) over time.
#'
#' @param trial A Trial object
#' @param tracked_behaviors Character vector of behaviors to track (e.g., c("Adaptive", "Legacy"))
#' @return A ggplot object
#' @export
#' @examples
plot_prevalence <- function(trials_or_tibble, tracked_behaviors = c("Adaptive")) {
  
  prevalence_tbl <- trials_or_tibble
  if (!inherits(trials_or_tibble, "tbl_df")) {
    prevalence_tbl <- summarise_prevalence(trials_or_tibble)
  }
  
  p <- 
    # If first arg not a tibble, assume trials and hand off to summarize...
    ifelse(inherits(trials_or_tibble, "tbl_df"), 
           trials_or_tibble, 
           summarise_prevalence(trials_or_tibble)) %>%  
      # Plot dynamics with 
      ggplot2::ggplot(ggplot2::aes(x = Step, y = Prevalence, color = Behavior)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_classic() +
      ggplot2::scale_color_manual(values = SOCMOD_PLOT_PALETTE)
      
}


#' Summarize behavior prevalence over time within or across trials
#'
#' This function summarizes the prevalence of tracked behaviors over time,
#' either returning a summary for each individual trial or averaging across multiple trials.
#' Prevalence is normalized by the number of agents in each trial.
#'
#' @param trials_or_trial A `Trial` object or a list of `Trial` objects
#' @param tracked_behaviors Character vector of behavior names to include in the summary.
#'   Defaults to \code{"Adaptive"}.
#' @param between_trials Logical. If TRUE (default), returns a summary aggregated across trials.
#'   If FALSE, returns per-trial prevalence values with a `trial_id` column distinguishing replicates.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{Step}{The time step (from observations)}
#'     \item{Behavior}{The behavior being tracked}
#'     \item{Count}{The number of agents exhibiting this behavior at this Step}
#'     \item{Prevalence}{The fraction of agents exhibiting this behavior (Count / n_agents)}
#'     \item{trial_id}{The trial index (only included if \code{between_trials = FALSE})}
#'     \item{<input parameters>}{One column per input parameter in the Trial's model}
#'   }
#'
#' @examples
#' mps <- make_model_parameters(n_agents = 10, adoption_rate = 1.0, learning_strategy = contagion_learning_strategy)
#' abm <- make_abm(mps)
#' trial <- make_trial(abm)
#' trial$run(steps = 5)
#'
#' # Summary aggregated across trials (default)
#' summary <- summarise_prevalence(trial, tracked_behaviors = c("A", "B"))
#' print(summary)
#'
#' # Per-trial summary without aggregation
#' summary_per_trial <- summarise_prevalence(trial, tracked_behaviors = c("A", "B"), between_trials = FALSE)
#' print(summary_per_trial)
#'
#' @export
summarise_prevalence <- function(trials_or_trial, 
                                 tracked_behaviors = 
                                   c("Adaptive"), 
                                 across_trials = TRUE) {
  
  # First handle case where data is a single Trial
  if (inherits(trials_or_trial, "Trial")) {
    trials <- list(trials_or_trial)
    # Then handle case of list of Trial instances
  } else if (is.list(trials_or_trial) && all(purrr::map_lgl(trials_or_trial, ~ inherits(.x, "Trial")))) {
    trials <- trials_or_trial
  } else {
    stop("Input must be a Trial or list of Trial objects")
  }
  
  obs1 <- trials[[1]]$get_observations()
  all_behaviors <- unique(c(tracked_behaviors, obs1$Behavior))
  prevalence_tbl <- purrr::imap_dfr(trials, \(trial, trial_index) {
    
    # Extract observations 
    obs <- 
      trial$get_observations() %>% 
      dplyr::mutate(Behavior = factor(Behavior, all_behaviors))
    
    # Build parameters list, replacing the graph and learning strategy
    # instances with their labels.
    params_list <- trial$model$get_parameters()$as_list()
    # print(c(igraph::graph_attr(params_list$graph, "label")))
    params_list$graph <- c(igraph::graph_attr(params_list$graph, "label"))
    params_list$learning_strategy <- c(params_list$learning_strategy$get_label())
    
    # Need this now for parameters and later for calculating Prevalence.
    n_agents <- trial$model$get_parameter("n_agents")
    params_list$n_agents <- n_agents
    
    params_row <- tibble::as_tibble(params_list)
    # print(params_row)
    # print(obs)
    
    # Create a within-trial summary for each trial.
    prevalence_summary <- obs %>%
      # dplyr::filter(Behavior %in% tracked_behaviors) %>%
      dplyr::group_by(Step, Behavior) %>%
      dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(Prevalence = Count / n_agents) %>%
      tidyr::complete(Step, Behavior, fill = list(Count = 0, Prevalence = 0)) %>%
      dplyr::bind_cols(
        # Although this is a one-row tibble, the values will be repeated to fill
        # in however many rows are present in prevalence_summary up to this point.
        params_row
      )
    
    # Assign trial index as ID
    prevalence_summary$trial_id <- trial_index
    
    return (prevalence_summary)
  })
  
  # return (prevalence_tbl)
  # print(prevalence_tbl)
  if (across_trials) {
    # Prepare grouping variables: convert parameter names to 
    # symbols for tidy evaluation
    group_params <- names(
      trials[[1]]$model$get_parameters()$as_list()
    ) %>% rlang::syms()
    
    prevalence_tbl <- prevalence_tbl %>%
      # Dynamically group by Step, Behavior, and all parameter columns
      dplyr::group_by(Step, Behavior, !!!group_params) %>%
      dplyr::summarise(
        Prevalence = mean(Prevalence),
        Count = mean(Count),
        .groups = "drop"
      )
  }
  
  return (prevalence_tbl)
}



#' Summarize outcomes across trials by input parameters
#'
#' This function summarizes trial-level outcomes by grouping across input parameters.
#' It computes the mean of specified outcome measures across all trials sharing the same input parameter values.
#'
#' @param trials A list of `Trial` objects
#' @param input_parameters Character vector of parameter names to group by
#' @param outcome_measures Character vector of outcome variable names to summarize
#'
#' @return A tibble with one row per unique combination of input parameters,
#'   containing the mean of each specified outcome measure.
#'
#' @examples
#' mps <- make_model_parameters(n_agents = 10, adoption_rate = 1.0, learning_strategy = contagion_learning_strategy)
#' abm <- make_abm(mps)
#' trial <- make_trial(abm)
#' trial$run(steps = 5)
#'
#' trials <- list(trial, trial)
#'
#' summary <- summarise_outcomes(trials, input_parameters = "adoption_rate", outcome_measures = "success_rate")
#' print(summary)
#' @export
summarise_outcomes <- function(trials, input_parameters, outcome_measures) {
  assertthat::assert_that(
    is.list(trials),
    all(purrr::map_lgl(trials, ~ inherits(.x, "Trial")))
  )
  
  # outcomes <- purrr::map_dfr(trials, function(trial) {
  #   trial$get_outcomes()
  # }, .id = "trial_id")
  
  
  outcomes <- purrr::imap_dfr(trials, function(trial, trial_index) {
    
    param_list <- trial$model$get_parameters()$as_list() 
    param_list$learning_strategy <- param_list$learning_strategy$get_label()
    graph_label <- igraph::graph_attr(param_list$graph, "label")
    param_list$graph <- param_list$graph_label
    
    
    row <- trial$model$get_parameters()$as_list()[input_parameters]
    row$adaptation_success <- trial$get_outcomes()$adaptation_success
    
    row$fixation_steps <- trial$get_outcomes()$fixation_steps
    row$trial_id <- trial_index
    
    return (tibble::as_tibble(row))
  })
  
  summary <- 
    dplyr::group_by(outcomes, across(all_of(input_parameters))) %>%
    dplyr::summarise(
      success_rate = mean(adaptation_success),
      mean_fixation_steps = mean(fixation_steps),
      .groups = "drop"
    )
  
  summary <- summary %>%
    tidyr::pivot_longer(all_of(outcome_measures),
                        names_to = "Measure",
                        values_to = "Value")
  
  return (summary)
}


