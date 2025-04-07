#' @keywords internal
#' @import R6
#' @import ggplot2
#' @import ggnetwork
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom rlang on_load local_use_cli new_environment
#' @importFrom dplyr count slice_max bind_rows 
#' @importFrom purrr map map_vec map_dfr
#' @importFrom tibble tibble
#' @importFrom igraph make_empty_graph add_edges delete_vertices degree ecount V neighbors are_adjacent

#' @importFrom R6 R6Class
"_PACKAGE"

# We're importing vctrs without `data_frame()` because we currently
# reexport the deprecated `tibble::data_frame()` function

# on_load(local_use_cli())

# Singletons
the <- rlang::new_environment()
