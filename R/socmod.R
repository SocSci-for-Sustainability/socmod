#' @keywords internal
#' @import rlang 
#' @import R6
#' @import ggplot2
#' @import ggnetwork
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom dplyr count slice_max bind_rows 
#' @importFrom purrr map map_vec
#' @importFrom tibble tibble
#' @importFrom igraph make_empty_graph add_edges delete_vertices degree ecount V neighbors

#' @importFrom R6 R6Class
"_PACKAGE"

# We're importing vctrs without `data_frame()` because we currently
# reexport the deprecated `tibble::data_frame()` function

on_load(local_use_cli())

# Singletons
the <- new_environment()
