
# florentine_seed_analysis.R
library(socmod)
library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# Define seeders
seeders <- list(
  medici_pazzi = function() c("Medici", "Pazzi"),
  medici_strozzi = function() c("Medici", "Strozzi")
)

# Model generator that accepts f_A and seed_set
make_florentine_model_generator <- function(f_A, seed_set) {
  function() {
    net <- igraph::make_graph(
      c("Acciaiuoli", "Medici",
        "Castellani", "Peruzzi",
        "Strozzi", "Peruzzi",
        "Strozzi", "Guadagni",
        "Strozzi", "Medici",
        "Albizzi", "Ginori",
        "Albizzi", "Guadagni",
        "Guadagni", "Bischeri",
        "Guadagni", "Medici",
        "Medici", "Bischeri",
        "Medici", "Ridolfi",
        "Medici", "Tornabuoni",
        "Medici", "Barbadori",
        "Medici", "Salviati",
        "Salviati", "Pazzi",
        "Pazzi", "Bischeri"),
      directed = FALSE
    )

    agents <- purrr::map(igraph::V(net)$name, function(id) {
      Agent$new(
        id = id,
        name = id,
        behavior = "Legacy",
        fitness = 1
      )
    })

    abm <- AgentBasedModel$new(agents = agents, graph = net)

    seed_ids <- seeders[[seed_set]]()
    for (id in seed_ids) {
      abm$get_agent(id)$set_behavior("Adaptive")
      abm$get_agent(id)$set_fitness(f_A)
    }

    return (abm)
  }
}

# Run the trials
adaptive_fitnesses <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0)

results <- run_trials_grid(
  f_A = adaptive_fitnesses,
  seed_set = names(seeders),
  reps = 10,
  model_generator = make_florentine_model_generator,
  partner_selection = success_bias_select_teacher,
  interaction = success_bias_interact,
  iterate = iterate_learning_model,
  stop = fixated
)

# Summarize and plot
summary_df <- summarise_by_metadata(results, fields = c("f_A", "seed_set"))

plot_florentine_seed_analysis <- function(summary_df) {
  ggplot(summary_df, aes(x = f_A, y = success_rate,
                         color = seed_set, linetype = seed_set)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Florentine seed analysis",
      x = "Adaptive Fitness (f_A)",
      y = "Proportion Adopted",
      color = "Seed Set",
      linetype = "Seed Set"
    ) +
    theme_minimal(base_size = 14)
}

plot_florentine_seed_analysis(summary_df)
