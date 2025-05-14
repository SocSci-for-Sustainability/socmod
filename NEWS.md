# socmod 0.2.2 (2025-05-14)

- `summarise_*` now work even if there are vectors/lists in parameters by
  removing them via helper `.clean_summary_params`

# socmod 0.2.1 (2025-05-13)

- Refined analysis helpers with updated, passing tests
- Updated README.Rmd with tools/make-figures-readme.R to simplify home/index 

# socmod 0.2.0 (2025-05-07)

- First full vignette-based pipeline working
- Added `initialize_agents()`
- Quarto-compatible documentation with reproducible examples

# socmod 0.1.0 (2025-04-15)

- Initial package release with working architecture for agent-based modeling
- Includes Agent, AgentBasedModel, Trial, and ModelParameters classes
- Implements success-biased, frequency-biased, and contagion learning strategies
- Core simulation functions: run_trial(), run_trials(), and stop conditions
- Basic plotting and outcome summarization: plot_prevalence(), summarise_by_metadata()
- Internal testing framework and documentation scaffolding

