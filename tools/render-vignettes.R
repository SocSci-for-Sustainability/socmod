# tools/render-vignettes.R

# Force rebuild of all vignettes via quarto
if (requireNamespace("quarto", quietly = TRUE)) {
  quarto::quarto_render("vignettes/", as_job = F)
} else {
  message("quarto package is not installed.")
}
