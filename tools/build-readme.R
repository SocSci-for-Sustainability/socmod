# tools/rebuild-readme.R

# Clean stale figure and cache directories
unlink("README_files", recursive = TRUE, force = TRUE)
unlink("README_cache", recursive = TRUE, force = TRUE)
unlink("man/figures", recursive = TRUE, force = TRUE)

# Rebuild the README.md from README.Rmd
devtools::build_readme(quiet = FALSE)

# Rebuild the pkgdown homepage (uses README.md)
pkgdown::build_home(preview = FALSE, quiet = FALSE)
