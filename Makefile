# Makefile for building socmod documentation and site

.PHONY: all docs site check install clean push preview

# Default target
all: docs

# Build vignettes and pkgdown site
docs:
	Rscript -e "pkgdown::build_site()"

# Build just the pkgdown site (skip vignette rendering)
site:
	Rscript -e "pkgdown::build_site()"

# Rebuild README.md and associated docs/index.html, opening in new window
readme:
	Rscript -e "devtools::build_readme()" \
	  && Rscript -e "pkgdown::build_home()" \
	  && open docs/index.html

# Run R CMD check
check:
	R CMD check socmod

# Install the package locally
install:
	R CMD INSTALL socmod

# Clean generated files
clean:
	rm -rf vignettes/*.html docs/

# Commit and push the updated site to GitHub
push:
	git add docs/
	git commit -m "Update site"
	git push

# Build and open site preview in default browser
preview:
	Rscript -e "quarto::quarto_render('vignettes/florentine-seed-analysis.qmd')"
	Rscript -e "pkgdown::build_site()"
	@if [ "$(shell uname)" = "Darwin" ]; then \
	  open docs/index.html; \
	elif [ "$(shell uname)" = "Linux" ]; then \
	  xdg-open docs/index.html; \
	else \
	  start docs/index.html; \
	fi
