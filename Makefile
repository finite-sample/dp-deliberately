PACKAGE := $(shell Rscript -e 'cat(read.dcf("DESCRIPTION")[1,"Package"])')
VERSION := $(shell Rscript -e 'cat(read.dcf("DESCRIPTION")[1,"Version"])')
R_IMAGE ?= rocker/r2u:latest

.PHONY: deps document test lint build check ci ci-docker examples docs

deps:
	Rscript -e 'install.packages("remotes", repos="https://cloud.r-project.org"); remotes::install_deps(dependencies=TRUE, upgrade="never")'

document:
	Rscript -e 'roxygen2::roxygenise()'

test:
	Rscript -e 'testthat::test_local(stop_on_failure=TRUE)'

lint:
	Rscript -e 'pkgload::load_all(quiet=TRUE); x <- lintr::lint_package(); print(x); if (length(x)) quit(status=1)'

build:
	R CMD build .

check: build
	R CMD check --no-manual $(PACKAGE)_$(VERSION).tar.gz

ci: lint test check

ci-docker:
	docker run --rm -v "$(CURDIR):/work" -w /work $(R_IMAGE) sh -c 'apt-get update && apt-get install -y pandoc && make deps && make ci'

examples:
	R CMD INSTALL .
	Rscript inst/examples/synthetic.R

docs:
	Rscript -e 'pkgdown::build_site()'
