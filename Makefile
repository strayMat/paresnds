## Build the package documentation
docs-r:
	Rscript -e 'library(devtools);pkgload::load_all();styler::style_pkg();devtools::document();devtools::check(error_on="error")'


## Build the package website
docs-html:
	Rscript -e 'library(devtools);pkgload::load_all();styler::style_pkg();devtools::document();devtools::check(error_on="error")'
	Rscript -e 'devtools::install();pkgdown::build_site()'

build:
## Build the package sources as .tar.gz
	Rscript -e 'devtools::install();devtools::build()'
	mv ../paresnds_* .