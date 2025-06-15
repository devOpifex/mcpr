default: install

document:
	R -s -e "devtools::document()"

check: site
	R -s -e "devtools::check(document = FALSE)"

install: check
	R -s -e "devtools::install()"

site: document
	R -s -e "pkgdown::build_site()"
