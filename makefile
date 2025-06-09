default: install

document:
	R -s -e "devtools::document()"

check: document
	R -s -e "devtools::check()"

install: check
	R -s -e "devtools::install()"

site: document
	R -s -e "pkgdown::build_site()"
