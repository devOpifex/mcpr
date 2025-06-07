default: document check install

document:
	R -s -e "devtools::document()"

check:
	R -s -e "devtools::check()"

install:
	R -s -e "devtools::install()"
