all: html R

html:
	R --no-save --no-restore -e "library(rmarkdown); render(\"AirlinesDemo.Rmd\", html_document(keep_md=TRUE, toc=TRUE))"

R:
	R --no-save --no-restore -e "library(knitr); purl(\"AirlinesDemo.Rmd\")"
