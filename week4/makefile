# Comments that begin with ## will be shown from target help

## This is template about how to use make automate the workflow of R project

# Make sure that Rscript.exe and pandoc are in your path (see Readmd.md for directions)

.PHONY: list help all
help : 
	@echo "Showing Help Comments:"
	@echo
	@sed -n 's/^##//p' makefile
	@printf "\nList of all targets: "
	@$(MAKE) -s list

# List targets (http://stackoverflow.com/a/26339924/3429373)
list:
	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$' | xargs

articles_business.tsv:
	python3 get_nyt_articles.py 7ec33b8ec364412f8a5facbece0cc554 business 1000

articles_world.tsv:
	python3 get_nyt_articles.py 7ec33b8ec364412f8a5facbece0cc554 world 1000

outputs.RData: classify_nyt_articles.R articles_business.tsv articles_world.tsv
	Rscript.exe classify_nyt_articles.R

writeup.html: writeup.Rmd outputs.RData
	Rscript.exe -e "rmarkdown::render('writeup.Rmd')"

all: writeup.html
