render:
	quarto render

run: render
	Rscript -e "shiny::runApp('bvq-app')"

renv:
	Rscript -e "renv::restore()"
	
targets:
	Rscript -e "targets::tar_make()"

docker-build:
	docker build -t bvq-app .

docker-run:
	docker run --rm -p 3838:3838 bvq-app