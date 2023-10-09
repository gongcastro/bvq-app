run:
	@echo "Running Shiny app..."
	@Rscript -e "shiny::runApp('app', launch.browser = TRUE)"

docker-build:
	@docker build -t bvq-app .

docker-run:
	@docker run --rm -p 3838:3838 bvq-app

bvq:
	Rscript -e "source('R/bvq.R'); get_bvq()"
