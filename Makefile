docker_build:
	docker build -t bvq-app .

docker_container:
	docker run --rm -p 3838:3838 bvq-app