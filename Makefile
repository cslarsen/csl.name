jekyll := jekyll

help:
	@echo "Usage:"
	@echo "  make setup --- sets up docker image"
	@echo "  make build --- builds HTML files"
	@echo "  make serve --- builds HTML and serves local webserver"
	@echo "  make bash  --- opens docker shell"

setup:
	docker build --tag csl.name .

bash:
	docker run --rm -it csl.name /bin/bash

build:
	docker run --rm -it csl.name make -j -C /source

serve:
	docker run --rm -it -p 4000:4000 csl.name make -j -C /source serve
