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
	docker run --rm -it \
		-v `pwd`/source:/source \
		csl.name make -j -C /source doctor minify

serve:
	docker run --rm -it \
		-p 4000:4000 \
		-v `pwd`/source:/source \
		-v `pwd`/source/_drafts:/source/_drafts \
		-v `pwd`/source/_posts:/source/_posts \
		csl.name make -j -C /source serve
