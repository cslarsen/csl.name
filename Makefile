jekyll=jekyll

default: build

doctor:
	$(jekyll) doctor

_site:
	$(jekyll) build --lsi --trace

build: _site

serve:
	$(jekyll) serve --lsi --watch

minify: build
	yuicompressor _site/css/bootstrap.css -o _site/css/bootstrap.css

compress: build
	find _site -name '*.html' -print0 \
		| parallel --no-notice -0 perl -pi -e 's/\\.css/\\.css\\.gz/gi'
	find _site \( -name '*.html' -or -name '*.css' \) -print0 \
		| parallel --no-notice -0 gzip -9

dist: doctor build minify compress
	rsync -avz --delete _site/. -e ssh foobar:/home/public

clean:
	rm -rf _site/
