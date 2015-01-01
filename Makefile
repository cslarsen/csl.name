jekyll=jekyll

default: build

doctor:
	$(jekyll) doctor

build:
	$(jekyll) build --lsi --trace

serve:
	$(jekyll) serve --drafts --host 0.0.0.0 --lsi --watch

minify: build
	yuicompressor _site/css/bootstrap.css -o _site/css/bootstrap.css

compress: build
	find _site -name '*.html' -print0 \
		| parallel --no-notice -0 perl -pi -e 's/\\.css/\\.css\\.gz/gi'
	find _site -name '*.html' -print0 \
		| parallel --no-notice -0 perl -pi -e 's/\\.svg/\\.svg\\.gz/gi'
	find _site -name '*.html' -print0 \
		| parallel --no-notice -0 perl -pi -e 's/\\.js/\\.js\\.gz/gi'
	find _site \( -name '*.html' -or \
		            -name '*.css' -or \
								-name '*.js' -or \
								-name '*.svg' \) -print0 \
		| parallel --no-notice -0 gzip -9

dist: doctor build minify compress
	rsync -avz --delete _site/. -e ssh cslarsen:/home/public

clean:
	rm -rf _site/
