jekyll=jekyll

default: build

doctor:
	$(jekyll) doctor

build:
	$(jekyll) build --lsi --trace

serve:
	$(jekyll) serve --lsi --watch

minify: build
	yuicompressor _site/css/main.css -o _site/css/main.css
	yuicompressor _site/css/syntax.css -o _site/css/syntax.css

compress: build
	find _site -name '*.html' -exec perl -pi -e 's/\.html/\.html\.gz/gi' {} \;
	find _site -name '*.html' -exec perl -pi -e 's/\.css/\.css\.gz/gi' {} \;
	find _site -name '*.html' -type f -exec gzip -9 {} \;
	find _site -name '*.css' -type f -exec gzip -9 {} \;

dist: doctor build minify compress
	rsync -avz --delete _site/. -e ssh foobar:/home/public

clean:
	rm -rf _site/
