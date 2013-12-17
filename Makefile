doctor:
	jekyll doctor

build:
	jekyll build --lsi --trace

serve:
	jekyll serve --lsi --watch

minify: build
	yuicompressor css/main.css > _site/css/main.css
	yuicompressor css/syntax.css > _site/css/syntax.css

compress: build
	find _site -name '*.html' -exec perl -pi -e 's/\.html/\.html\.gz/gi' {} \;
	find _site -name '*.html' -exec perl -pi -e 's/\.css/\.css\.gz/gi' {} \;
	find _site -name '*.html' -or -name '*.css' -type f -exec gzip {} \;

dist: doctor build minify compress
	rsync -avz --delete _site/. -e ssh foobar:/home/public

clean:
	rm -rf _site/
