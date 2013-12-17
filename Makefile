build:
	@echo '### Building site'
	jekyll build --lsi

serve:
	jekyll serve --watch --lsi

minify: build
	@echo '### Minifying CSS'
	yuicompressor css/main.css > _site/css/main.css
	yuicompressor css/syntax.css > _site/css/syntax.css

compress: build
	@echo '### Update HTML and CSS links to compressed versions'
	find _site -name '*.html' -exec perl -pi -e 's/\.html/\.html\.gz/gi' {} \;
	find _site -name '*.html' -exec perl -pi -e 's/\.css/\.css\.gz/gi' {} \;

	@echo '### Compressing HTML and CSS files'
	find _site -name '*.html' -or -name '*.css' -type f -exec gzip {} \;

dist: build minify compress
	@echo '### Publishing site'
	rsync -avz --delete _site/. -e ssh foobar:/home/public

clean:
	rm -rf _site/
