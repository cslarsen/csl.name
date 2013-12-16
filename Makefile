build:
	jekyll build --lsi

serve:
	jekyll serve --watch --lsi

minify: build
	yuicompressor css/main.css > _site/css/main.css
	yuicompressor css/syntax.css > _site/css/syntax.css

compress: build
	# create new hrefs
	find _site -name '*.html' -exec perl -pi -e 's/\.html/\.html\.gz/gi' {} \;
	find _site -name '*.html' -exec perl -pi -e 's/\.css/\.css\.gz/gi' {} \;
	# compress files
	find _site -name '*.html' -type f -exec gzip {} \;
	find _site -name '*.css' -type f -exec gzip {} \;

dist: build minify compress
	rsync -avz --delete _site/. -e ssh foobar:/home/public

clean:
	rm -rf _site/
