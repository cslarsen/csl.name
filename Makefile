build:
	jekyll build --lsi

serve:
	jekyll serve --watch --lsi

compress-css: build
	echo Compressing CSS files
	yuicompressor css/main.css > _site/css/main.css
	yuicompressor css/syntax.css > _site/css/syntax.css

dist: build compress-css
	rsync -avz --delete _site/. -e ssh foobar:/home/public

clean:
	rm -rf _site/
