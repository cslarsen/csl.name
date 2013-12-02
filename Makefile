build:
	jekyll build --lsi

serve:
	jekyll serve --watch --lsi

dist: build
	rsync -avz --delete _site/. -e ssh foobar:/home/public

clean:
	rm -rf _site/
