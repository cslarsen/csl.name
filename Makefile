build:
	jekyll build --lsi

serve:
	jekyll serve --watch --lsi

dist: build
	rsync -avz --delete _site/. -e ssh csl_foobar@ssh.phx.nearlyfreespeech.net:/home/public
