build:
	jekyll build

serve:
	jekyll serve --watch

dist: build
	rsync -avz --delete _site/. -e ssh csl_foobar@ssh.phx.nearlyfreespeech.net:/home/public
