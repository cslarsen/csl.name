jekyll := jekyll

default: build

help:
	@echo "Usage:"
	@echo "  make build - builds static _site/"
	@echo "  make clean"
	@echo "  make dist  - compress + publish"
	@echo "  make draft - create a draft post"
	@echo "  make update-posts - update the updated field in posts"

doctor: build
	@echo -- Jekyll doctor
	$(jekyll) doctor

build: update-posts
	@echo -- Building
	$(jekyll) build --lsi --trace

serve:
	$(jekyll) serve --drafts --host 0.0.0.0 --lsi --watch

minify: build
	@echo -- Minifying CSS
	yuicompressor _site/css/bootstrap.css -o _site/css/bootstrap.css

update-posts:
	@echo -- Updating post dates
	python _tools/update_post.py _posts/*.markdown _posts/*.md

draft:
	@cd _tools && ./new-draft.sh

compress: build
	@echo -- Compressing files
	find _site -name '*.html' -print0 \
		| parallel --no-notice -0 perl -pi -e 's/\\.css/\\.css\\.gz/gi'
	find _site -name '*.html' -print0 \
		| parallel --no-notice -0 perl -pi -e 's/\\.svg/\\.svg\\.gz/gi'
	find _site -name '*.html' -print0 \
		| parallel --no-notice -0 perl -pi -e 's/\\.js/\\.js\\.gz/gi'
	find _site -name '*.html' -print0 \
		| parallel --no-notice -0 perl -pi -e 's/analytics.js.gz/analytics.js/gi'
	perl -pi -e 's/dosbox.html.mem/dosbox.html.mem.gz/gi' _site/a-system/dosbox.js
	perl -pi -e 's/dosbox.html.mem/dosbox.html.mem.gz/gi' _site/a-system/index.html
	perl -pi -e 's/intro.data/intro.data.gz/gi' _site/a-system/index.html
	perl -pi -e 's/<code>dosbox.js.gz/<code>dosbox.js/gi' _site/post/em-dosbox/index.html
	find _site \( -name '*.html' -or \
		            -name '*.css' -or \
								-name '*.js' -or \
								-name '*.svg' -or \
								-name 'dosbox.html.mem' -or \
								-name 'intro.data' \) -print0 \
		| parallel --no-notice -0 gzip -9

dist: update-posts doctor minify compress
	@echo -- Publishing
	rsync -avz --delete _site/. -e ssh cslarsen:/home/public

clean:
	rm -rf _site/
