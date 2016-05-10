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

build: includes
	@echo -- Building
	$(jekyll) build --trace

serve: includes
	$(jekyll) serve --drafts --host 0.0.0.0 --watch

_includes/css/all.css: _includes/css/normalize.css _includes/css/skeleton.css _includes/css/customized.css _includes/css/font.css
	cat $^ > $@
	yuicompressor $@ -o $@.tmp
	mv $@.tmp $@

minify:
	@echo -- Minified CSS

# Currently not being used
update-posts:
	@echo -- Updating post dates
	python _tools/update_post.py _posts/*.markdown _posts/*.md

includes: _includes/css/all.css
	@echo -- Building include files
	$(MAKE) -C _includes/scheme/exceptions/ -f _Makefile all
	$(MAKE) -C _includes/scheme/goto/ -f _Makefile all

clean-includes:
	$(MAKE) -C _includes/scheme/exceptions/ -f _Makefile clean
	$(MAKE) -C _includes/scheme/goto/ -f _Makefile clean

new: draft

draft:
	@cd _tools && ./new-draft.sh

compress: build pngfix
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

pngfix: optipng
	@echo -- Optimizing PNG images with pngfix
	for a in _site/gfx/post/*.png; do pngfix --suffix=.tmp --strip=all --optimize $$a; done
	for a in _site/gfx/post/*.png.tmp; do mv $$a $${a%.tmp}; done

optipng: build
	@echo -- Optimizing PNG images with optipng
	find _site/gfx/post -name '*.png' -print0 \
		| parallel --no-notice -0 optipng -o4

dist: doctor minify compress
	@echo -- Publishing
	rsync -avz --delete _site/. -e ssh cslarsen:/home/public

clean: clean-includes
	rm -rf _site/ _includes/css/all.css _includes/css/all.css.tmp
