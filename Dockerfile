FROM jekyll/jekyll
RUN \
      apk update && \
      apk upgrade && \
      apk add \
        curl \
        libpng \
        libpng-utils \
        optipng \
        parallel \
        py-yuicompressor \
      && (echo 'will cite' | parallel --citation)

# Install chibi-scheme
RUN mkdir /tmp/chibi && \
      cd /tmp/chibi && \
      curl --progress -LO https://github.com/ashinn/chibi-scheme/archive/0.7.3.tar.gz && \
      tar xzf 0.7.3.tar.gz && \
      cd chibi-scheme-0.7.3 && \
      make PREFIX=/usr libchibi-scheme.a && \
      make PREFIX=/usr && \
      make PREFIX=/usr install || exit 0

# Add repo and install Ruby packages
ADD source /source
RUN cd /source && chmod go+rw Gemfile && \
      touch Gemfile.lock && chmod go+rw Gemfile.lock && \
      bundle install
#RUN gem update
#RUN bundle update
RUN chmod -R go+xrw /source
