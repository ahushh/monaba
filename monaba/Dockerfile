# build
FROM fpco/stack-build:lts-15 AS build-env

RUN mkdir -p /opt/monaba-build
WORKDIR /opt/monaba-build

RUN apt-get update && apt-get install -y locales && rm -rf /var/lib/apt/lists/* \
 && locale-gen "en_US.UTF-8"
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

RUN apt-get update && apt-get -y install \
  libcrypto++-dev \
  libssl-dev \
  libgeoip-dev

COPY stack.yaml ./stack.yaml
COPY Monaba.cabal ./Monaba.cabal
COPY nano-md5-0.1.2 ./nano-md5-0.1.2
RUN stack setup --silent
RUN stack build --only-snapshot --silent
RUN stack install --only-dependencies --silent
COPY . ./

RUN stack install

COPY captcha captcha
WORKDIR /opt/monaba-build/captcha
RUN stack setup --silent
RUN stack install --silent

# run
#FROM ubuntu:16.04
RUN mkdir -p /opt/monaba
WORKDIR /opt/monaba

RUN apt-get update && apt-get -y install \
  php7.2-fpm \
  ffmpeg \
  imagemagick \
  exiftool \
  libpq-dev \
  libmagickwand-dev \
  libmagickcore-dev \
  libgeoip-dev \
  libicu-dev \
  libcrypto++-dev

COPY ./geshi ./geshi
ADD ./GeoIPCity.dat.gz /usr/share/GeoIP/GeoIPCity.dat

COPY --from=build-env /opt/monaba-build/highlight.php ./highlight.php
COPY --from=build-env /opt/monaba-build/config ./config
COPY --from=build-env /opt/monaba-build/static ./static
COPY --from=build-env /root/.local/bin ./
RUN mkdir -p ./upload
