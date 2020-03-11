# build
FROM fpco/stack-build:lts-15 AS build-env

RUN mkdir -p /opt/monaba
WORKDIR /opt/monaba

# configure locales
RUN apt-get update && apt-get install -y locales && rm -rf /var/lib/apt/lists/* \
 && locale-gen "en_US.UTF-8"
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

# build dependencies
RUN apt-get update && apt-get -y install \
  libcrypto++-dev \
  libssl-dev \
  libgeoip-dev

# build monaba executable
COPY stack.yaml ./stack.yaml
COPY Monaba.cabal ./Monaba.cabal
COPY nano-md5-0.1.2 ./nano-md5-0.1.2
COPY ./geshi ./geshi
ADD ./GeoIPCity.dat.gz /usr/share/GeoIP/GeoIPCity.dat

RUN stack setup --silent
RUN stack build --only-snapshot --silent
RUN stack install --only-dependencies --silent
COPY . ./

# install to /root/local/.bin
RUN stack install --silent

RUN mkdir -p ./upload

# build captcha executable
WORKDIR /opt/monaba/captcha
RUN stack setup --silent
# install to /root/local/.bin
RUN stack install --silent

# run dependencies
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

# copy Monaba and captcha executables
COPY /root/.local/bin ./
