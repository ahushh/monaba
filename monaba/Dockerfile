# build
FROM fpco/stack-build AS build-env

RUN mkdir -p /opt/monaba-build
WORKDIR /opt/monaba-build

RUN apt-get update && apt-get -y install \
  libcrypto++-dev \
  libssl-dev \
  libgeoip-dev

COPY stack.yaml ./stack.yaml
RUN stack setup --silent
COPY Monaba.cabal ./Monaba.cabal
COPY nano-md5-0.1.2 ./nano-md5-0.1.2
RUN stack build --only-snapshot --silent

COPY . ./

RUN stack install

# run
FROM ubuntu:16.04
RUN mkdir -p /opt/monaba
WORKDIR /opt/monaba

COPY --from=build-env /opt/monaba-build/extra/GeSHi-1.0.8.11.tar.gz geshi.tar.gz
RUN tar -zxvf geshi.tar.gz
COPY geshi .
ADD http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz /usr/share/GeoIP/GeoIPCity.dat

RUN apt-get update && apt-get -y install \
  php7.0-fpm \
  libav-tools \
  exiftool \
  libpq-dev \
  libmagickwand-dev \
  libmagickcore-dev \
  libgeoip-dev \
  libicu-dev \
  libcrypto++-dev

COPY --from=build-env /opt/monaba-build/highlight.php ./highlight.php
COPY --from=build-env /opt/monaba-build/config ./config
COPY --from=build-env /opt/monaba-build/static ./static
COPY --from=build-env /root/.local/bin ./
RUN mkdir -p ./upload