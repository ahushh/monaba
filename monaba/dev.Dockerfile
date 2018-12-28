# build
FROM fpco/stack-build

RUN mkdir -p /opt/monaba
WORKDIR /opt/monaba

RUN apt-get update && apt-get -y install \
  libssl-dev \
  libgeoip-dev \
  php7.0-fpm \
  libav-tools \
  exiftool \
  libpq-dev \
  libmagickwand-dev \
  libmagickcore-dev \
  libicu-dev \
  libcrypto++-dev

RUN ls
COPY stack.yaml ./stack.yaml
RUN stack setup --silent
COPY Monaba.cabal ./Monaba.cabal
COPY nano-md5-0.1.2 ./nano-md5-0.1.2
RUN stack build --only-snapshot --silent

# RUN apt-get update && apt-get -y install nslookup

RUN stack install --only-dependencies
# XSexec -- cabal install --only-dependencies
RUN stack install yesod-bin-1.4.18.7

COPY ./geshi ./geshi
ADD http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz /usr/share/GeoIP/GeoIPCity.dat

RUN mkdir -p ./upload
COPY . ./

CMD stack exec yesod devel