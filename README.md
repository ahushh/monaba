Monaba
======

Imageboard engine written in Haskell and powered by Yesod.

Dependencies
------
* GHC >= 7.6
* cabal-install
* PHP 5
* GD image library
* SQLite 3

Installation
------
Edit config/settings.yml

**Download GeoIPCity:**

    wget http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz
    gzip -d GeoLiteCity.dat.gz
    cp GeoLiteCity.dat /usr/share/GeoIP/GeoIPCity.dat

Or it can be installed from repositories.

**Download GeSHi:**

    wget http://sourceforge.net/projects/geshi/files/geshi/GeSHi%201.0.8.11/GeSHi-1.0.8.11.tar.gz
    tar -zxvf GeSHi-1.0.8.11.tar.gz
    mv geshi /your/path/to/geshi

Set your path to GeSHi in highlight.php

**Install all required packages:**

    apt-get install ghc
    apt-get install cabal-install
    apt-get install php5
    apt-get install libgd-dev
    apt-get install sqlite3

**Build:**
    cabal install hsenv
    hsenv
    source .hsenv/bin/activate
    cabal install yesod-bin
    cabal install --only-dependencies
    cabal clean && yesod configure && yesod build
