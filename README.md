Monaba
======

Imageboard engine written in Haskell and powered by Yesod.

Features
------
* Multiple file attachment
* File censorship ratings
* [Hellbanning](http://en.wikipedia.org/wiki/Hellbanning)
* AJAX
* Prooflabes
* Flexible account system
* Internationalization
* Post deletion by OP
* GeoIP support

Dependencies
------
* GHC >= 7.6
* PHP 5
* GD image library
* MySQL 5

Installation
------
Edit config/settings.yml and config/mysql.yml

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

    apt-get install ghc php5 libgd-dev mysql-server
    apt-get install cabal-install zlibc libpcre++-dev libgeoip-dev libcrypto++-dev libssl-dev libmysqlclient-dev

**Using already compiled binary:**

Download Monaba-[your-arch]-[your-platform].7z [here](https://github.com/ahushh/Monaba/releases) and unpack it to dist/build/Monaba/

**Manual building:**

    cabal update
    cabal install hsenv
    ~/.cabal/bin/hsenv
    source .hsenv/bin/activate
    cabal install happy
    cabal install --only-dependencies
    cabal install yesod-bin
    cabal clean && yesod configure && yesod build

Compilation will take about half an hour and probably run out of memory. You might need to enable swap. 

**Run:**

Create a database:

    mysql -u mysqluser -pmysqlpassword -e 'create database Monaba_production;'

Run the application to initialize database schema:

    ./dist/build/Monaba/Monaba production

Open another terminal and fill database with default values:

    mysql -u mysqluser -pmysqlpassword -e 'use Monaba_production; source init-db.sql;'

Use "admin" both for username and for password to log in.
