Monaba
======

Anonymouse imageboard engine written in Haskell and powered by Yesod. [Demo board](http://haibane.ru).

Dependencies
------
* GHC >= 7.6
* cabal-install
* PHP 5
* Imagemagick library
* Postgresql >= 9.1

Installation
-----

    git clone https://github.com/ahushh/Monaba
    cd Monaba

Setup PostgreSQL `user` and `password` in config/postgresql.yml

Application root, port, site name and other settings you can change in `config/settings.yml`

**Download GeoIPCity:**

    wget http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz
    gzip -d GeoLiteCity.dat.gz
    cp GeoLiteCity.dat /usr/share/GeoIP/GeoIPCity.dat

Or it can be installed from repositories. You can change the path in `config/settings.yml`

**Download GeSHi:**

    wget http://sourceforge.net/projects/geshi/files/geshi/GeSHi%201.0.8.11/GeSHi-1.0.8.11.tar.gz
    tar -zxvf GeSHi-1.0.8.11.tar.gz
    mv geshi /your/path/to/geshi

Set your path to GeSHi in `highlight.php`

**Install all required packages (apt based distros):**

    apt-get install ghc php5 imagemagick libmagickwand-dev libmagickcore-dev postgresql
    apt-get install cabal-install zlibc libpcre++-dev libpcre3 libpcre3-dev libgeoip-dev libcrypto++-dev libssl-dev postgresql-server-dev-9.1

**Manual building:**

    cabal update
    cabal sandbox init
    cabal install --only-dependencies
    cabal clean && cabal configure && cabal build

You may also want to change meta tags such as `description` and `keywords` in `templates/default-layout-wrapper.hamlet`. Do it before building.

**Run:**

Create a database:

    psql -U postgres -c 'create database monabas_production';

Run the application to initialize database schema:

    ./dist/build/Monaba/Monaba production

Open another terminal and fill database with default values:

     psql -U postgres monabas_production < init-db.sql

You are done. Open [http://localhost:3000](http://localhost:3000) and navigate to manage page and use "admin" both for username and for password to log in.
