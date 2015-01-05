Monaba
======

Wakaba-like imageboard written in Haskell and powered by Yesod. [Demo board](http://haibane.ru).

Dependencies
------
* GHC >= 7.6
* Postgresql >= 9.1
* cabal-install >= 1.18
* PHP5 to use GeSHi for code highlighting
* Imagemagick library
* ffmpeg and ffprobe for webm video

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

**Sample list of required packages (apt based distros):**

    apt-get install ghc php5 imagemagick libmagickwand-dev libmagickcore-dev postgresql
    apt-get install cabal-install zlibc libgeoip-dev libcrypto++-dev libssl-dev postgresql-server-dev-9.1

**Building executable file:**

    cabal update
    cabal sandbox init
    cabal install yesod-bin && cabal install --only-dependencies # this takes a while, be patient
    cabal clean && cabal configure && cabal build # and this too

You may also want to change meta tags such as `description` and `keywords` in `templates/default-layout-wrapper.hamlet`. Do it before building.

*If you get an error during installation of dependencies*

    Data/Digest/OpenSSL/MD5.hs:49:12: Not in scope: ‘unsafePerformIO’
    cabal: Error: some packages failed to install:
    nano-md5-0.1.2 failed during the building phase. The exception was:
    ExitFailure 1

*this should help*

    cabal fetch nano-md5
    tar -zxvf ~/.cabal/packages/hackage.haskell.org/nano-md5/0.1.2/nano-md5-0.1.2.tar.gz
    patch nano-md5-0.1.2/Data/Digest/OpenSSL/MD5.hs < extra/MD5.hs.patch
    cabal sandbox add-source nano-md5-0.1.2
    cabal install --only-dependencies

Running
------

Create a database:

    psql -U postgres -c 'create database monabas_production';

Run the application to initialize database schema:

    ./dist/build/Monaba/Monaba production

Open another terminal and fill database with default values:

     psql -U postgres monabas_production < init-db.sql

You are done. Open [http://localhost:3000](http://localhost:3000) and navigate to manage page and use "admin" both for username and for password to log in.

Deployment
------

See `extra/nginx.conf` example if you want to use Nginx as reverse proxy. That's generally a good idea.

For systemd users: `extra/monaba.service`
