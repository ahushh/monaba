Monaba
======

Wakaba-like imageboard written in Haskell and powered by Yesod. [Demo board](http://haibane.ru).

Features
------
* Multiple file attachment
* Webm and audio support
* AJAX posting and quick reply
* Feed page and RSS
* Online user counter
* New posts counter
* Answer map and previews
* Thread and image expanding
* Thread hiding
* Post deletion and editing by user
* Prooflabes as replacement of tripcodes
* Kusaba-like formatting with code highlighting and LaTeX support
* Custom CAPTCHA
* Internationalization (English, Русский, Português Brasil)
* Country flag support
* Switchable stylesheets
* YouTube, vimeo, coub embedding
* Works fine with JavaScript disabled
* Administration
    - [Hellbanning](http://en.wikipedia.org/wiki/Hellbanning) by session
    - Banning by IP
    - Thread moderation by OP
    - Flexible account system with customizable groups and permissions
    - Ability to stick and lock threads and to put on auto-sage
    - Moving threads between boards
    - Changing post's parent
    - Modlog which allows to view previous actions
    - Post search by ID and UID

Dependencies
------
* Nginx for serving uploaded files
* Postgresql >= 9.1
* PHP5 to use GeSHi for code highlighting
* Imagemagick library
* ffmpeg/libav (thumbnails for webm)
* exiftool

Required for builiding from source:

* GHC >= 7.10
* cabal-install >= 1.20

Installation
======

    git clone https://github.com/ahushh/Monaba
    cd Monaba

Main config file `config/settings.yml`

The maximum files size is hard coded and can be changed in `Foundation.hs` before building. Default value is 25 MB.

Default login/password: admin

### Download GeoIPCity

    wget http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz
    gzip -d GeoLiteCity.dat.gz
    cp GeoLiteCity.dat /usr/share/GeoIP/GeoIPCity.dat

Or it can be installed from repositories. You can change the path in `config/settings.yml`

### Download GeSHi

    wget http://sourceforge.net/projects/geshi/files/geshi/GeSHi%201.0.8.11/GeSHi-1.0.8.11.tar.gz
    tar -zxvf GeSHi-1.0.8.11.tar.gz
    mv geshi /your/path/to/geshi

Set your path to GeSHi in `highlight.php`

## Using binary packages

Download archive of the latest verion of Monaba here: https://github.com/ahushh/Monaba/releases/ and unpack it to current directory. 

If it's not working or outdated, try to build from source.

## Building from source

Sample list of required packages for debian (probably outdated and not full):

    apt-get install ghc cabal-install zlibc libgeoip-dev libcrypto++-dev libssl-dev postgresql-server-dev-9.1 libmagickwand-dev libmagickcore-dev

### Execute the following commands

    cabal update
    cabal sandbox init
    cabal fetch nano-md5
    tar -zxvf ~/.cabal/packages/hackage.haskell.org/nano-md5/0.1.2/nano-md5-0.1.2.tar.gz
    patch nano-md5-0.1.2/Data/Digest/OpenSSL/MD5.hs < extra/MD5.hs.patch
    cabal sandbox add-source nano-md5-0.1.2
    cabal install --only-dependencies --force-reinstalls # this takes a while, be patient
    cabal clean && cabal configure && cabal build # and this too

## Setup database

Create a database:

    psql -U postgres -c 'create database monabas';

Run the application to initialize database schema:

    ./dist/build/Monaba/Monaba config/settings.yml

Wait until it finish (a few seconds) then stop with Ctrl+C

Fill the database with default values:

     psql -U postgres monabas < init-db.sql

## Configuring Nginx for serving uploaded files

See `extra/nginx.conf`

## Init scripts

init.d script for gentoo: `extra/monaba`

For systemd users: `extra/monaba.service`
