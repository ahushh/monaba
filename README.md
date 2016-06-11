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
* Thread catalog
* Bookmarks
* Private messages (buggy for now)
* Full-text search
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

Requirements
------
* Nginx (for serving uploaded files)
* Postgresql >= 9.1
* PHP5 to use GeSHi for code highlighting
* Imagemagick (image thumbnails)
* ffmpeg/libav (webm thumbnails)
* exiftool (for audio and webm files)
* Sphinx (post search)

Required for builiding from source:

* GHC >= 7.10
* cabal-install >= 1.20

Installation
======

Open the prompt and type:

    git clone https://github.com/ahushh/Monaba
    cd Monaba

Main configuration file `config/settings.yml`

The maximum files size is hard coded and can be changed in `Foundation.hs` before building. Default value is 25 MB.

Default login/password: admin

### Geolocation

Download GeoIPCity by running the following commands:

    wget http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz
    gzip -d GeoLiteCity.dat.gz
    cp GeoLiteCity.dat /usr/share/GeoIP/GeoIPCity.dat # or whatever path you want

Or it can be installed from repositories. You can change the  in `config/settings.yml`

### Download GeSHi

    wget http://sourceforge.net/projects/geshi/files/geshi/GeSHi%201.0.8.11/GeSHi-1.0.8.11.tar.gz
    tar -zxvf GeSHi-1.0.8.11.tar.gz
    mv geshi ~/

Set your path to GeSHi in `highlight.php`. Home directory is usually good.

## Using binary packages

Download archive of the latest verion of Monaba here: https://github.com/ahushh/Monaba/releases/ and unpack it to current directory. 

If it's not working or outdated, try to build from source.

## Building from source

Sample list of required packages for debian:

    apt-get install ghc cabal-install zlibc libgeoip-dev libcrypto++-dev libssl-dev postgresql-server-dev-9.1 libmagickwand-dev libmagickcore-dev libicu-dev

### Execute the following commands

    cabal update
    cabal sandbox init

    cabal install happy alex

    cabal fetch nano-md5
    tar -zxvf ~/.cabal/packages/hackage.haskell.org/nano-md5/0.1.2/nano-md5-0.1.2.tar.gz
    patch nano-md5-0.1.2/Data/Digest/OpenSSL/MD5.hs < extra/MD5.hs.patch
    cabal sandbox add-source nano-md5-0.1.2

    cabal install --only-dependencies --force-reinstalls # this takes a while, be patient
    cabal clean && cabal configure && cabal build # and this too

Monaba and Captcha binaries are located at ./dist/build/Monaba/

## Setup database

Create a database:

    psql -U postgres -c 'create database monabas';

Run the application to initialize database schema:

    ./dist/build/Monaba/Monaba config/settings.yml

Wait until it finish (a few seconds) then stop with Ctrl+C

Fill the database with default values:

     psql -U postgres monabas < init-db.sql

## Configuring Sphinx search

See `extra/sphinx.conf`

Create the search index by running `sudo indexer --rotate monaba` and add this command to cron job so the index is regulary updated

Start `searchd` service:

`systemctl start sphinxsearch.service`

## Configuring Nginx for serving uploaded files

See `extra/nginx.conf`

## Init scripts

init.d script for gentoo: `extra/monaba`

For systemd users:

Firstly install `libpam-systemd` package and relogin.

Configure `extra/monaba.service` and put this file into `/etc/systemd/user/` or `~/.config/systemd/user/` directory

Example of usage:

`systemctl --user start monaba`
`systemctl --user stop monaba`
`systemctl --user restart monaba`

