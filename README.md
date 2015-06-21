Monaba
======

Wakaba-like imageboard written in Haskell and powered by Yesod. [Demo board](http://haibane.ru).

Features
------
* Multiple file attachment
* Webm and audio support
* AJAX posting and quick reply
* Feed page
* Online user counter
* New posts counter
* Answer map and previews
* Thread and image expanding
* Thread hiding
* Post deletion and editing by user
* Prooflabes as replacement of tripcodes
* Kusaba-like formatting with code highlighting and LaTeX support
* Custom CAPTCHA
* Internationalization
* Country flag support
* Switchable stylesheets
* YouTube, vimeo, coub embedding
* Works fine with JavaScript disabled
* Administration
    - [Hellbanning](http://en.wikipedia.org/wiki/Hellbanning)
    - Banning by IP
    - Thread moderation by OP
    - Flexible account system with customizable groups and permissions
    - Ability to stick and lock threads and to put on auto-sage
    - Moving threads between boards
    - Changing post's parent
    - Modlog which allows to view previous actions
    - Post search by ID and UID

Limitations
-----------
* Quite slow :(

Dependencies
------
* GHC >= 7.6
* Postgresql >= 9.1
* cabal-install >= 1.18
* PHP5 to use GeSHi for code highlighting
* Imagemagick library
* ffmpeg
* exiftool

Installation
-----

    git clone https://github.com/ahushh/Monaba
    cd Monaba

Main config file `config/settings.yml`

The maximum files size is hard coded and can be changed in `Foundation.hs`

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
    cabal install yesod-bin --force-reinstall && cabal install --only-dependencies --force-reinstalls # this takes a while, be patient
    cabal clean && cabal configure && cabal build # and this too
    cp dist/build/Captcha/Captcha Captcha

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

    psql -U postgres -c 'create database monabas';

Run the application to initialize database schema:

    ./dist/build/Monaba/Monaba config/settings.yml

Open another terminal and fill database with default values:

     psql -U postgres monabas < init-db.sql

Default login/password: admin

Configuring Nginx for serving uploaded files
------

See `extra/nginx.conf`

Deployment
------

init.d script for gentoo: `extra/monaba`

For systemd users: `extra/monaba.service`
