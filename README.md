<img src="https://assets.gitlab-static.net/uploads/-/system/project/avatar/6507113/monaba-logo.png?width=64" alt="Monaba" width="64"/>

Monaba
======

Imageboard engine written in Haskell and powered by Yesod. [Demo board](http://haibane.ru).

It's a classic web application with the flavour of AJAX and EventSource used to extend UI of good ol' design insipred by Wakaba.

GitLab CI/CD: https://gitlab.com/ahushh/Monaba

Features
------
* Main capabilities:
    - Boards groupping by categories
    - Hiding boards from the list
    - Easy Tor configuration
    - Making boards to be accessed through Tor only
    - Feed page and RSS
    - Threads catalog
    - Threads bookmarks
    - Post deletion and editing
    - Post editing history
    - Multiple file attachment
    - File deletion
    - Video, audio and flash support
    - File rating system: SFW, R15, R18, R18G
    - Text formatting based on BBCode
    - Code highlighting and dice support
    - Prooflables as replacement of tripcodes
    - Private messages
    - Online users counter
    - New posts counter displayed for each board
    - Embedded and customizable CAPTCHA
    - Full-text search
    - Internationalization (English, Русский, Português, Brasil)
    - Country flag support
    - Works fine with JavaScript disabled
    - Custom banners at the top of the page
    - Reports system
   
* UI enhancements:
    - Posting through AJAX and quick reply
    - Threads hiding
    - Answer map and post previews
    - Expanding of threads and images
    - YouTube, Vimeo, Coub embedding
    - 20+ switchable stylesheets
    - Buttons for searching selected image using several search engines like Google, TinEye, etc.
    - Desktop notifications of new posts
    - Each UI feature can be configured or disabled

* Administration features:
    - [Hellbanning](http://en.wikipedia.org/wiki/Hellbanning) by session
    - Banning by IP
    - Listing and recovering deleted posts
    - OP can moderate his own thread
    - Flexible account system with customizable groups and permissions
    - Ability to stick and lock threads and to put on auto-sage
    - Moving threads between boards
    - Moving posts between threads
    - Modlog which allows to view previous actions done by staff
    - Post search by ID and UID
    - Wordfilter with regex support which can trigger different actions (ban, replace text, hide post, deny posting and so on)
    - Each board can be configured independently and has a lot of options
    - Home and about pages and footer can be filled in with custom HTML

Cons
-----

* Bad UI/UX design of administration tools

* Can be slow in some cases

* Have not been tested much, no unit/integration tests

* Difficult to contribute: slow building, legacy dependencies, ugly code

* Not scalable due to sessions which are being stored on disk

* Not under active development

Known issues
------

* Docker Swarm: Monaba gets incorrect anon's IP. Onion detection doesn't work too.

* Memory leak while building: https://github.com/ahushh/Monaba/issues/14

* Incorrect pagination of search results

Requirements
------
* Unix-like distro supported by Docker

* Server with 2GB RAM and 2 CPU cores would be fine

* If you are going to build Monaba from source, you'd better have 16GB RAM and i7 CPU. MacBook Pro 13 2017 without TouchBar has been dying on build.

API docs
------
https://documenter.getpostman.com/view/5005722/SzRxVpvC?version=latest

Installation guide
======

Docker & docker-compose Installation
=====

That's official Docker install script for Debian:

    cd /usr/local/src && wget -qO- https://get.docker.com/ | sh

Make Docker to work without sudo:

`sudo groupadd docker && sudo usermod -aG docker $USER`

Then log out and log back in your system.

Run Docker service:

    sudo systemctl start docker

And download docker-compose - yeah, just download it:

    sudo curl -L https://github.com/docker/compose/releases/download/1.22.0/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose
    sudo chmod +x /usr/local/bin/docker-compose

Monaba Installation
======

Option #1. Plain Docker
------

**Open your CLI and type:**

    git clone https://github.com/ahushh/Monaba && cd Monaba

**Prepare local dependencies by running build script:**

    ./build.sh

The previous command has just created `settings.yml` file that contains all run configurations. All you want to edit is under `CUSTOMIZE` section.

**Pull Docker images**

Log in to github registry to get access to pull Monaba images:

    docker login docker.pkg.github.com -u ahushh -p 18ba5e57502213ad4218a61e73d107096e249a85

Once you're ready to go further let's get all docker images by running this command:

    docker-compose pull

Or build them by yourself:

    docker-compose build

It takes rather long time so be patient.

### Run

Start the application:

    docker-compose up -d

In production mode Monaba binds 80 port.

### Almost there

Now open you browser and visit `/admin/setup` page and you will be redirected to the login form. Use `admin` both for login and password to log in the admin panel. **Don't forget to change your password** on `/admin/account` page afterwards.

*By opnening `/admin/setup` Monaba creates `admin` user with default password `admin`. This method works only once.*

Open `admin/boards/list/NewBoard/-` and create your first board.

### Note

The maximum files size is hardcoded, but it can be changed in `Foundation.hs` before building. Default value is 25 MB. After you made your changes, docker image must be rebuilt:

    docker-compose build app

Option #2. Docker Swarm
------

This is an experimental options and is not recommended to use. Furthermore, it has no reasonable advantages as Monaba is not a stateless server.

Assuming you have a server on `haibane.tk` with ssh access for user `ahushh`.

Open terminal on your local machine and follow the instructions.

### Create SSH key and add it to our server:

    ssh-keygen -f ~/.ssh/monaba

    ssh-copy-id -i ~/.ssh/monaba ahushh@haibane.tk

### Create docker instance on the server:

    docker-machine create --driver generic --generic-ip-address haibane.tk --generic-ssh-user ahushh --generic-ssh-key ~/.ssh/haibane1 --engine-storage-driver=overlay2 monaba

### Connect to the server using SSH:

    docker-machine ssh monaba

### Install docker & docker-compose, clone repository and change dir, run build.sh script:

    See the previous sections. 

### Initialize Docker Cluster:

    docker swarm init

### Deploy

This command pulls the latest monaba images from registry and runs everything:

    docker stack deploy --compose-file docker-compose.yml monaba

Cheatsheet
======

### Setting up onion service

1. Install Eschalot

   `sudo apt-get install openssl`
    
   `git clone https://github.com/ReclaimYourPrivacy/eschalot.git`
    
   `cd eschalot && make`
    
2. Generate a domain name

   `./eschalot -vct4 -p desiredDomainPrefix`
   
Wait until you get a domain name you like. Remove `-----BEGIN RSA PRIVATE KEY-----` and `-----END RSA PRIVATE KEY-----` from the key and encode it with `base64` using the tool with the same name.

3. Open `env_prod` file and uncomment the last three lines, fill in `TORGATE_HOSTNAME` and `TORGATE_PRIVATE_KEY` with the domain name and encoded key.

4. Restart torgate service: `docker-compose restart torgate`

### Update and restart

    cd ~/haibane.ru/Monaba

    docker-compose pull

    docker-compose down
    
    docker-compose up -d
    
### Check the status of all services
    
    docker-compose ps
    
### Check the logs of the selected service

    docker-compose logs webserver | less

### Contribution

#### Guide to install development environment

Tested on: Linux Mint 18.2 Sonya / macOS Catalina

Docker version: 18.09 / 19.03

dontremembershouldbethesame / GHC 8.8.2 and stack 2.1.3

It is possible to run Monaba in dev mode through Docker using dev.Dockefile, but it has been a while since I tried that last time

Install stack (Linux):

    curl -sSL https://get.haskellstack.org/ | sh

It's the best way to get the latest version. You can use package manager of course.

For macOS:

    brew install haskell-stack

Install local deps (Linux):

    sudo apt-get update && sudo apt-get -y install \
      php7.0-fpm \
      libav-tools \
      exiftool \
      libpq-dev \
      libmagickwand-dev \
      libmagickcore-dev \
      libgeoip-dev \
      libicu-dev \
      libcrypto++-dev

For non-apt distro you have to look for equivalent version of these packages.

For macOS:

    # binary tools used by Monaba
    brew install php libav imagemagick
    sudo ln -s /usr/local/bin/convert /usr/bin/convert
    # You may need to run these commands in order to edit /usr/bin directory
    # csrutil disable
    # reboot 
    # Or just update convertPath to point /usr/local/bin/convert in src/Utils/File.hs during development

    # download dmg here https://exiftool.org/

    # required for postgresql-libpq
    brew install postgres libpq

    # set geoipcitypath to the file path in settings.yml
    wget https://github.com/ahushh/monaba/releases/download/v2.5.0/GeoIPCity.dat

    # required for hs-GeoIP
    brew install libpq libgeoip

    # required for nano-md5 
    brew install openssl 

    # required text-icu
    brew install icu4c

Go to the project and prepare some local deps:

    cd Monaba

    ./build.sh

Let's install all those Haskell packages...

    cd monaba

    stack setup

Build Monaba:

    stack build

If you gen an error try this:

    stack build --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include --extra-lib-dirs=/usr/local/opt/openssl@1.1/lib --extra-include-dirs=/usr/local/opt/openssl@1.1/include

Then we need executable version of yesod which support hot reloading:

    stack install yesod-bin

Give all access to file upload directory:

    chmod 777 upload

Build captcha executable file (if you are going to use it):

    cd captcha && stack setup && stack install && cp ~/.local/bin/PlainCaptcha .. && cd ..

Unarchive `GeoIPCity.data` file:

    gunzip ./GeoIPCity.dat.gz

Run nginx, postgres, sphinx:

    docker-compose -f docker-compose.dev.yml up

Load env variables:

    source ../monaba_dev_env

Open `monaba/config/settings.yml` and check all paths are set correctly.

Run:

    stack exec yesod devel

And do not forget to update /etc/hosts for your convenience with:

    127.0.0.1       monaba.in
    
