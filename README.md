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

Open your CLI and type:

    git clone https://github.com/ahushh/Monaba && cd Monaba

### Set up some local dependencies by running build script:

    ./build.sh

The previous command has just created `settings.yml` file that contains all run configurations. All you want to edit is under `CUSTOMIZE` section.

### Pull Docker images

Once you're ready to go further let's get all docker images by running this command:

    docker-compose pull

Or build them by yourself:

    docker-compose build

It takes rather long time so be patient.

### Run

Start the application:

    docker-compose up -d

Visit `/admin/setup` page and use `admin` both for login and password to log in the admin panel. Don't forget to change your password on `/admin/account` page afterwards.

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

Recommended OS: Linux Mint 18.2 Sonya

Docker version: 18.09

Install stack:

    curl -sSL https://get.haskellstack.org/ | sh

Install local deps:

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

Go to the project and install some local deps:

    cd Monaba

    ./build.sh

Run nginx, postgres, sphinx:

    docker-compose -f docker-compose.dev.yml up

Configure Monaba dev server:

    cd monaba

    stack setup && stack build && stack install yesod-bin

    chmod 777 upload

    source ../monaba_dev_env

Run:

    stack exec yesod devel

Update /etc/hosts with:

    127.0.0.1       monaba.in
    
