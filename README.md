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
* Prooflables as replacement of tripcodes
* Kusaba-like formatting with code highlighting and LaTeX support
* Custom CAPTCHA
* Internationalization (English, Русский, Português, Brasil)
* Country flag support
* Switchable stylesheets
* YouTube, vimeo, coub embedding
* Works fine with JavaScript disabled
* Thread catalog
* Bookmarks
* Private messages
* Full-text search
* Administration
    - [Hellbanning](http://en.wikipedia.org/wiki/Hellbanning) by session
    - Banning by IP
    - Thread moderation by OP
    - Flexible account system with customizable groups and permissions
    - Ability to stick and lock threads and to put on auto-sage
    - Moving threads between boards
    - Changing post's parent
    - Modlog which allows to view previous actions done by staff
    - Post search by ID and UID
    - Wordfilter with regex support

Requirements
------
* Unix-like distro supported by Docker

Docker & docker-compose Installation
=====

That's official Docker install script for Debian:

    cd /usr/local/src && wget -qO- https://get.docker.com/ | sh

Run Docker service:

    sudo systemctl start docker

And download docker-compose - yeah, just download it:

    sudo curl -L https://github.com/docker/compose/releases/download/1.21.2/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose
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

Visit `/admin/setup` page and use `admin` both for login and password to log in admin panel.

The maximum files size is hardcoded and can be changed in `Foundation.hs` before building. Default value is 25 MB. After you made your changes, docker image must be rebuilt:

    docker-compose build app

Option #2. Docker Swarm
------

Assuming you have a server on haibane.tk with ssh access for user ahushh.

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

