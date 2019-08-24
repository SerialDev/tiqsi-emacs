#!/bin/sh


sudo apt-get update && \
sudo apt-get install --no-install-recommends -y \
ca-certificates curl file \
build-essential \
autoconf automake autotools-dev libtool xutils-dev && \
sudo rm -rf /var/lib/apt/lists/*  && \
sudo apt-get update && apt-get -y upgrade && \
sudo apt-get install software-properties-common -y && \
sudo apt-get install git -y && \
sudo apt-get install silversearcher-ag -y && \
sudo apt-get install curl -y && \
sudo add-apt-repository ppa:ubuntu-elisp/ppa -y && \
sudo apt-get update && \
sudo apt-get install emacs-snapshot -y && \
sudo apt-get update --fix-missing && apt-get install -y wget bzip2 ca-certificates \
libglib2.0-0 libxext6 libsm6 libxrender1 \
git mercurial subversion && \
sudo apt-get update && sudo apt-get install -y \
automake \
build-essential \
curl \
libgif-dev \
libgnutls28-dev \
libgtk-3-dev \
libjpeg-dev \
libncurses5-dev \
libtiff-dev \
libxml2-dev \
libxpm-dev \
texinfo && \
sudo apt-get install ninja-build -y && \
sudo apt-get install libclang-dev -y && \
sudo apt-get install pkg-config -y && \
sudo apt-get install clang -y && \
sudo apt-get install cmake -y

sudo apt-get install -y openssh-server && \
    sudo mkdir /var/run/sshd && \
    sudo echo 'root:root' |chpasswd && \
    sudo sed -ri 's/^#?PermitRootLogin\s+.*/PermitRootLogin yes/' /etc/ssh/sshd_config & \
    sudo sed -ri 's/UsePAM yes/#UsePAM yes/g' /etc/ssh/sshd_config & \
    sudo mkdir /root/.ssh

sudo apt-get clean && \
sudo rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

sudo curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

sudo apt-get update && apt-get install opam -y && \
sudo opam init && \
sudo opam install utop -y && \
sudo opam install tuareg -y && \
sudo opam install merlin -y && \
sudo opam user-setup install

sudo apt-get install nodejs -y && \
sudo apt-get install npm -y && \
sudo npm install -g bs-platform && \
sudo npm install npm --global && \
sudo curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg |  apt-key add - && \
sudo echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
sudo apt-get update && sudo apt-get install yarn

sudo apt-get install -y curl grep sed dpkg && \
sudo TINI_VERSION=`curl https://github.com/krallin/tini/releases/latest | grep -o "/v.*\"" | sed 's:^..\(.*\).$:\1:'` && \
sudo curl -L "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini_${TINI_VERSION}.deb" > sudo tini.deb && \
sudo dpkg -i tini.deb && \
sudo rm tini.deb && \
sudo apt-get clean

sudo echo 'export PATH=/opt/conda/bin:$PATH' > sudo /etc/profile.d/conda.sh && \
sudo wget --quiet https://repo.continuum.io/archive/Anaconda3-5.1.0-Linux-x86_64.sh -O ~/anaconda.sh && \
sudo /bin/bash ~/anaconda.sh -b -p /opt/conda && \
sudo rm ~/anaconda.sh

sudo conda update -n base conda && \
sudo conda install pytorch -c pytorch && \
sudo conda install virtualenv && \
sudo conda install torchvision && \
sudo conda install jupyter &&  \
sudo conda install pip && \
sudo pip install meson && \
sudo pip install python-language-server[all] && \
sudo pip install pyls-mypy && \
sudo pip install pyls-isort && \
sudo pip install black

sudo git clone --recursive https://github.com/Andersbakken/rtags.git && \
sudo cd rtags && \
sudo cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 . && \
sudo sudo make 

sudo wget "https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-linux.tar.gz" && \
    sudo tar xzf binaries-for-linux.tar.gz && \
    sudo mv elm /usr/local/bin/

sudo wget "https://download.visualstudio.microsoft.com/download/pr/7e4b403c-34b3-4b3e-807c-d064a7857fe8/95c738f08e163f27867e38c602a433a1/dotnet-sdk-3.0.100-preview5-011568-linux-x64.tar.gz" &&\
    sudo mkdir -p /dotnet && \
    sudo tar zxf dotnet-sdk-3.0.100-preview5-011568-linux-x64.tar.gz -C /dotnet

sudo apt-get install firefox -y && \
    sudo apt-get install silversearcher-ag -y && \
    sudo git clone https://github.com/domtronn/all-the-icons.el.git && \
    sudo cp /all-the-icons.el/fonts/all-the-icons.ttf /usr/local/share/fonts && \
    sudo cp /all-the-icons.el/fonts/file-icons.ttf /usr/local/share/fonts && \
    sudo cp /all-the-icons.el/fonts/fontawesome.ttf /usr/local/share/fonts && \
    sudo cp /all-the-icons.el/fonts/material-design-icons.ttf /usr/local/share/fonts && \
    sudo cp /all-the-icons.el/fonts/octicons.ttf /usr/local/share/fonts && \
    sudo cp /all-the-icons.el/fonts/weathericons.ttf /usr/local/share/fonts

sudo apt-get install chicken-bin -y && \
    sudo cd `csi -p '(chicken-home)'` && \
    sudo curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | tar zx


sudo git clone https://github.com/Microsoft/python-language-server.git && \
	sudo cd python-language-server/src/LanguageServer/Impl && \
	sudo dotnet publish -c Release -r linux-x64 && \
	sudo ln -sf $(git rev-parse --show-toplevel)/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer /usr/bin/

go get golang.org/x/tools/cmd/...

# ------------------------------------------------------------------------- #
#                                 Install Go                                #
# ------------------------------------------------------------------------- #
sudo curl -O https://dl.google.com/go/go1.10.3.linux-amd64.tar.gz && \
    sudo tar xvf go1.10.3.linux-amd64.tar.gz && \
	sudo chown -R root:root ./go && sudo mv go /usr/local

ENV GOPATH="${HOME}/work"
ENV PATH="${PATH}:/usr/local/go/bin:${GOPATH}/bin"


# install rustup toolchain
sudo curl https://sh.rustup.rs -sSf | \
    sudo sh -s -- --default-toolchain nightly -y

sudo apt-get install pkg-config libssl-dev -y && \
    sudo echo "(curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly --no-modify-path) && rustup default nightly" > sudo /install-rust.sh &&
    sudo chmod 755 /install-rust.sh && \
    sudo bash install-rust.sh && \
    sudo rustup component add rust-src && \
    sudo cargo install racer && \
    sudo rustup target add wasm32-unknown-unknown && \
    sudo cargo install cargo-web

sudo apt-get install sbcl && \
    sudo apt-get install cl-quicklisp && \
    sudo sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp \
       --eval '(quicklisp-quickstart:install)'       \
       --eval '(ql:add-to-init-file)'                \
       --eval '(ql:quickload "quicklisp-slime-helper")'                \
       --eval '(quit)'



export PATH="${PATH}:/rtags/bin"
export DOTNET_ROOT="/dotnet"
export PATH="${PATH}:$DOTNET_ROOT"
# export RUSTUP_HOME=/rust
# export CARGO_HOME=/cargo
export PATH="/cargo/bin:/rust/bin:${PATH}"
export PATH=/opt/conda/bin:$PATH
export PATH=/root/.cask/bin:$PATH
