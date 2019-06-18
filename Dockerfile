
FROM       ubuntu:bionic

MAINTAINER Andres Mariscal "https://github.com/serialdev"

RUN apt-get update

# common packages
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    ca-certificates curl file \
    build-essential \
    autoconf automake autotools-dev libtool xutils-dev && \
    rm -rf /var/lib/apt/lists/*  && \
    apt-get update && apt-get -y upgrade && \
    apt-get install software-properties-common -y && \
    apt-get install git -y && \
    apt-get install silversearcher-ag -y && \
    apt-get install curl -y && \
    add-apt-repository ppa:ubuntu-elisp/ppa -y && \
    apt-get update && \
    apt-get install emacs-snapshot -y && \
    apt-get update --fix-missing && apt-get install -y wget bzip2 ca-certificates \
    libglib2.0-0 libxext6 libsm6 libxrender1 \
    git mercurial subversion && \
    apt-get update && apt-get install -y \
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
    apt-get install ninja-build -y && \
    apt-get install libclang-dev -y && \
    apt-get install pkg-config -y && \
    apt-get install clang -y && \
    apt-get install cmake -y

RUN apt-get install -y openssh-server && \
    mkdir /var/run/sshd && \
    echo 'root:root' |chpasswd && \
    sed -ri 's/^#?PermitRootLogin\s+.*/PermitRootLogin yes/' /etc/ssh/sshd_config & \
    sed -ri 's/UsePAM yes/#UsePAM yes/g' /etc/ssh/sshd_config & \
    mkdir /root/.ssh

RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

ENV PATH /root/.cask/bin:$PATH

RUN apt-get update && apt-get install opam -y && \
    opam init && \
    opam install utop -y && \
    opam install tuareg -y && \
    opam install merlin -y && \
    opam user-setup install

RUN apt-get install nodejs -y && \
    apt-get install npm -y && \
    npm install -g bs-platform && \
    npm install npm --global && \
    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg |  apt-key add - && \
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    apt-get update && apt-get install yarn

RUN apt-get install -y curl grep sed dpkg && \
    TINI_VERSION=`curl https://github.com/krallin/tini/releases/latest | grep -o "/v.*\"" | sed 's:^..\(.*\).$:\1:'` && \
    curl -L "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini_${TINI_VERSION}.deb" > tini.deb && \
    dpkg -i tini.deb && \
    rm tini.deb && \
    apt-get clean

RUN echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh && \
    wget --quiet https://repo.continuum.io/archive/Anaconda3-5.1.0-Linux-x86_64.sh -O ~/anaconda.sh && \
    /bin/bash ~/anaconda.sh -b -p /opt/conda && \
    rm ~/anaconda.sh

ENV PATH /opt/conda/bin:$PATH

RUN conda update -n base conda && \
    conda install pytorch -c pytorch && \
    conda install virtualenv && \
    conda install torchvision && \
    conda install jupyter &&  \
    conda install pip && \
    pip install meson && \
    pip install python-language-server[all] && \
    pip install pyls-mypy && \
    pip install pyls-isort && \
    pip install black

RUN git clone --recursive https://github.com/Andersbakken/rtags.git && \
    cd rtags && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 . && make 

# install rustup toolchain
RUN curl https://sh.rustup.rs -sSf | \
    sh -s -- --default-toolchain nightly -y

ENV RUSTUP_HOME=/rust

ENV CARGO_HOME=/cargo

ENV PATH="/cargo/bin:/rust/bin:${PATH}"

RUN apt-get install pkg-config libssl-dev -y && echo "(curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly --no-modify-path) && rustup default nightly" > /install-rust.sh && chmod 755 /install-rust.sh && \
    bash install-rust.sh && \
    rustup component add rust-src && \
    cargo install racer && rustup target add wasm32-unknown-unknown && cargo install cargo-web

RUN apt-get install sbcl && \
    apt-get install cl-quicklisp && \
    sbcl --load /usr/share/cl-quicklisp/quicklisp.lisp \
       --eval '(quicklisp-quickstart:install)'       \
       --eval '(ql:add-to-init-file)'                \
       --eval '(ql:quickload "quicklisp-slime-helper")'                \
       --eval '(quit)'

RUN wget "https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-linux.tar.gz" && \
    tar xzf binaries-for-linux.tar.gz && \
    mv elm /usr/local/bin/

RUN wget "https://download.visualstudio.microsoft.com/download/pr/7e4b403c-34b3-4b3e-807c-d064a7857fe8/95c738f08e163f27867e38c602a433a1/dotnet-sdk-3.0.100-preview5-011568-linux-x64.tar.gz" &&\
	mkdir -p /dotnet && tar zxf dotnet-sdk-3.0.100-preview5-011568-linux-x64.tar.gz -C /dotnet


ENV PATH="${PATH}:/rtags/bin"
ENV DOTNET_ROOT="/dotnet"
ENV PATH="${PATH}:$DOTNET_ROOT"



# Install docker
RUN apt update && \
    apt install apt-transport-https ca-certificates curl software-properties-common -y && \
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - && \
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable" && \
    apt update && \
    apt-cache policy docker-ce && \
    apt install docker-ce -y


#---------------------------------------------------------------------------------------------------#
#                                          Important tools                                          #
#---------------------------------------------------------------------------------------------------#

RUN apt-get install firefox -y && \
    apt-get install silversearcher-ag -y && \
    git clone https://github.com/domtronn/all-the-icons.el.git && \
    cp /all-the-icons.el/fonts/all-the-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/file-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/fontawesome.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/material-design-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/octicons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/weathericons.ttf /usr/local/share/fonts

RUN apt-get install chicken-bin -y && \
    cd `csi -p '(chicken-home)'` && \
    curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | tar zx



RUN	git clone https://github.com/Microsoft/python-language-server.git && \
	cd python-language-server/src/LanguageServer/Impl && \
	dotnet publish -c Release -r linux-x64 && \
	ln -sf $(git rev-parse --show-toplevel)/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer /usr/bin/


# ------------------------------------------------------------------------- #
#                                 Install Go                                #
# ------------------------------------------------------------------------- #
RUN curl -O https://dl.google.com/go/go1.10.3.linux-amd64.tar.gz && tar xvf go1.10.3.linux-amd64.tar.gz && \
	chown -R root:root ./go && mv go /usr/local

ENV GOPATH="${HOME}/work"
ENV PATH="${PATH}:/usr/local/go/bin:${GOPATH}/bin"

RUN go get golang.org/x/tools/cmd/...

# RUN apt-get install clang-tools-7 && update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-7 100

ADD ./ /tiqsi-emacs

RUN cp /tiqsi-emacs/PragmataPro.ttf /usr/local/share/fonts

ENV DISPLAY=192.168.100.13:0

ENV LC_ALL=C.UTF-8

ENV LANG=C.UTF-8

RUN echo "XLIB_SKIP_ARGB_VISUALS=1 emacs -q -l /tiqsi-emacs/init.el &" >> /tiqsi-emacs/launch-tiqsi.sh && \
    chmod 777 /tiqsi-emacs/launch-tiqsi.sh


WORKDIR /tiqsi-emacs/
