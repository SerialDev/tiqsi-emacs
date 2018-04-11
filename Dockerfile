FROM serialdev/sdev-ide


# RUN conda update conda && \
#     conda install gxx_linux-64 && \
#     conda install -c r r-nloptr && \
#     conda install -c r r-igraph && \
#     conda install -c r r-pki && \
#     conda install -c r r-hmisc && \
#     conda install -c r r-aer 

# RUN conda install -c r r-cairo && \
#     conda install -c r r-udunits2 && \
#     conda install -c r r-rodbc && \
#     conda install -c r r-essentials && \
#     conda install libgfortran  && \
#     conda install gfortran_linux-64 

# RUN git clone --recursive https://github.com/Wilfred/remacs.git && \
#     cd remacs && ./autogen.sh && ./configure --enable-rust-debug && \
#     make


# RUN apt-get update && \		    
#     apt-get install default-jdk -y && \
#     echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
#     apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
#     apt-get update && \
#     apt-get install sbt && \
#     apt-get install scala -y

# RUN echo 'addSbtPlugin("org.ensime" % "sbt-ensime" % "2.5.1")' >> ~/.sbt/1.0/plugins/plugins.sbt


RUN apt-get install ninja-build -y

ADD ./ /tiqsi-emacs


RUN git clone https://github.com/domtronn/all-the-icons.el.git

RUN cp /all-the-icons.el/fonts/all-the-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/file-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/fontawesome.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/material-design-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/octicons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/weathericons.ttf /usr/local/share/fonts && \
    cp /tiqsi-emacs/PragmataPro.ttf /usr/local/share/fonts


RUN echo "export DISPLAY=172.22.215.129:0" >> /tiqsi-emacs/tiqsi.sh

RUN echo "XLIB_SKIP_ARGB_VISUALS=1 emacs -q -l /tiqsi-emacs/init.el &" >> /tiqsi-emacs/tiqsi.sh

WORKDIR /tiqsi-emacs/

