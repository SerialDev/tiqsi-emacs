FROM serialdev/sdev-ide

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

