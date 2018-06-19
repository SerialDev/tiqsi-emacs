FROM serialdev/sdev-ide:latest
ADD ./ /tiqsi-emacs
RUN cp /tiqsi-emacs/PragmataPro.ttf /usr/local/share/fonts
ENV DISPLAY=172.23.189.49:0
RUN echo "XLIB_SKIP_ARGB_VISUALS=1 emacs -q -l /tiqsi-emacs/init.el &" >> /tiqsi-emacs/launch-tiqsi.sh && \
    chmod 777 /tiqsi-emacs/launch-tiqsi.sh
WORKDIR /tiqsi-emacs/

