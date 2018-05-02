#!/bin/bash
rdm &
cp /tiqsi-emacs/PragmataPro.ttf /usr/local/share/fonts                                            
echo "XLIB_SKIP_ARGB_VISUALS=1 emacs -q -l /tiqsi-emacs/init.el &" > /tiqsi-emacs/launch-tiqsi.sh
chmod 777 /tiqsi-emacs/launch-tiqsi.sh
/bin/bash
