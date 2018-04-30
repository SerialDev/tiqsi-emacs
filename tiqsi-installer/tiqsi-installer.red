Red [title: "Basic Red GUI Widgets" Needs: 'View]

prompt-popup: function [
    "Prompts for a string.  Has OK/Cancel"
    msg [string!] "Message to display"
] [
    result: none ;-- in case user closes window with 'X'
    view/flags [
        msg-text: text msg center return
        in-field: field return
        yes-btn: button "OK" [result: in-field/text unview]
        no-btn: button "Cancel" [result: false unview]
        do [
            gap: 10 ;--between OK and Cancel
            ;-- enlarge text if small
            unless msg-text/size/x > (yes-btn/size/x + no-btn/size/x + gap) [
                msg-text/size/x: yes-btn/size/x + no-btn/size/x + gap
            ]

            win-centre: (2 * msg-text/offset/x + msg-text/size/x) / 2 ;-- centre buttons
            yes-btn/offset/x: win-centre - yes-btn/size/x - (gap / 2)
            no-btn/offset/x: win-centre + (gap / 2)
            in-field/size/x: 150
            in-field/offset/x: win-centre - (in-field/size/x / 2)
        ]
    ] [modal popup]
    result
]

prompt-popup-ip: func [] [
    print [ip: prompt-popup "Enter your IP address here for x11 export"]
]

prompt-popup-win32: func [] [
    print [win32: prompt-popup "Please Enter your runemacs.exe path | no quotes"]
]

prompt-popup-unix: func [] [
    print [unix: prompt-popup "Please Enter your emacs alias or executable path"]
]

view [
    below
    text font-size 16 "Tiqsi-emacs native/docker based install." red
    below
    check  "Win32 native" [prompt-popup-win32]
    below
    check  "Unix native" [prompt-popup-unix]
    below
    check "Docker" [prompt-popup-ip]


    below
    dfile: field 1 "FROM serialdev/sdev-ide:latest ^/^/ADD ./ /tiqsi-emacs ^/^/RUN git clone https://github.com/domtronn/all-the-icons.el.git ^/RUN cp /all-the-icons.el/fonts/all-the-icons.ttf /usr/local/share/fonts && \ ^/cp /all-the-icons.el/fonts/file-icons.ttf /usr/local/share/fonts && \ ^/cp /all-the-icons.el/fonts/fontawesome.ttf /usr/local/share/fonts && \ ^/cp /all-the-icons.el/fonts/material-design-icons.ttf /usr/local/share/fonts && \ ^/cp /all-the-icons.el/fonts/octicons.ttf /usr/local/share/fonts && \ ^/cp /all-the-icons.el/fonts/weathericons.ttf /usr/local/share/fonts && \ ^/cp /tiqsi-emacs/PragmataPro.ttf /usr/local/share/fonts ^/ENV DISPLAY="
    dfile2: field 1 ":0 ^/RUN echo ^"XLIB_SKIP_ARGB_VISUALS=1 emacs -q -l /tiqsi-emacs/init.el &^" >> /tiqsi-emacs/launch-tiqsi.sh && \ ^/chmod 777 /tiqsi-emacs/launch-tiqsi.sh ^/WORKDIR /tiqsi-emacs/"
    button red 300 "Install" [
    
      	   attempt [if any-string? form ip [
	   write %Dockerfile dfile/text
	   write/append %Dockerfile form ip
	   write/append %Dockerfile dfile2/text
	   print form ip]]

	   attempt [if any-string? form win32 [
	   write %run.bat ""
	   write/append %run.bat form win32
	   write/append %run.bat " -q -l init.el"]]

	   attempt[if any-string? form unix [
	   write %run.sh form unix
	   write/append %run.sh "-q -l init.el"]]

]
    area 400x200
    
]