extern crate clap;
use clap::{Arg, App, SubCommand};

use std::error::Error;
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;
use std::fmt;


fn create_file(path:String) -> std::fs::File {
    
    let path = Path::new(&path);
    let display = path.display();
    // Open a file in write-only mode, returns `io::Result<File>`
    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}",
                           display,
                           why.description()),
        Ok(file) => file,
    };
    return file;
}

fn main() {
    let matches = App::new("Tiqsi Installer")
        .version("0.0.1")
        .author("Andres Mariscal")
        .about("Generates dockerfile for tiqsi emacs")
        .arg(Arg::with_name("hostip")
             .short("ip")
             .long("hostip")
             .value_name("IP")
             .takes_value(true)
             .help("Sets the exported IP in dockerfile"))
        .get_matches();
    
    // // Gets a value for config if supplied by user, or defaults to "default.conf"
    let hostip = matches.value_of("hostip").unwrap_or("127.0.0.1");
    println!("hostIP: {}", hostip);

    let mut file = create_file("Dockerfile".to_string());
    

    let mut dockerfile = format!(r#"FROM serialdev/sdev-ide

ADD ./ /tiqsi-emacs


RUN git clone https://github.com/domtronn/all-the-icons.el.git

RUN cp /all-the-icons.el/fonts/all-the-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/file-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/fontawesome.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/material-design-icons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/octicons.ttf /usr/local/share/fonts && \
    cp /all-the-icons.el/fonts/weathericons.ttf /usr/local/share/fonts && \
    cp /tiqsi-emacs/PragmataPro.ttf /usr/local/share/fonts


ENV DISPLAY={}:0

RUN echo "XLIB_SKIP_ARGB_VISUALS=1 emacs -q -l /tiqsi-emacs/init.el &" >> /tiqsi-emacs/launch-tiqsi.sh && \
    chmod 777 /tiqsi-emacs/launch-tiqsi.sh

WORKDIR /tiqsi-emacs/
"#, hostip);



    match file.write_all(dockerfile.as_bytes()) {
        Err(why) => {
            panic!("couldn't write :{}", why.description())
        },
        Ok(_) => println!("successfully wrote " ),
    }


    
}
