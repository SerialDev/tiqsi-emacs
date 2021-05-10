;;; modes-shell.el --- Tiqsi shell buffer support  -*- lexical-binding: t -*-

;; Copyright (C) 2018-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>

;; URL: https://github.com/serialdev/tiqsi-emacs
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(defun send-to-shell(command-string)
  (shell)
  (with-current-buffer "*shell*"
    (let ((process (get-buffer-process (current-buffer)))
          )
      (unless process
        (error "No process in %s" buffer-or-name))
      (goto-char (process-mark process))
      (insert command-string)
      (comint-send-input nil t )
        )))



; ------------------------------------------------------------------------- ;


(multiple-async-shell-commands "*Output*"
			       "echo 1; sleep 1"
			       "echo 2; sleep 1"
			       "echo 2; sleep 1"
			       "echo 3; sleep 1")


; ------------------------------------------------------------------------- ;


;; --------------------------------Create a frame with tooltip---------------------
;; TODO Make tip based on tip char len + height

(setq tip-frame-params
      '(
	(minibuffer . nil)
	(name . "*Tip Frame*")
	(lambda () (setq mode-line-format nil))
	(visibility . nil)
	(minibuffer-frame-alist nil)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(line-spacing . 0)
	(unsplittable . t)
	(undecorated . t)
	(mouse-wheel-frame . nil)
	(no-other-frame . t)
	(cursor-type . nil)
	(inhibit-double-buffering . t)
	(drag-internal-border . t)
	(no-special-glyphs . t)
	(no-accept-focus . t)
	(no-focus-on-map . t)
	(internal-border-width . 1)
	(right-fringe . 0)
	(left-fringe . 0)
	(top . -1)
	(desktop-dont-save . t)
	(left . -1)))


(defun frame--set-input-focus (frame)
  ;; Ensure, if possible, that FRAME gets input focus.
  (when (memq (window-system frame) '(x w32 ns))
    (x-focus-frame frame)))

(defun make-tip-frame (tip &rest args)
  (setq tip-frame (make-frame
		   (append (append
			    tip-frame-params
			    `((width . ,(+ (* (/ (frame-char-width) 2) (length tip)) (frame-char-width)))))
			   `((height . ,(*(frame-char-height) 2))))))

    ;; (generate-new-buffer "*Tip Frame Buffer*")

    (set-frame-position tip-frame
			(- (car (window-absolute-pixel-position)) (frame-char-width))
			(+ (cdr (window-absolute-pixel-position)) (frame-char-size)))


    (let ((current-frame (selected-frame) ))
      (make-frame-visible tip-frame)
      (select-frame tip-frame)
      (pop-to-buffer "*Tip Frame*")
      (with-current-buffer "*Tip Frame*"
	(fundamental-mode)
	(setq-local beacon-mode nil)
	(setq mode-line-format nil)
	(set-background-color "#5F55FF")
	(linum-mode -1)
	(insert  tip)
	)
      (frame--set-input-focus current-frame)
      (frame-restack current-frame tip-frame)
      )
    )



(defun close-tip-frame()
  (with-current-buffer "*Tip Frame*"
    (delete-region (point-min) (point-max)))
  (delete-frame tip-frame))

;; (make-tip-frame "whyssssssss")
;; (close-tip-frame)


;  -------------------------------------------------------------------------------- ;


;; (make-tip-frame "e")
;; (async-shell-command "ls" "*Tip Frame Buffer*")

(defun tooltip-command (shell-command-to-execute)
  (while-no-input
    (let ((command (shell-command-to-string shell-command-to-execute) ))
      (make-tip-frame command)
      (sit-for 3)
      ))
  (close-tip-frame)
  )

; ------------------------------------------------------------------------- ;
;                           TODO: Make this async                           ;
; ------------------------------------------------------------------------- ;

(defmacro create-tooltip-command(command-name command-to-execute)
  (let ((current-command (s-prepend "command-tooltip-" command-name) ))
    `(defun ,(intern current-command) ()
       (interactive)
       (pos-tip-show (shell-command-to-string ,command-to-execute)))))

(defmacro create-tooltip-prompt-command(command-name command-to-execute prompt)
  (let ((current-command (s-prepend "command-tooltip-" command-name) ))
    `(defun ,(intern current-command) (,(intern prompt))
       (interactive ,(s-prepend "sEnter " (s-prepend prompt ": ")))
       (pos-tip-show (shell-command-to-string (s-prepend ,command-to-execute ,(intern prompt))))

       )))



;;:8181

(straight-use-package
 '(extractor
   :type git
   :host github
   :ensure t
   :repo "DamienCassou/extractor-el"
))


(straight-use-package
 '(etop-mode
   :type git
   :host github
   :ensure t
   :repo "DamienCassou/etop-mode"
))


(when-executable
 "nmap"
 (progn
   ;; Which ports are listening for TCP connections from the network
   ;; #notice that some companies might not like you using nmap
   (create-tooltip-command "ports-listening-tcp" "nmap -sT -O localhost")
))


(with-system darwin
  ()
)


(create-tooltip-command "ls" "ls")
(create-tooltip-command "count-running-processes" "ps aux | wc -l")
(create-tooltip-command "display-uid" "cut -d ':' -f 1,3 /etc/passwd | sort -t ':' -k2n - | tr ':' '\t'")
(create-tooltip-command "process-memory" "ps aux | awk '{if ($5 != 0 ) print $2,$5,$6,$11}' | sort -k2n")
(create-tooltip-command "top-largest" "du -sk /var/log/* | sort -r -n | head -10")
(create-tooltip-command "count-all-files" "ls |xargs -n1 wc -l | sort -r -n")
(create-tooltip-command "show-memory-usage" "free -c 1 -mhs 1")
(create-tooltip-command "show-uptime" "uptime")
(create-tooltip-command "show-running-processes" "ps aux") ;; Make visual-line-mode active
(create-tooltip-command "show-process-tree" "pstree")
(create-tooltip-command "show-kernel-ring-buffer" "dmesg | tail -20")
(create-tooltip-command "show-maximum-n-processes" "cat /proc/sys/kernel/pid_max")
(create-tooltip-command "show-system-version" "cat /etc/*-release")
(create-tooltip-command "show-current-jobs" "jobs -l" )
(create-tooltip-command "show-system-info" "uname -a" )
(create-tooltip-command "show-system-platform" "uname -i" )
(create-tooltip-command "show-system-processor" "uname -mp" )
;; TODO make a version that asks what port (of the macro) 
(create-tooltip-command "show-open-ports" "sudo ss -tulpn" )
(create-tooltip-prompt-command "find-application-using-port" "netstat -ap | grep :" "port" )



;; TODO allow for internal {} f-string style replacement
(create-tooltip-prompt-command "kill-all-processes-of-a-program" "kill -9 $(ps aux | grep 'program_name' | awk '{print $2}')netstat -ap | grep :" "program-name" )


;;
(create-tooltip-command "show-system-alias-list" "alias -p" )
(create-tooltip-command "show-disk-usage" "df -h" )
(create-tooltip-command "show-dir-usage" "du -h" )
(create-tooltip-command "show-dir-tree" "tree" )
(create-tooltip-command "show-cpu-info" "lscpu" )
(create-tooltip-command "show-libs-in-cache" "ldconfig -p" )
(create-tooltip-command "count-num-cores" "nproc --all" )
(create-tooltip-command "show-running-services" "service --status-all" )
(create-tooltip-command "show-weather-forecast" "curl wttr.in/hel" )


(defun tidy-html(current-val)
  (interactive "sString Add: ")
   (let ((current-region-text
	  current-val ))
     (progn
       (print current-region-text)
      (kill-new
       (shell-command-to-string
	(s-prepend
	 (s-prepend "echo '" current-region-text )
	 "' | tidy --show-body-only yes -i 4 -w 75 -m -quiet --force-output y -wrap 0 2>/dev/null " ))))))




; ------------------------------------------------------------------------- ;
;                                Improvements                               ;
; ------------------------------------------------------------------------- ;

;; Print or control the kernel ring buffer
;; dmesg
;; (create-tooltip-command "show-kernel-alert" "dmesg - -level=alert")
;; (create-tooltip-command "show-kernel-memory" "dmesg | grep -i memory")


;; TODO: improve the macro || alternative macro for the following use-cases 

;; Copy your default public key to remote user
;; ssh-copy-id <user_name>@<server_IP>

;; Display file status (size; access, modify and change time, etc) of a file (e.g. filename.txt)
;; stat filename.txt

;; Print information related to USB
;; $ dmesg | grep -i usb

;; Print with human readable time
;; $ dmesg  - -ctime

;; Print dmesg based on facility 
;; dmesg --facility=daemon
;; kern - kernel messages
;; user - random user-level messages
;; mail - mail system
;; daemon - system daemons
;; auth - security/authorization messages
;; syslog - messages generated internally by syslogd
;; lpr - line printer subsystem
;; news - network news subsystemt

;; Print log level
;; dmesg - -level=err
;; Supported levels
;; emerg - system is unusable
;; alert - action must be taken immediately
;; crit - critical conditions
;; err - error conditions
;; warn - warning conditions
;; notice - normal but significant condition
;; info - informational
;; debug - debug-level messages

;; Set audible alarm when an IP address comes online
;; ping -i 60 -a IP_address

;; List of commands used most often
;; history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head

;; Display the top ten running processes - sorted by memory usage
;; ps aux | sort -nk +4 | tail

;; Delete all files in a folder that don't match a certain file extension
;; rm !(*.foo|*.bar|*.baz)

;; Kill a process locking a file 
;; fuser -k filename

;; Dup finder
;; find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate

;; Find the process you are looking for minus the grepped one
;; ps aux | grep [p]rocess-name

;; Extract tarball from internet without local saving
;; wget -qO - "http://www.tarball.com/tarball.gz" | tar zxvf -

;; Copy your ssh public key to a server from a machine that doesn't have ssh-copy-id
;; cat ~/.ssh/id_rsa.pub | ssh user@machine "mkdir ~/.ssh; cat >> ~/.ssh/authorized_keys"

;; Graphical tree of sub-directories
;; ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/ /' -e 's/-/|/'

;; intercept stdout/stderr of another process
;; strace -ff -e trace=write -e write=1,2 -p SOME_PID

;; Make directory including intermediate directories
;; mkdir -p a/long/directory/path

;; Easily search running processes (alias).
;; alias 'ps?'='ps ax | grep '

;; Graph # of connections for each hosts.
;; netstat -an | grep ESTABLISHED | awk '{print $5}' | awk -F: '{print $1}' | sort | uniq -c | awk '{ printf("%s\t%s\t",$2,$1) ; for (i = 0; i < $1; i++) {printf("*")}; print "" }'

;; Monitor the queries being run by MySQL
;; watch -n 1 mysqladmin --user=<user> --password=<password> processlist

;; Show numerical values for each of the 256 colors in bash
;; for code in {0..255}; do echo -e "\e[38;05;${code}m $code: Test"; done

;; Recursively remove all empty directories
;; find . -type d -empty -delete

;; Processor / memory bandwidth in GB/s
;; dd if=/dev/zero of=/dev/null bs=1M count=32768

;; pretend to be busy in office to enjoy a cup of coffee
;; cat /dev/urandom | hexdump -C | grep "ca fe"

;; Create a persistent connection to a machine
;; ssh -MNf <user>@<host>

;; Nice weather forecast on your shell
;; curl wttr.in/<city>

;; Show a 4-way scrollable process tree with full details. whenb a process breaks remember awwfux
;; ps awwfux | less -S

;; Attach screen over ssh
;; ssh -t remote_host screen -r

;; snapshot of the open files for a PID 1234 then waits 10 seconds and takes another
;; snapshot of the same PID, it then displays the difference between each snapshot
;; to give you an insight into what the application is doing.
;; diff <(lsof -p 1234) <(sleep 10; lsof -p 1234)

;; which program does this port belong to
;; lsof -i tcp:80

;; Draw kernel module dependancy graph.
;; lsmod | perl -e 'print "digraph \"lsmod\" {";<>;while(<>){@_=split/\s+/; print "\"$_[0]\" -> \"$_\"\n" for split/,/,$_[3]}print "}"' | dot -Tpng | display -

;; Compare two directory trees.
;; diff <(cd dir1 && find | sort) <(cd dir2 && find | sort)

;; Download all images from a site
;; wget -r -l1 --no-parent -nH -nd -P/tmp -A".gif,.jpg" http://example.com/images

;; Find out how much data is waiting to be written to disk
;; grep ^Dirty /proc/meminfo

;; Show apps using itnernet connection at the moment 
;; lsof -P -i -n | cut -f 1 -d " "| uniq | tail -n +2

;; Recursively change permissions on files, leave directories alone.
;; find ./ -type f -exec chmod 644 {} \;

;; Find files that have been modified on your system in the past 60 minutes
;; sudo find / -mmin 60 -type f

;; Intercept, monitor and manipulate a TCP connection.
;; Forwards localhost:1234 to machine:port, running all data through your chain of piped commands. The above command logs inbound and outbound traffic to two files. Tip: replace tee with sed to manipulate the data in real time (use "sed -e 's/400 Bad Request/200 OK/'" to tweak a web server's responses ;-) Limitless possibilities.
;; mkfifo /tmp/fifo; cat /tmp/fifo | nc -l -p 1234 | tee -a to.log | nc machine port | tee -a from.log > /tmp/fifo

;; run complex remote shell cmds over ssh, without escaping quotes
;; Much simpler method. More portable version: ssh host -l user "`cat cmd.txt`"
;; ssh host -l user $(<cmd.txt)

;; output your microphone to a remote computer's speaker
;; arecord -f dat | ssh -C user@host aplay -f dat

;; analyze traffic remotely over ssh w/ wireshark
;; ssh root@server.com 'tshark -f "port !22" -w -' | wireshark -k -i -

;; Lists all listening ports together with the PID of the associated process
;; lsof -Pan -i tcp -i udp

;; easily find megabyte eating files or directories
;; alias dush="du -sm *|sort -n|tail"

;; exit without saving history
;; also works perfectly in shells that don't have $$ if you do something like kill -9 `readlink /proc/self`
;; kill -9 $$

;; find all file larger than 500M
;; find / -type f -size +500M

;; live ssh network throughput test
;; using /dev/random (combined with compression) would give a good indication of the throughput.
;; pv /dev/zero|ssh $host 'cat > /dev/null'

;; List the number and type of active network connections
;; netstat -ant | awk '{print $NF}' | grep -v '[a-z]' | sort | uniq -c

;; Brute force discover
;; sudo zcat /var/log/auth.log.*.gz | awk '/Failed password/&&!/for invalid user/{a[$9]++}/Failed password for invalid user/{a["*" $11]++}END{for (i in a) printf "%6s\t%s\n", a[i], i|"sort -n"}'

;; Resume scp of a big file
;; rsync --partial --progress --rsh=ssh $file_source $user@$host:$destination_file

;; Analyse an Apache access log for the most common IP addresses
;; tail -10000 access_log | awk '{print $1}' | sort | uniq -c | sort -n | tail

;; Analyse compressed Apache access logs for the most commonly requested pages
;; zcat access_log.*.gz | awk '{print $7}' | sort | uniq -c | sort -n | tail -n 20

;; processes per user counter
;; ps hax -o user | sort | uniq -c

;; convert filenames in current directory to lowercase
;; rename -f 'y/A-Z/a-z/' *

;; Limit the cpu usage of a process
;; sudo cpulimit -p pid -l 50

;; List alive hosts in specific subnet
;; nmap -sP 192.168.1.0/24

;; Matrix style
;; echo -e "\e[32m"; while :; do for i in {1..16}; do r="$(($RANDOM % 2))"; if [[ $(($RANDOM % 5)) == 1 ]]; then if [[ $(($RANDOM % 4)) == 1 ]]; then v+="\e[1m $r "; else v+="\e[2m $r "; fi; else v+=" "; fi; done; echo -e "$v"; v=""; done

;; sniff network traffic on a given interface and displays the IP addresses of the machines communicating with the current host (one IP per line)
;; sudo tcpdump -i wlan0 -n ip | awk '{ print gensub(/(.*)\..*/,"\\1","g",$3), $4, gensub(/(.*)\..*/,"\\1","g",$5) }' | awk -F " > " '{print $1"\n"$2}'

;; List all open ports and their owning executables
;; lsof -i -P | grep -i "listen"

;; List files accessed by a command
;; strace -ff -e trace=file chmod 2>&1 | sed -n 's/^[^"]*"\([^"]*\)".*/\1/p'  | sort | uniq

;; get all pdf and zips from a website using wget
;; If the site uses https, use: wget --reject html,htm --accept pdf,zip -rl1 --no-check-certificate https-url
;; wget --reject html,htm --accept pdf,zip -rl1 url

;; Show me a histogram of the busiest minutes in a log file
;; cat /var/log/secure.log | awk '{print substr($0,0,12)}' | uniq -c | sort -nr | awk '{printf("\n%s ",$0) ; for (i = 0; i<$1 ; i++) {printf("*")};}'

;; Generate a Random MAC address
;; MAC=`(date; cat /proc/interrupts) | md5sum | sed -r 's/^(.{10}).*$/\1/; s/([0-9a-f]{2})/\1:/g; s/:$//;'`

;; All IP connected to my host
;; netstat -nut | awk -F'[ :]+' '/SHED *$/{print $6}' | sort -u 

;; Create a script of the last executed command
;; echo "!!" > foo.sh

;; network activity in realtime
;; lsof -i

;; Show apps that use internet connection at the moment
;; lsof -P -i -n

;; Sharing file through http 80 port
;; nc -v -l 80 < file.ext

;; Print shared library dependencies (e.g. for ‘ls’)
;; ldd /bin/ls

;; This will show whether the target is a builtin, a function, an alias or an external executable
;; type -a lshw

;; Kill all process of a program
;; kill -9 $(ps aux | grep 'program_name' | awk '{print $2}')

; ------------------------------------------------------------------------- ;
; ------------------------------------------------------------------------- ;



;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd





(provide 'modes-shell)

;;; modes-shell.el ends here

