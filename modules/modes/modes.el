;;; modes.el --- Tiqsi modes manager  -*- lexical-binding: t -*-

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



(load-expand  "modules/modes/modes-compilation.el")
(load-expand  "modules/modes/modes-flycheck.el")
(load-expand  "modules/modes/modes-helm.el")
(load-expand  "modules/modes/modes-ido.el")
(load-expand  "modules/modes/modes-dired.el")
(load-expand  "modules/modes/modes-magit.el")
(load-expand  "modules/modes/modes-minimap.el")
(load-expand  "modules/modes/modes-dash.el")
(load-expand  "modules/modes/modes-tramp.el")
(load-expand  "modules/modes/modes-waka.el")
(load-expand  "modules/modes/modes-avy.el")
(load-expand  "modules/modes/modes-erc.el")
(load-expand  "modules/modes/modes-org.el")
(load-expand  "modules/modes/modes-spotify.el")
(load-expand  "modules/modes/modes-parinfer.el")
(load-expand  "modules/modes/modes-neotree.el")
(load-expand  "modules/modes/modes-evil.el")
(load-expand  "modules/modes/modes-company.el")
(load-expand  "modules/modes/modes-flyspell.el")
(load-expand  "modules/modes/modes-shell.el")

(provide 'modes)

;;; modes.el ends here
