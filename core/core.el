;;; core.el --- core initialization  -*- lexical-binding: t -*-

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

;; Thanks to :
;; http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
;; https://sites.google.com/site/steveyegge2/effective-emacs
;; https://github.com/ieure/scratch-el
;; https://github.com/jopecko/dotfiles
;; http://sachachua.com
;; https://www.wisdomandwonder.com/article/10395/hide-uninteresting-files-in-dired-mode - Hiding unnecesary files in dired mode
;; http://www.mygooglest.com/fni/dot-emacs.html
;; https://github.com/sjrmanning/.emacs.d/blob/master/core/sm-defuns.el
;; https://github.com/xuchunyang/region-state.el
;;  https://github.com/larstvei/dot-emacs#global-scale-mode GLOBAL SCALE MODE ;; TODO: Make sure this works well, extend to work with minimap
;; http://xenodium.com/#installing-emacs-spaceline
;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;; https://github.com/k-talo/volatile-highlights.el
;; https://github.com/fgeller/highlight-thing.el
;; https://github.com/nschum/highlight-symbol.el
;; https://github.com/jordonbiondo/column-enforce-mode
;; https://github.com/xuchunyang/region-state.el
;; http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
;; https://github.com/codemac/config/blob/master/emacs.d/boot.org
;; https://gitlab.com/emacs-stuff/indent-tools
;; https://github.com/malb/emacs.d/blob/master/malb.org
;; stackoverflow http://emacs.stackexchange.com/questions/18771/pop-up-documentation-with-pos-tip-or-popup
;; https://sriramkswamy.github.io/dotemacs/#orgheadline222
;; http://stackoverflow.com/questions/21955162/emacs-how-to-display-a-buffer-without-switching-window-and-without-raising-fram
;; https://github.com/emacsmirror/diminish
;; https://gist.github.com/antifuchs/9238468
;; http://sriramkswamy.github.io/dotemacs/
;; http://xenodium.com/#fishing-with-emacs
;; https://github.com/rejeep/emacs/blob/master/defuns.el
;; https://www.emacswiki.org/emacs/move-text.el and sk's init file
;; https://github.com/magnars/expand-region.el Magnars Expand Region
;; https://github.com/abo-abo/avy ;; still need to get used to it.
;; http://www.wilfred.me.uk/.emacs.d/init.html
;; http://wayback.archive.org/web/20010802153839/http://www.andersl.com:80/emacs/
;; https://github.com/codemac/config/blob/master/emacs.d/boot.org
;; https://github.com/codemac/config/blob/master/emacs.d/boot.org ;; TODO: further edit this to do it for one buffer
;; https://github.com/sabof/stripe-buffer stripe buffer to read tables better [this: https://github.com/sabof/stripe-buffer/pull/14 applied for performance]
;; https://github.com/domtronn/all-the-icons.el all the icons
;; https://github.com/howardabrams/dot-files/blob/master/emacs.org
;; http://chrishecker.com/images/4/4b/Remove_me.emacs
;; http://mbork.pl/2016-09-26_Emacs_now_suggests_shorter_ways_of_invocating_a_command
;; https://www.emacswiki.org/emacs/DelightedModes Delight to change line names, diminish does not have this functionality
;; https://github.com/afonso360/dotconf/  ;; Great Hydras
;; Amit P
;; Chris Hecker
;; Casey Muratory

(load-expand  "core/core-os.el")
(load-expand  "core/core-setup.el")
(load-expand  "core/core-ui.el")
(load-expand  "core/core-editing.el")

(load-expand  "core/core-navigation.el")
(load-expand  "core/core-files.el")
(load-expand  "core/core-functionality.el")
(load-expand  "core/core-functions.el")
(load-expand  "core/core-completion.el")
(load-expand  "core/core-debug.el")
(load-expand  "core/core-secrets.el")
(load-expand  "core/core-performance.el")


(provide 'core)

;;; core.el ends here
