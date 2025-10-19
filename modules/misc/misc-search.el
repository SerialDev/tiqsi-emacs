;;; misc-search.el --- General search utilities  -*- lexical-binding: t -*-

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
;; General search utilities that can be used across different modes

;;; Code:

(defun ag-search-and-highlight (query)
  "Perform ag search and highlight results in another buffer"
  (interactive "sEnter search query: ")
  (let* ((results-buffer (get-buffer-create "*Ag Search Results*"))
          (ag-command "ag")
          (ag-arguments `("-l" "--nobreak" "--nocolor" "--hidden" ,query))
          (full-directory (shell-quote-argument (expand-file-name "."))))
    (with-current-buffer results-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Ag search results for '%s' in %s:\n\n" query default-directory)))
    (let ((process (apply 'start-file-process "ag-search" results-buffer ag-command ag-arguments)))
      (set-process-sentinel process
        (lambda (p e)
          (when (eq (process-status p) 'exit)
            (with-current-buffer (process-buffer p)
              (goto-char (point-max))
              (if (= (process-exit-status p) 0)
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward "^\\([^:]+\\):\\([0-9]+\\):" nil t)
                    (insert "\nSearch completed. Issues found.\n")
                    (insert "\nSearch completed. No matches found.\n")))
                (insert (format "\nSearch failed with error code %d.\n" (process-exit-status p))))
              (highlight-regexp (format "\\(%s\\)" (regexp-quote query)) 'hi-yellow)
              (setq buffer-read-only t)
              (display-buffer (process-buffer p)))))))))

(provide 'misc-search)

;;; misc-search.el ends here