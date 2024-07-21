;;; programming-llm.el --- Tiqsi support for LLMs  -*- lexical-binding: t -*-

;; Copyright (C) 2024-  Andres Mariscal

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
;; Inspired by Justine "jart" Tunney's implementation emacs-copilot
;;

(defgroup copilot nil
  "Large language model code completion."
  :prefix "copilot-"
  :group 'editing)

(defcustom copilot-bin
  "/Users/amariscalcloudflare.com/Documents/workdir/personal/llms/models/wizardcoder-python-34b-v1.0.Q5_K_M.llamafile"
  "Path of llamafile executable with LLM weights."
  :type 'string
  :group 'copilot)

;;;###autoload
(defun copilot-complete ()
  (interactive)
  (let* ((spot (point))
          (inhibit-quit t)
          (curfile (buffer-file-name))
          (cash (concat curfile ".cache"))
          (hist (concat curfile ".prompt"))
          (lang (file-name-extension curfile))

          ;; extract current line, to left of caret
          ;; and the previous line, to give the llm
          (code (save-excursion
                  (dotimes (i 2)
                    (when (> (line-number-at-pos) 1)
                      (previous-line)))
                  (beginning-of-line)
                  (buffer-substring-no-properties (point) spot)))

          ;; create new prompt for this interaction
          (system "\
You are an Emacs code generator. \
Writing comments is forbidden. \
Writing test code is forbidden. \
Writing English explanations is forbidden. ")
          (prompt (format
                    "[INST]%sGenerate %s code to complete:[/INST]\n```%s\n%s"
                    (if (file-exists-p cash) "" system) lang lang code)))

    ;; iterate text deleted within editor then purge it from prompt
    (when kill-ring
      (save-current-buffer
        (find-file hist)
        (dotimes (i 10)
          (let ((substring (current-kill i t)))
            (when (and substring (string-match-p "\n.*\n" substring))
              (goto-char (point-min))
              (while (search-forward substring nil t)
                (delete-region (- (point) (length substring)) (point))))))
        (save-buffer 0)
        (kill-buffer (current-buffer))))

    ;; append prompt for current interaction to the big old prompt
    (write-region prompt nil hist 'append 'silent)

    ;; run llamafile streaming stdout into buffer catching ctrl-g
    (with-local-quit
      (call-process copilot-bin nil (list (current-buffer) nil) t
        "--prompt-cache" cash
        "--prompt-cache-all"
        "--silent-prompt"
        "--temp" "0"
        "-c" "1024"
        "-ngl" "35"
        "-r" "```"
        "-r" "\n}"
        "-f" hist))

    ;; get rid of most markdown syntax
    (let ((end (point)))
      (save-excursion
        (goto-char spot)
        (while (search-forward "\\_" end t)
          (backward-char)
          (delete-backward-char 1 nil)
          (setq end (- end 1)))
        (goto-char spot)
        (while (search-forward "```" end t)
          (delete-backward-char 3 nil)
          (setq end (- end 3))))

      ;; append generated code to prompt
      (write-region spot end hist 'append 'silent))))

(defun get-openai-key ()
  "Returns the value of the OPENAI_KEY environment variable as a string."
  (let ((openai-key (getenv "OPENAI_KEY")))
    (if openai-key
      openai-key
      (error "The OPENAI_KEY environment variable is not set."))))

(setq openai-key (get-openai-key))

;; Ensure the 'shell-command' uses an interactive shell
(setq shell-command-switch "-ic")

(defun escape-shell-args (input-string)
  "Escape special characters in INPUT-STRING to safely use in shell commands."
  (replace-regexp-in-string
    "[\\\\\"'`$;()&|*?~<>^[]{}!#% \n]"  ; Matches a broad set of special characters
    "\\\\\\&"  ; Prefixes each matched character with a backslash
    input-string))

(straight-require 'ansi-color)

(defun cai-flow-call-region (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and display output in a side buffer."
  (interactive "r")  ; 'r' uses the current region, or prompts to select one if not already set
  (let* ((region-text (buffer-substring-no-properties begin end))
          (arg-string (shell-quote-argument (replace-regexp-in-string "\\s-+" " " region-text)))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string)))
    (with-current-buffer output-buffer
      (erase-buffer)
      ;; Use `shell-command` with properly directed output
      (shell-command shell-command-string (current-buffer) (current-buffer))
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; More robust handling for potential lack of output lines
      (goto-char (point-min))
      (when (>= (line-number-at-pos (point-max)) 10)
        (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point)))))
      ;; Handling window display
      (display-buffer-in-side-window output-buffer '((side . right)))))
  ;; Provide visual feedback that the command has executed
  (message "cai_flow executed with escaped and formatted region."))


;; (defun cai-flow-call-region (begin end)
;;   "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and display output in a side buffer."
;;   (interactive "r")  ; 'r' uses the current region, or prompts to select one if not already set
;;   (let* ((region-text (buffer-substring-no-properties begin end))
;;           (arg-string (shell-quote-argument  (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
;;           (output-buffer (get-buffer-create "*cai_flow-output*"))
;;           (shell-command-string (concat "source ~/.zshrc && cai " arg-string)))
;;     (with-current-buffer output-buffer
;;       (erase-buffer)
;;       (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
;;       (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
;;       ;; Delete the first 10 lines (assumed to be sourcing output, etc.)
;;       (goto-char (point-min))
;;       (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
;;       ;; Find an existing side window or open a new side window if none is available
;;       (let ((side-window (or (get-window-with-predicate
;;                                (lambda (window)
;;                                  (equal (window-parameter window 'window-side) 'right)))
;;                            (car (window-at-side-list nil 'right)))))
;;         (if side-window
;;           (set-window-buffer side-window output-buffer)  ; Use existing side window
;;           (display-buffer-in-side-window output-buffer '((side . right))))))
;;     ;; Provide visual feedback that the command has executed
;;     (message "cai_flow executed with escaped and formatted region.")))

(defun cai-flow-call-prompt ()
  "Prompt user for input and call cai_flow with the input, displaying output in a side buffer."
  (interactive)
  (let* ((input (read-string "Enter search query: "))
          (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " input)))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command shell-command-string (current-buffer) (current-buffer))
      ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; Delete the first 10 lines (assumed to be sourcing output, etc.)
      (goto-char (point-min))
      (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
      ;; Find an existing side window or open a new side window if none is available
      (let ((side-window (or (get-window-with-predicate
                               (lambda (window)
                                 (equal (window-parameter window 'window-side) 'right)))
                           (car (window-at-side-list nil 'right)))))
        (if side-window
          (set-window-buffer side-window output-buffer)  ; Use existing side window
          (display-buffer-in-side-window output-buffer '((side . right))))))
    ;; Provide visual feedback that the command has executed
    (message "cai_flow executed with escaped and formatted input.")))

(defun cai-flow-comment (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties begin end))
          (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
          (formatted-comment (shell-quote-argument (concat
                                                     "You are an editor function, a copilot, \n"
						     " you will convert the following code into a docstring \n"
						     " in the following format using this as a scaffold -> "
                                                     "* ---------------Function---------------\n"
                                                     "* <FUNCTION DESCRIPTION HERE>\n"
                                                     "* ----------------Returns---------------\n"
                                                     "* -> result ::str |'Success' if the operation was successful, 'Failure' otherwise\n"
                                                     "* ----------------Params----------------\n"
                                                     "* <PARAMS DESCRIPTION HERE>\n"
                                                     "* ----------------Usage-----------------\n"
                                                     "* ----------------Notes-----------------\n"
                                                     "The code you will edit is this, provide a docstring \n"
						     " be STRICT about the scaffold \n"
						     " Make --Function-- etc spaced into the middle and use * as a line starter \n"
						     "Do not repeat nonsense or boilerplate \n"
						     "Use <any> if type unknown\n"
						     "When checking returns, RETURN THE ACTUAL TYPES or object\n"
						     "YOU are a copilot you MUST adhere to these maxims, and please always include USAGE"
                                                     region-text "\n")))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command shell-command-string (current-buffer) (current-buffer))
      ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; Optionally delete unwanted output, e.g., the first 10 lines
      (goto-char (point-min))
      (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
      ;; Find an existing side window or open a new side window if none is available
      (let ((side-window (or (get-window-with-predicate
                               (lambda (window)
                                 (equal (window-parameter window 'window-side) 'right)))
                           (car (window-at-side-list nil 'right)))))
        (if side-window
          (set-window-buffer side-window output-buffer)  ; Use existing side window
          (display-buffer-in-side-window output-buffer '((side . right)))))
      ;; Provide visual feedback that the command has executed
      (message "cai_flow executed with escaped and formatted region."))))




(defun cai-flow-explain (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties begin end))
          (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
          (formatted-comment (shell-quote-argument (concat
                                                     "You are an editor function, a copilot, \n"
						     " you will explain the following code \n"
						     " in the following format using this as a scaffold -> "
                                                     "* ---------------Line explanations---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ----------------Returns---------------\n"
                                                     "* -> result ::str |'Success' if the operation was successful, 'Failure' otherwise\n"
                                                     "* ----------------When should you use this----------------\n"
                                                     "* <When should you use this functionality>\n"
                                                     "* ----------------Usage-----------------\n"
                                                     "* ----------------How to safely edit this section-----------------\n"
                                                     "The code you will edit is this, provide a docstring \n"
						     " be STRICT about the scaffold \n"
						     " Make --Function-- etc spaced into the middle and use * as a line starter \n"
						     "Do not repeat nonsense or boilerplate \n"
						     "Use <any> if type unknown\n"
						     "When checking returns, RETURN THE ACTUAL TYPES or object\n"
						     "YOU are a copilot you MUST adhere to these maxims, and please always include USAGE, and introduce a \n after 100 chars max"
                                                     region-text "\n")))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command shell-command-string (current-buffer) (current-buffer))
      ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; Optionally delete unwanted output, e.g., the first 10 lines
      (goto-char (point-min))
      (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
      ;; Find an existing side window or open a new side window if none is available
      (let ((side-window (or (get-window-with-predicate
                               (lambda (window)
                                 (equal (window-parameter window 'window-side) 'right)))
                           (car (window-at-side-list nil 'right)))))
        (if side-window
          (set-window-buffer side-window output-buffer)  ; Use existing side window
          (display-buffer-in-side-window output-buffer '((side . right)))))
      ;; Provide visual feedback that the command has executed
      (message "cai_flow executed with escaped and formatted region."))))


(defun cai-flow-explain-docs (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties begin end))
          (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
          (formatted-comment (shell-quote-argument (concat
						     "YOU ARE A Copyright editor, you take complex technical code into prose"
						     "Do not repeat nonsense or boilerplate \n"
						     "Use <any> if type unknown\n"
						     "Ignore the print calls, those are just my debug stuff"
						     "Now your important maxim is to create information we can directly use from this code"
						     "as documentation on a internal wiki, use the right speech patterns for a technical wiki"
						     "YOU are a copilot you MUST adhere to these maxims, and introduce a \n after 100 chars max"
                                                     region-text "\n")))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command shell-command-string (current-buffer) (current-buffer))
      ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; Optionally delete unwanted output, e.g., the first 10 lines
      (goto-char (point-min))
      (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
      ;; Find an existing side window or open a new side window if none is available
      (let ((side-window (or (get-window-with-predicate
                               (lambda (window)
                                 (equal (window-parameter window 'window-side) 'right)))
                           (car (window-at-side-list nil 'right)))))
        (if side-window
          (set-window-buffer side-window output-buffer)  ; Use existing side window
          (display-buffer-in-side-window output-buffer '((side . right)))))
      ;; Provide visual feedback that the command has executed
      (message "cai_flow executed with escaped and formatted region."))))


(defun cai-flow-transform-function (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties begin end))
          (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
          (formatted-comment (shell-quote-argument (concat
                                                     "You are an editor function, a copilot, \n"
						     "you will transform the following code \n"
						     "into a function of itself, with type annotations "
						     "extra effort must be paid to correctness, usability and re-usability"
						     "Do not repeat nonsense or boilerplate \n"
						     "name it based on its potential usability not just what it holds\n"
						     "make the naming verb driven, so transform_datatype vs datatype_transform \n"
						     "When you're done explain how to use it, and also WHEN \n"
						     "Use <any> if type unknown\n"
						     "When checking returns, RETURN THE ACTUAL TYPES or object\n"
						     "YOU are a copilot you MUST adhere to these maxims, and please always include USAGE, and introduce a \n after 100 chars max"
                                                     region-text "\n")))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command shell-command-string (current-buffer) (current-buffer))
      ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; Optionally delete unwanted output, e.g., the first 10 lines
      (goto-char (point-min))
      (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
      ;; Find an existing side window or open a new side window if none is available
      (let ((side-window (or (get-window-with-predicate
                               (lambda (window)
                                 (equal (window-parameter window 'window-side) 'right)))
                           (car (window-at-side-list nil 'right)))))
        (if side-window
          (set-window-buffer side-window output-buffer)  ; Use existing side window
          (display-buffer-in-side-window output-buffer '((side . right)))))
      ;; Provide visual feedback that the command has executed
      (message "cai_flow executed with escaped and formatted region."))))

(defun cai-flow-debug-functionality (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties begin end))
          (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
          (formatted-comment (shell-quote-argument (concat
                                                     "You are an editor function, a copilot, \n"
						     "you will inspect the following code \n"
						     "try to identify and scaffold the following "
						     " in the following format using this as a scaffold -> "
                                                     "* ---------------type errors---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Correct usage lines---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Incorrect usage lines---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Potential logic errors---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Overall Whether you believe this is Correct or not---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Implementation---------------\n"
                                                     "* <Complete implementation of a fix, all code>\n"
						     "YOU are a copilot you MUST adhere to these maxims, and please always include USAGE, and introduce a \n after 100 chars max"
                                                     region-text "\n")))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command shell-command-string (current-buffer) (current-buffer))
      ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; Optionally delete unwanted output, e.g., the first 10 lines
      (goto-char (point-min))
      (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
      ;; Find an existing side window or open a new side window if none is available
      (let ((side-window (or (get-window-with-predicate
                               (lambda (window)
                                 (equal (window-parameter window 'window-side) 'right)))
                           (car (window-at-side-list nil 'right)))))
        (if side-window
          (set-window-buffer side-window output-buffer)  ; Use existing side window
          (display-buffer-in-side-window output-buffer '((side . right)))))
      ;; Provide visual feedback that the command has executed
      (message "cai_flow executed with escaped and formatted region."))))



(defun cai-flow-debug-performance (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties begin end))
          (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
          (formatted-comment (shell-quote-argument (concat
                                                     "You are an editor function, a copilot, \n"
						     "you will inspect the following code \n"
						     "try to identify and scaffold the following \n "
						     "BAD performance IS an error "
						     " in the following format using this as a scaffold -> "
                                                     "* ---------------Potential Performance errors---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Cache locality issues---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Memory Layout issues---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------O(n) issues---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Potential Performance improvements---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* ---------------Overall Whether you believe this is Correct or not <bad performance is incorrect>---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
						     "YOU are a copilot you MUST adhere to these maxims"
                                                     "* ---------------Resolution recommendations---------------\n"
                                                     "* <DESCRIPTION HERE>\n"
                                                     "* <CODE here>\n"
                                                     region-text "\n")))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command shell-command-string (current-buffer) (current-buffer))
      ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; Optionally delete unwanted output, e.g., the first 10 lines
      (goto-char (point-min))
      (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
      ;; Find an existing side window or open a new side window if none is available
      (let ((side-window (or (get-window-with-predicate
                               (lambda (window)
                                 (equal (window-parameter window 'window-side) 'right)))
                           (car (window-at-side-list nil 'right)))))
        (if side-window
          (set-window-buffer side-window output-buffer)  ; Use existing side window
          (display-buffer-in-side-window output-buffer '((side . right)))))
      ;; Provide visual feedback that the command has executed
      (message "cai_flow executed with escaped and formatted region."))))

(defun cai-flow-complete (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties begin end))
          (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
          (formatted-comment (shell-quote-argument (concat
                                                     "You are an editor function, a copilot, \n"
						     "You are an Emacs code generator. \
Writing comments is forbidden. \
Writing test code is forbidden. \
Writing English explanations is forbidden. \n "
						     "Emphasis on correctness and performance always"
						     "try to complete the following code "
						     "YOU are a copilot you MUST adhere to these maxims"
                                                     region-text "\n")))
          (output-buffer (get-buffer-create "*cai_flow-output*"))
          (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command shell-command-string (current-buffer) (current-buffer))
      ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
      (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
      ;; Optionally delete unwanted output, e.g., the first 10 lines
      (goto-char (point-min))
      (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
      ;; Find an existing side window or open a new side window if none is available
      (let ((side-window (or (get-window-with-predicate
                               (lambda (window)
                                 (equal (window-parameter window 'window-side) 'right)))
                           (car (window-at-side-list nil 'right)))))
        (if side-window
          (set-window-buffer side-window output-buffer)  ; Use existing side window
          (display-buffer-in-side-window output-buffer '((side . right)))))
      ;; Provide visual feedback that the command has executed
      (message "cai_flow executed with escaped and formatted region."))))

(defun cai-flow-custom (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (let ((custom-input (read-string "Enter custom text: ")))
    (let* ((region-text (buffer-substring-no-properties begin end))
            (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
            (formatted-comment (shell-quote-argument (concat
                                                       "You are an editor function, a copilot, \n"
						       custom-input
						       "\nYOU are a copilot you MUST adhere to these maxims, and please always include USAGE"
                                                       region-text "\n")))
            (output-buffer (get-buffer-create "*cai_flow-output*"))
            (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
      (with-current-buffer output-buffer
	(erase-buffer)
	(shell-command shell-command-string (current-buffer) (current-buffer))
	;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
	(ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
	;; Optionally delete unwanted output, e.g., the first 10 lines
	(goto-char (point-min))
	(dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
	;; Find an existing side window or open a new side window if none is available
	(let ((side-window (or (get-window-with-predicate
				 (lambda (window)
                                   (equal (window-parameter window 'window-side) 'right)))
                             (car (window-at-side-list nil 'right)))))
          (if side-window
            (set-window-buffer side-window output-buffer)  ; Use existing side window
            (display-buffer-in-side-window output-buffer '((side . right)))))
	;; Provide visual feedback that the command has executed
	(message "cai_flow executed with escaped and formatted region.")))))

;; TODO debug this further
(defun cai-flow-continue (begin end)
  "Call cai_flow with the selected text from BEGIN to END, formatted as a single line, and append a formatted comment."
  (interactive "r")
  (kill-new (buffer-substring-no-properties begin end))  ; Copy the region to the clipboard
  (let ((custom-input (read-string "Enter custom text: ")))
    (let* ((region-text (buffer-substring-no-properties begin end))
            (output-buffer (get-buffer "*cai_flow-output*"))
            (conversation-context (with-current-buffer output-buffer
                                    (buffer-string)))
            (arg-string (shell-quote-argument (replace-regexp-in-string "[ \t\n\r]+" " " region-text)))
            (formatted-comment (shell-quote-argument (concat
                                                       "You are an editor function, a copilot, \n"
                                                       custom-input
                                                       "\nYOU are a copilot you MUST adhere to these maxims, and please always include USAGE"
                                                       conversation-context
                                                       region-text "\n")))
            (shell-command-string (concat "source ~/.zshrc && cai " arg-string " " formatted-comment)))
      (with-current-buffer output-buffer
        (erase-buffer)
	(shell-command shell-command-string (current-buffer) (current-buffer))
        ;; (shell-command shell-command-string (current-buffer) t)  ; t means insert output at point
        (ansi-color-apply-on-region (point-min) (point-max))  ; Apply ANSI color to the entire buffer
        ;; Optionally delete unwanted output, e.g., the first 10 lines
        (goto-char (point-min))
        (dotimes (_ 10) (delete-region (point) (progn (forward-line 1) (point))))
        ;; Find an existing side window or open a new side window if none is available
        (let ((side-window (or (get-window-with-predicate
                                 (lambda (window)
                                   (equal (window-parameter window 'window-side) 'right)))
                             (car (window-at-side-list nil 'right)))))
          (if side-window
            (set-window-buffer side-window output-buffer)  ; Use existing side window
            (display-buffer-in-side-window output-buffer '((side . right)))))
        ;; Provide visual feedback that the command has executed
        (message "cai_flow executed with escaped and formatted region.")))))

(defhydra hydra-cai-flow (:color blue :hint nil)
  "
` ` _ _ _ _ _ _ _ _ _` ` ` ` | ^CAI Flow^
`  |_ _ _ _ _ _ _ _ _ |` ` ` | -----------------------------------------------------------
` ` ` \\\\ \\ \\ // ///` ` ` ` ` | _c_: Custom               _C_: Complete
` ` `  \\\\_|_|_| // ` ` ` ` ` | _df_: Debug Functionality  _e_: Explain
 Tiqsi |        |` ` ` ` ` ` | _t_: Transform into Function _o_: Continue from last
` ` `  | o    o | Emacs` ` ` | _m_: Comment  _dp_: Debug Performance
` ` _ _ _ _ _ _ _ _ _  ` ` ` | _r_: Call Region
`  |_ _ _ _ _ _ _ _ _ |` ` ` | _p_: Call Prompt
` ` ` ` \\_ _ _ /` ` ` ` ` ` `|"
  ("c" cai-flow-custom)
  ("C" cai-flow-complete)
  ("df" cai-flow-debug-functionality)
  ("dp" cai-flow-debug-performance)
  ("e" cai-flow-explain)
  ("t" cai-flow-transform-function)
  ("m" cai-flow-comment)
  ("r" cai-flow-call-region)
  ("p" cai-flow-call-prompt)
  ("o" cai-flow-continue)
  ("ESC" nil "Exit"))

;; define `ctrl-c ctrl-k` keybinding for llm code completion
(defun copilot-c-hook ()
  (define-key c-mode-base-map (kbd "C-c C-k") 'copilot-complete))
(add-hook 'c-mode-common-hook 'copilot-c-hook)

(defun copilot-py-hook ()
  (define-key python-mode-map (kbd "C-c C-k") 'copilot-complete))
(add-hook 'python-common-hook 'copilot-py-hook)
(global-set-key (kbd "C-c C-k") 'copilot-complete)

(global-set-key (kbd "C-k") 'hydra-cai-flow/body)

(straight-use-package
  '(openai
     :type git
     :host github
     :ensure t
     :repo "emacs-openai/openai"
     ))


(straight-use-package
  '(chatgpt
     :type git
     :host github
     :ensure t
     :repo "emacs-openai/chatgpt"
     ))


(provide 'programming-llm)

;;; programming-llm.el ends here
