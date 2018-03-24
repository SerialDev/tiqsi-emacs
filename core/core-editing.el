;;; core-editing.el --- Tiqsi Editing

;;; Commentary:
;; 


;----{Commenting}---;


(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
   (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))


(defun append-as-kill ()
  "Performs copy-region-as-kill as an append."
  (interactive)
  (append-next-kill) 
  (copy-region-as-kill (mark) (point))
  )


;----{Replacing}----;


(defun casey-replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
            (narrow-to-region (mark) (point))
            (goto-char (point-min))
            (replace-string old-word new-word)
            ))
  )


(defun casey-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
  ))


;------{Saving}-----;


(defun casey-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))


;----{Inserting}----;

(defun add-full-stop ()
  "Terminate each line with a full stop."
  (interactive "*")
  (while (re-search-forward "$")
    (insert ".")
    (forward-char )))

(defun
  sdev/timestamp()
        (interactive)
        ; If you want to insert date and time, you can use:
        (insert(format-time-string "%A %d-%m-%Y %H:%M:%S ")))

;;; editing-defuns.el --- Basic text editing defuns -*- lexical-binding: t; -*-
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

;;; Still to find a good keymap for this one  
(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                             (and (looking-back ">" 1) (looking-at "<"))
                             (and (looking-back "(" 1) (looking-at ")"))
                             (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))   


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding "d" (? (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))


;---{Indentation}---;

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (point-to-register 'o)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (jump-to-register 'o))


;---{Modification}--;

(defun sk/goto-closest-number ()
  (interactive)
  (let ((closest-behind (save-excursion (search-backward-regexp "[0-9]" nil t)))
        (closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
    (push-mark)
    (goto-char
     (cond
      ((and (not closest-ahead) (not closest-behind)) (error "No numbers in buffer"))
      ((and closest-ahead (not closest-behind)) closest-ahead)
      ((and closest-behind (not closest-ahead)) closest-behind)
      ((> (- closest-ahead (point)) (- (point) closest-behind)) closest-behind)
      ((> (- (point) closest-behind) (- closest-ahead (point))) closest-ahead)
       (closest-ahead)))))

;; Once you go to the closest number, you might want to change it. These functions are useful for that.
	  
(defun sk/incs (s &optional num)
  (interactive "p")
  (let* ((inc (or num 1))
         (new-number (number-to-string (+ inc (string-to-number s))))
         (zero-padded? (s-starts-with? "0" s)))
    (if zero-padded?
        (s-pad-left (length s) "0" new-number)
      new-number)))

(defun sk/change-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
              (looking-back "[0-9]"))
    (sk/goto-closest-number))
  (save-excursion
    (while (looking-back "[0-9]")
      (forward-char -1))
    (re-search-forward "[0-9]+" nil)
    (replace-match (sk/incs (match-string 0) arg) nil nil)))

(defun sk/subtract-number-at-point (arg)
  (interactive "p")
  (sk/change-number-at-point (- arg)))

(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
                        ((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion
                           (goto-char end-of-first)
                           (move-backward-out-of-param)
                           (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion
                        (goto-char start-of-last)
                        (move-forward-out-of-param)
                        (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))


(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-is-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))


(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-is-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))


;------{Toggle}-----;

; yas-expand is run first and does what it has to, then it calls malb/indent-or-complete.

; This function then hopefully does what I want:

; if a region is active, just indent
; if we’re looking at a space after a non-whitespace character, we try some company-expansion
; otherwise call whatever would have been called otherwise.	  
	  
(defun malb/indent-or-complete (&optional arg)
  (interactive "P")
  (cond
   ;; if a region is active, indent
   ((use-region-p)
    (indent-region (region-beginning)
                   (region-end)))
   ;; if the next char is space or eol, but prev char not whitespace
   ((and (not (active-minibuffer-window))
         (or (looking-at " ")
             (looking-at "$"))
         (looking-back "[^[:space:]]")
         (not (looking-back "^")))

    (ac-expand arg))

   ;; no whitespace anywhere
   ((and (not (active-minibuffer-window))
         (looking-at "[^[:space:]]")
         (looking-back "[^[:space:]]")
         (not (looking-back "^")))
    (origami-toggle-node (current-buffer) (point)))

   ;; by default just call whatever was bound
   (t
    (let ((fn (or (lookup-key (current-local-map) (kbd "TAB"))
                  'indent-for-tab-command)))
      (if (not (called-interactively-p 'any))
          (fn arg)
        (setq this-command fn)
        (call-interactively fn))))))

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

(defun alternate-quotes-char ()
  (if (eq ?' (current-quotes-char)) ?\" ?'))

(defun toggle-quotes ()
  (interactive)
  (if (point-is-in-string-p)
      (let ((old-quotes (char-to-string (current-quotes-char)))
            (new-quotes (char-to-string (alternate-quotes-char)))
            (start (make-marker))
            (end (make-marker)))
        (save-excursion
          (move-point-forward-out-of-string)
          (backward-delete-char 1)
          (set-marker end (point))
          (insert new-quotes)
          (move-point-backward-out-of-string)
          (delete-char 1)
          (insert new-quotes)
          (set-marker start (point))
          (replace-string new-quotes (concat "\\" new-quotes) nil start end)
          (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point isn't in a string")))



;-----{Deleting}----;

(defun remove-blank-lines ()
  "Delete blank lines from the current buffer."
  (interactive "*")
  (while (re-search-forward "^$")
    (kill-line)))

;----{Kill Ring}----;

(defun pre-process-kill-ring-element (element)
  (replace-regexp-in-string "^[[:space:]]+" ""
                            (replace-regexp-in-string "[[:space:]]+$" "" (substring-no-properties element))))
 
(defun preprocess-kill-ring ()
  (let ((result nil)
        (element nil))
    (dolist (element kill-ring)
      (progn
        (setq element (pre-process-kill-ring-element element))
        (when (not (or
                    (eq 0 (length element))
                    (string-match-p "[\r\n]+" element)))
          (setq result (cons element result)))))
    (reverse result)))
 
(defun browse-kill-ring ()
  (interactive)
  (insert (completing-read "Pick an element: "
                       (preprocess-kill-ring))))
 



;---{Keybindings}---;
(define-key global-map [f8] 'casey-replace-string)

; Editting
(define-key global-map (kbd "C-q" )'copy-region-as-kill)
(define-key global-map (kbd "C-f" )'yank)
(define-key global-map "" 'nil)
(define-key global-map "" 'rotate-yank-pointer)
(define-key global-map "\eu" 'undo)
(define-key global-map "\e6" 'upcase-word)
(define-key global-map "\e^" 'captilize-word)
(define-key global-map "\e." 'fill-paragraph)
(define-key global-map "\el" 'casey-replace-in-region)
(define-key global-map "\eo" 'query-replace)
(define-key global-map "\eO" 'casey-replace-string)
; \377 is alt-backspace
(define-key global-map "\377" 'backward-kill-word)
(define-key global-map [M-delete] 'kill-word)
(define-key global-map "\e[" 'start-kbd-macro)
(define-key global-map "\e]" 'end-kbd-macro)
(define-key global-map "\e'" 'call-last-kbd-macro)
; Buffers
(define-key global-map "\er" 'revert-buffer)
(define-key global-map "\ek" 'kill-this-buffer)
(define-key global-map "\es" 'save-buffer)

(define-key global-map (kbd "S-<down>") 'open-line-below)
(define-key global-map (kbd "S-<up>") 'open-line-above)
(define-key global-map (kbd "S-<right>") 'new-line-in-between)
(define-key global-map (kbd "C-+") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

(global-set-key (kbd "C-c q") 'toggle-quotes)
(global-set-key (kbd "C-c t t") 'sdev/timestamp)
;(bind-key "<tab>" #'malb/indent-or-complete)

(provide 'core-editing)

;;; core-editing.el ends here
