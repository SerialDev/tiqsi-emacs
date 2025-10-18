;;; test-raw-output.el --- Test raw Claude output -*- lexical-binding: t -*-

(defun test-claude-raw-output ()
  "Test Claude output to see what we're actually receiving."
  (interactive)
  
  ;; Create a buffer to capture raw output
  (with-current-buffer (get-buffer-create "*claude-raw-test*")
    (erase-buffer)
    
    ;; Create a simple process filter that just captures everything
    (let* ((process-filter (lambda (proc output)
                            (with-current-buffer "*claude-raw-test*"
                              (goto-char (point-max))
                              (insert "\n=== NEW OUTPUT CHUNK ===\n")
                              (insert output)
                              (insert "\n=== END CHUNK ===\n"))))
           (cmd (list "claude" "-p" "--output-format" "stream-json" "--verbose"))
           (process (make-process
                    :name "claude-raw-test"
                    :buffer (current-buffer)
                    :command cmd
                    :filter process-filter
                    :sentinel (lambda (proc event)
                                (message "Process ended: %s" event)))))
      
      ;; Send a simple message
      (process-send-string process "Say hello and remember the number 42\n")
      (process-send-eof process)
      
      (message "Test started. Check *claude-raw-test* buffer for output.")
      (display-buffer "*claude-raw-test*"))))

(defun test-claude-without-json ()
  "Test Claude without JSON streaming to compare."
  (interactive)
  
  (with-current-buffer (get-buffer-create "*claude-plain-test*")
    (erase-buffer)
    
    (let* ((process-filter (lambda (proc output)
                            (with-current-buffer "*claude-plain-test*"
                              (goto-char (point-max))
                              (insert output))))
           (cmd (list "claude" "-p"))
           (process (make-process
                    :name "claude-plain-test"
                    :buffer (current-buffer)
                    :command cmd
                    :filter process-filter
                    :sentinel (lambda (proc event)
                                (message "Plain process ended: %s" event)))))
      
      (process-send-string process "Say hello\n")
      (process-send-eof process)
      
      (message "Plain test started. Check *claude-plain-test* buffer.")
      (display-buffer "*claude-plain-test*"))))

(provide 'test-raw-output)
;;; test-raw-output.el ends here