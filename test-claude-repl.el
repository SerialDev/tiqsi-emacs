;;; test-claude-repl.el --- Test the Claude REPL thoroughly -*- lexical-binding: t -*-

;; Test scenarios for tiqsi-claude-repl

(require 'tiqsi-claude-repl)

(defun test-claude-repl-all ()
  "Run all Claude REPL tests."
  (interactive)
  (message "Starting Claude REPL tests...")
  
  ;; Test 1: Check if Claude CLI is available
  (test-claude-repl-cli-availability)
  
  ;; Test 2: Start a new session
  (test-claude-repl-start-session)
  
  ;; Test 3: Test JSON parsing
  (test-claude-repl-json-parsing)
  
  ;; Test 4: Test session management
  (test-claude-repl-session-management)
  
  ;; Test 5: Test UI elements
  (test-claude-repl-ui-elements)
  
  (message "All tests completed!"))

(defun test-claude-repl-cli-availability ()
  "Test if Claude CLI is available."
  (message "\n=== Test 1: CLI Availability ===")
  (if (tiqsi-claude-repl--executable-available-p)
      (message "✅ Claude CLI found at: %s" (executable-find tiqsi-claude-repl-program))
    (message "❌ Claude CLI not found! Install with: npm install -g @anthropic-ai/claude-code")))

(defun test-claude-repl-start-session ()
  "Test starting a new Claude REPL session."
  (message "\n=== Test 2: Starting Session ===")
  (condition-case err
      (progn
        (tiqsi-claude-repl-start)
        (message "✅ Session started successfully")
        (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
          (message "✅ Buffer created: %s" (buffer-name))
          (message "✅ Session ID: %s" (or tiqsi-claude-repl--session-id "not set"))
          (message "✅ Buffer size: %d chars" (buffer-size))))
    (error (message "❌ Error starting session: %s" err))))

(defun test-claude-repl-json-parsing ()
  "Test JSON parsing with sample output."
  (message "\n=== Test 3: JSON Parsing ===")
  (let ((sample-json-lines
         '("{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-123\"}"
           "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hello, I'm Claude!\"}]}}"
           "{\"type\":\"result\",\"duration_ms\":1000}")))
    (dolist (json-line sample-json-lines)
      (condition-case err
          (let* ((json-obj (json-parse-string json-line))
                 (type (gethash "type" json-obj)))
            (message "✅ Parsed JSON type: %s" type))
        (error (message "❌ JSON parse error: %s" err))))))

(defun test-claude-repl-session-management ()
  "Test session management features."
  (message "\n=== Test 4: Session Management ===")
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    ;; Test session recovery
    (tiqsi-claude-repl-recover-session)
    (message "✅ Session recovery completed")
    
    ;; Test prompt recovery
    (tiqsi-claude-repl-recover-prompt)
    (message "✅ Prompt recovery completed")
    
    ;; Test session summary
    (tiqsi-claude-repl-show-session-summary)
    (message "✅ Session summary displayed")))

(defun test-claude-repl-ui-elements ()
  "Test UI elements and formatting."
  (message "\n=== Test 5: UI Elements ===")
  
  ;; Test colorization
  (let ((colored-text (tiqsi-claude-repl--colorize "Test" 'tiqsi-claude-repl-success)))
    (message "✅ Colorization works: %s" colored-text))
  
  ;; Test separator
  (let ((separator (tiqsi-claude-repl--make-separator 20)))
    (message "✅ Separator created: %s" separator))
  
  ;; Test status formatting
  (let ((status (tiqsi-claude-repl--format-status "Success" "Test completed")))
    (message "✅ Status formatted: %s" status))
  
  ;; Test timestamp
  (let ((timestamp (tiqsi-claude-repl--format-timestamp)))
    (message "✅ Timestamp: %s" timestamp)))

(defun test-claude-repl-simulate-response ()
  "Simulate a Claude response to test the process filter."
  (interactive)
  (message "\n=== Simulating Claude Response ===")
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    (let ((test-outputs
           '("{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-session-123\"}\n"
             "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"I understand you want to test. The number you mentioned was 22.\"}]}}\n"
             "{\"type\":\"result\",\"subtype\":\"success\",\"duration_ms\":500}\n")))
      ;; Clear JSON processing state
      (setq-local tiqsi-claude-repl--json-buffer "")
      (setq-local tiqsi-claude-repl--json-processing-enabled nil)
      
      ;; Simulate process filter receiving data
      (dolist (output test-outputs)
        (message "Processing: %s" (substring output 0 (min 50 (length output))))
        ;; Create a mock process
        (let ((mock-process (make-process
                            :name "mock-claude"
                            :buffer (current-buffer)
                            :command '("echo" "test"))))
          (tiqsi-claude-repl--process-filter mock-process output)
          (delete-process mock-process)))
      
      (message "✅ Response simulation completed")
      (message "Buffer contents:\n%s" (buffer-string)))))

(defun test-claude-repl-interactive ()
  "Interactive test - actually send a message to Claude."
  (interactive)
  (message "\n=== Interactive Test ===")
  (tiqsi-claude-repl-start)
  (sit-for 1) ; Wait for buffer to be ready
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    (goto-char (point-max))
    (insert "Hello Claude, this is a test. Remember the number 42.")
    (tiqsi-claude-repl-send-input)
    (message "✅ Test message sent. Watch the REPL buffer for response.")))

;; Provide the test module
(provide 'test-claude-repl)

;;; test-claude-repl.el ends here