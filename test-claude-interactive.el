;;; test-claude-interactive.el --- Interactive test for Claude REPL -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-claude-repl-real-interaction ()
  "Test real Claude interaction with proper JSON handling."
  (interactive)
  
  ;; Start fresh
  (when-let ((existing-buffer (get-buffer "*Claude REPL (tiqsi-emacs)*")))
    (kill-buffer existing-buffer))
  
  ;; Start REPL
  (tiqsi-claude-repl-start)
  
  ;; Give it a moment to initialize
  (sit-for 0.5)
  
  ;; Prepare test cases
  (let ((test-messages
         '("Hello Claude, please remember the number 42."
           "What number did I ask you to remember?"
           "Let's test code highlighting. Show me a simple Python function.")))
    
    ;; Send first message
    (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
      (goto-char (point-max))
      (insert (car test-messages))
      (tiqsi-claude-repl-send-input)
      
      ;; Wait for response
      (message "Sent first test message. Waiting for response...")
      (sit-for 5)
      
      ;; Check if we got a response
      (goto-char (point-min))
      (if (search-forward "42" nil t)
          (message "✅ Claude acknowledged the number 42!")
        (message "⚠️  Claude response doesn't contain '42'"))
      
      ;; Send second message to test session continuity
      (goto-char (point-max))
      (when (re-search-backward "^λ " nil t)
        (goto-char (match-end 0))
        (insert (cadr test-messages))
        (tiqsi-claude-repl-send-input)
        
        (message "Sent second test message. Testing session continuity...")
        (sit-for 5)
        
        ;; Check if Claude remembers
        (goto-char (point-min))
        (if (or (search-forward "42" nil t)
                (search-forward "forty-two" nil t))
            (message "✅ Claude remembers the number from previous message!")
          (message "❌ Claude doesn't seem to remember the previous message")))
      
      ;; Display buffer for visual inspection
      (display-buffer (current-buffer))
      (message "Test completed. Check the Claude REPL buffer for results."))))

(defun test-claude-repl-json-handling ()
  "Test the JSON handling specifically."
  (interactive)
  (with-current-buffer (get-buffer-create "*claude-json-test*")
    (erase-buffer)
    (tiqsi-claude-repl-mode)
    
    ;; Simulate the exact JSON output we saw
    (let ((test-json-sequence
           "{\"type\":\"system\",\"subtype\":\"init\",\"cwd\":\"/Users/test\",\"session_id\":\"b3ec24ec-bc42-45b8-b19e-e115bbb46d51\",\"tools\":[\"Task\",\"Bash\"],\"model\":\"claude-opus-4-20250514\",\"permissionMode\":\"default\",\"apiKeySource\":\"none\"}
I'll help you test. Yes, I can see your message where you mentioned \"remember 22\". How can I assist you with testing today?
{\"type\":\"assistant\",\"message\":{\"id\":\"msg_0137v92R1vVP1B73jS5iyawQ\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"text\",\"text\":\"I'll help you test. Yes, I can see your message where you mentioned \\\"remember 22\\\". How can I assist you with testing today?\"}],\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":3,\"cache_creation_input_tokens\":6614,\"cache_read_input_tokens\":13797,\"cache_creation\":{\"ephemeral_5m_input_tokens\":6614,\"ephemeral_1h_input_tokens\":0},\"output_tokens\":33,\"service_tier\":\"standard\"}},\"parent_tool_use_id\":null,\"session_id\":\"b3ec24ec-bc42-45b8-b19e-e115bbb46d51\"}
{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"duration_ms\":4542,\"duration_api_ms\":5346,\"num_turns\":1,\"result\":\"I'll help you test. Yes, I can see your message where you mentioned \\\"remember 22\\\". How can I assist you with testing today?\",\"session_id\":\"b3ec24ec-bc42-45b8-b19e-e115bbb46d51\",\"total_cost_usd\":0.14816459999999998,\"usage\":{\"input_tokens\":3,\"cache_creation_input_tokens\":6614,\"cache_read_input_tokens\":13797,\"output_tokens\":34,\"server_tool_use\":{\"web_search_requests\":0},\"service_tier\":\"standard\"}}"))
      
      ;; Initialize buffer state
      (setq-local tiqsi-claude-repl--json-buffer "")
      (setq-local tiqsi-claude-repl--json-processing-enabled nil)
      (setq-local tiqsi-claude-repl--output-start (point-marker))
      
      ;; Create a mock process
      (let ((mock-process (make-process
                          :name "mock-claude-json"
                          :buffer (current-buffer)
                          :command '("echo" "test"))))
        
        ;; Process the output
        (tiqsi-claude-repl--process-filter mock-process test-json-sequence)
        
        ;; Clean up
        (delete-process mock-process))
      
      ;; Check results
      (goto-char (point-min))
      (let ((has-clean-output (search-forward "I'll help you test" nil t))
            (has-json (search-forward "{\"type\":" nil t)))
        (if has-json
            (message "❌ Raw JSON is still visible in output")
          (message "✅ JSON properly parsed and hidden"))
        (if has-clean-output
            (message "✅ Claude's response text extracted correctly")
          (message "❌ Claude's response text not found")))
      
      (display-buffer (current-buffer))
      (message "JSON handling test completed. Check *claude-json-test* buffer."))))

;; Run the tests
(provide 'test-claude-interactive)