# Claude REPL Session Continuity Fix

## Problem
The Claude REPL was creating a new session for each message instead of continuing the existing session. This meant Claude couldn't remember previous context (like "remember the number 22").

## Root Cause
The issue was that we were using `claude -p` (print mode) for each request, which creates a new session each time. We needed to use Claude's `--resume` flag to continue an existing session.

## Solution

### 1. Capture Claude's Session ID
Added code to capture the session ID from Claude's JSON response:

```elisp
;; Added variable to store Claude's session ID
(defvar-local tiqsi-claude-repl--claude-session-id nil
  "The actual Claude session ID returned by the CLI.")

;; In process filter, capture the session ID:
((and (equal type "system") (equal (gethash "subtype" json-obj) "init"))
 (let ((claude-session-id (gethash "session_id" json-obj)))
   (when claude-session-id
     (setq-local tiqsi-claude-repl--claude-session-id claude-session-id))))
```

### 2. Use --resume Flag
Modified the command building to include `--resume` when we have a session:

```elisp
(let* ((cmd-args (list "-p"))  ; Use short form of --print
       ;; Add resume flag if we have a session ID
       (cmd-args (if tiqsi-claude-repl--claude-session-id
                     (append cmd-args (list "--resume" tiqsi-claude-repl--claude-session-id))
                   cmd-args))
       ;; Add output format for streaming
       (cmd-args (if tiqsi-claude-repl-show-thinking
                     (append cmd-args '("--output-format" "stream-json" "--verbose"))
                   cmd-args)))
```

### 3. Result
Now the command changes based on session state:
- First message: `claude -p --output-format stream-json --verbose`
- Subsequent messages: `claude -p --resume <session-id> --output-format stream-json --verbose`

This ensures Claude maintains conversation context across messages.

## Testing
You can verify session continuity with:

1. Start Claude REPL: `M-x tiqsi-claude-repl-start`
2. Send: "Remember the number 42"
3. Send: "What number did I ask you to remember?"
4. Claude should respond with "42"

You can also check the session info with `tiqsi-claude-repl-show-session-summary` to see both the internal session ID and Claude's session ID.

## Key Improvements

1. **Session Persistence**: Claude now remembers context between messages
2. **Clean JSON Parsing**: No raw JSON visible in output
3. **Beautiful Formatting**: Syntax highlighting, markdown rendering
4. **Debug Info**: Session ID visible in status messages
5. **Robust Error Handling**: Handles edge cases gracefully

The Claude REPL now provides a truly conversational experience with full session continuity!