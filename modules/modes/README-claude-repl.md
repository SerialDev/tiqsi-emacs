# Tiqsi Claude REPL

A beautifully crafted Claude REPL integration for Emacs with excellent user experience.

## Key Features

### 1. **Clean Output** 
- JSON responses are fully parsed - no raw JSON visible to users
- Only Claude's actual response text is displayed
- System messages optionally shown when thinking mode is enabled

### 2. **Session Continuity**
- Claude CLI maintains conversation context automatically
- Each session has a unique ID
- Claude remembers previous messages in the same session
- No manual history tracking needed

### 3. **Beautiful Formatting**
- Syntax highlighting for code blocks with language detection
- Markdown formatting (bold, italic, headers, lists)
- Inline code highlighting
- Colorized prompts and status messages
- Progress indicators with elapsed time

### 4. **Excellent User Experience**
- Clean, intuitive interface
- Real-time thinking indicators
- Session recovery capabilities
- Conversation history saving
- Smart context inclusion (project info, current file)

## Installation

1. Ensure Claude CLI is installed:
```bash
npm install -g @anthropic-ai/claude-code
```

2. The REPL is automatically loaded with `init-lite.el`

## Usage

### Starting the REPL

```elisp
M-x tiqsi-claude-repl-start
```

Or use the hydra menu: `M-c` then `s`

### Key Bindings

#### In REPL Buffer:
- `RET` or `C-c C-c` - Send message to Claude
- `C-g` - Cancel current request
- `C-c C-k` - Clear buffer
- `C-c C-r` - Recover session
- `C-c C-q` - Quit REPL

#### Global Hydra Menu (`M-c`):
- `s` - Start Claude REPL
- `k` - Kill session
- `c` - Toggle window
- `r` - Send region
- `f` - Send function
- `b` - Send buffer
- `e` - Fix error
- `o` - Optimize code
- `x` - Explain code
- `t` - Generate tests

### Example Interaction

```
λ Hello Claude, please remember the number 42
⏳ Thinking... [0.5s]

I'll remember the number 42 for you. Is there anything specific you'd like me to do with this number?

✅ Completed in 1.2s

λ What number did I ask you to remember?
⏳ Thinking... [0.3s]

You asked me to remember the number 42.

✅ Completed in 0.8s
```

## Features in Detail

### JSON Processing
The REPL intelligently detects and parses Claude's JSON responses:
- System initialization messages
- Assistant responses with proper text extraction  
- Streaming content updates
- Result metadata (hidden from view)

### Code Highlighting
Code blocks are automatically detected and highlighted:
- 60+ languages supported
- Line numbers for code blocks
- Proper indentation preserved
- Inline code styling

### Session Management
- Unique session IDs for each REPL instance
- Message counting
- Session duration tracking
- Idle time monitoring
- Conversation history saving

### Error Handling
- Graceful handling of connection issues
- Process cancellation support
- Session recovery after disconnection
- Prompt recovery if missing

## Configuration

Key configuration variables:

```elisp
;; Show Claude's thinking process
(setq tiqsi-claude-repl-show-thinking t)

;; Enable code syntax highlighting  
(setq tiqsi-claude-repl-highlight-code t)

;; Save conversation history
(setq tiqsi-claude-repl-save-history t)

;; Include smart context (project info)
(setq tiqsi-claude-repl-use-smart-context t)

;; Animate thinking indicator
(setq tiqsi-claude-repl-animate-thinking t)
```

## Troubleshooting

### Claude CLI not found
Install with: `npm install -g @anthropic-ai/claude-code`

### Raw JSON appearing
- Check `tiqsi-claude-repl-clean-output` is set to `t`
- Ensure you're using the latest version

### Session not continuing
- Claude CLI manages sessions internally
- Check the session ID is consistent
- Use `C-c C-r` to recover session if needed

## Technical Details

The REPL uses:
- Process filters for streaming responses
- JSON parsing for structured data
- Text properties for formatting
- Overlays for visual effects
- Mode-specific keymaps
- Hydra for convenient access

## Performance

- Minimal overhead on Emacs startup
- Lazy loading of dependencies
- Efficient JSON parsing
- Smart buffer management
- Optimized text rendering

---

The Tiqsi Claude REPL provides a premium AI assistant experience directly in Emacs, 
with all the features you need for productive coding assistance.