# Claude REPL Permission System

## Overview

The Claude REPL now includes a comprehensive permission control system for tool execution.

## Permission Modes

1. **Ask Mode** (default): Claude will ask for permission before executing tools
2. **Auto-Accept Mode**: Automatically grants permission when Claude asks
3. **Bypass Mode**: Attempts to bypass permission requirements entirely

## Key Commands

### Mode Control
- `C-c C-m` - Open permission modes hydra menu
- `C-c M` - Cycle through permission modes quickly (ask → auto-accept → bypass → ask)
- `M-c` then `M` - Cycle permissions from main Claude hydra
- `M-c` then `m` - Open modes menu from main Claude hydra
- `M-x tiqsi-claude-repl-set-permission-mode` - Set specific mode
- `M-x tiqsi-claude-repl-show-modes` - Show current modes

### Quick Permission Grant
- `C-c p` - Grant permission immediately (when Claude asks)
- `C-c x` - Execute command with explicit permission
- `C-c g s` - Execute git status
- `C-c l s` - Execute ls -la

### Planning Mode
- `M-x tiqsi-claude-repl-toggle-planning-mode` - Toggle planning mode

## Visual Indicators

The prompt shows mode indicators:
- `[A]` - Auto-accept mode
- `[B]` - Bypass mode  
- `[P]` - Planning mode active

Example: `[A][P] λ` means auto-accept and planning modes are active.

## Usage Examples

### Quick Tool Execution
```
λ exec ! git status
⏳ Thinking...
I need you to grant permission...
```
Then press `C-c p` to grant permission.

### Auto-Accept Mode
```elisp
;; Enable auto-accept mode
(setq tiqsi-claude-repl-permission-mode 'auto-accept)
```

Now when Claude asks for permission, it will be automatically granted after 0.5 seconds.

### Using the Hydra Menu
Press `C-c C-m` to see:
```
Claude REPL Modes
─────────────────────────────────────────────
Permission:  ask  Auto-accept  Bypass
Planning:    planning toggle
Show:        status
─────────────────────────────────────────────
quit
```

## Troubleshooting

1. If Claude keeps asking for permission:
   - Switch to auto-accept mode: `C-c M` 
   - Or use direct grant: `C-c p`

2. If tool output isn't showing:
   - Check buffer for 🔧 TOOL EXECUTION markers
   - Ensure `--output-format stream-json` is being used

3. For debugging:
   - Enable thinking traces: `(setq tiqsi-claude-repl-show-thinking t)`
   - Check *Messages* buffer for JSON type messages