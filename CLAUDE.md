# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Tiqsi Emacs is a comprehensive, self-contained Emacs configuration framework designed for cross-platform development (Windows/WSL, Linux, macOS). It provides a complete IDE experience with extensive language support, remote development capabilities, and performance optimizations.

**Note: Always use `init-lite.el` as the entry point for this configuration.** This provides a faster, more focused development experience with the essential features.

## Common Development Commands

### Launching Tiqsi Emacs

```bash
# Primary method (always use this)
XLIB_SKIP_ARGB_VISUALS=1 emacs -q -l ~/path/to/tiqsi-emacs/init-lite.el &

# Docker-based development
docker-compose up
```

### Language-Specific Commands

#### Rust Development (via Hydra: `C-c C-c`)
- `r` - cargo run
- `b` - cargo build
- `+b` - cargo build --release
- `t` - cargo test
- `k` - cargo check
- `m` - cargo fmt
- `q` - cargo clippy

#### Python Development
- `C-c C-c` - Compile with UV/venv support
- `C-c C-a` - Send line to REPL
- `C-c C-s` - Send line wrapped in print()
- `C-c C-r` - Send region to REPL

#### C/C++ Development
- `M-m` - Run make without asking
- Quick compile/run: `clang main.c && ./a.out` (via `tiqsi--tool-c-quick-run`)
- Valgrind: `valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v`

#### Clojure Development (via Hydra: `M-c`)
- `<f1>` - lein start repl
- `<f2>` - lein connect repl
- `<f3>` - lein compile uberjar / run uberjar

## Architecture

### Initialization Flow

1. **Entry Point**:
   - `init-lite.el` - **Always use this** - Lightweight configuration with essential features
   - `init.el` - Full configuration (not recommended for regular use)

2. **Loading Sequence**:
   ```elisp
   init-lite.el → core/ → selective modules/modes/ → selective modules/programming/ → modules/private/
   ```

### Directory Structure

```
tiqsi-emacs/
├── core/              # Core functionality
│   ├── core.el       # Main initialization
│   ├── core-setup.el # Package management (straight.el)
│   ├── core-ui.el    # UI configuration
│   └── ...           # Other core modules
├── modules/
│   ├── modes/        # Emacs mode configurations (helm, magit, org, etc.)
│   ├── programming/  # Language-specific support
│   ├── private/      # User customizations
│   └── misc/         # Third-party contributions
└── init*.el          # Entry points
```

### Key Functions and Macros

- **`load-expand`**: Loads files relative to tiqsi-core directory
- **`try-require`**: Safely requires packages with error handling
- **`with-system`**: OS-specific conditional execution
- **`when-available`**: Conditional execution based on feature availability
- **`straight-require`**: Package installation via straight.el
- **`straight-require-lazy`**: Deferred package loading

### Package Management

Uses straight.el as the primary package manager with fallback to MELPA/GNU ELPA:

```elisp
(straight-require 'package-name)      ; Install immediately
(straight-require-lazy 'package-name) ; Defer installation
```

## Programming Language Support

### Fully Supported Languages
- **Python**: LSP, remote REPL, venv support, black formatter
- **Rust**: rust-analyzer, cargo integration, clippy
- **C/C++**: clangd, RTags, compile commands, debugging tools
- **Clojure**: CIDER, Figwheel, lein integration
- **Lisp family**: Elisp, Common Lisp, Scheme with SLIME/Geiser

### Partial Support
Go, JavaScript/TypeScript, Java, Scala, Nim, Elm, C#/F#, Assembly (MIPS/ARM)

## Key Features

1. **Remote Development**: AWS REPL execution for Python via `remote-python.sh`
2. **Performance**: Lazy loading, garbage collection optimization
3. **UI Enhancements**: Spaceline, all-the-icons, multiple themes
4. **Completion**: Company-mode, Helm/Ido, LSP backends
5. **Version Control**: Magit with extensive customizations

## Customization Points

1. **Private Directory**: Place personal customizations in `modules/private/`
2. **Custom Modules**: Add new files in appropriate directories and update parent module loaders
3. **OS-Specific Code**: Use `with-system` macro for conditional execution
4. **Performance Tuning**: Modify `core-performance.el` for startup optimization

## Development Conventions

1. **File Headers**: All files include GPL v3 license header
2. **Module Structure**: Each module should be self-contained with clear dependencies
3. **Loading**: Use `try-require` for optional dependencies
4. **Naming**: Functions prefixed with `tiqsi-` for namespace consistency
5. **Comments**: Use `;;` for inline comments, maintain consistent formatting

## Tips for Development

1. **Testing Changes**: Use init-lite.el for faster iteration when developing
2. **Debugging**: Enable `core-debug.el` features for troubleshooting
3. **Performance**: Check startup time with `emacs-startup-time`
4. **Package Issues**: Clear straight cache at `~/.emacs.d/straight/` if needed
5. **Remote Work**: Configure `sdev-use-venv` and `sdev-use-remote` for remote development

## Performance Troubleshooting

If experiencing performance issues, use `M-x explain-pause-top` to identify slow operations.

### Performance Test Results (Latest - 2025-08-10)

Tested with Django repository (6937 files, 9201-line Python files):
- **Startup time**: 0.002 seconds (optimized)
- **Opening large files**: 3.4 seconds for 9201 lines
- **Search performance**: 0.033 seconds for 100 searches
- **Indentation**: 0.020 seconds for 200 lines
- **Completion**: 0.076 seconds
- **Python project navigation**: Sub-second (was 8+ seconds before optimization)
- **LSP operations**: Fast response (was 3+ seconds for lsp--get-body-length)

### Critical Performance Fixes Applied (2025-08-10):

#### 1. **LSP Performance Optimizations (Balanced for Functionality)**
Fixed major `lsp--get-body-length` slowdowns while maintaining full LSP functionality:

```elisp
;; Core LSP settings - balanced for excellent functionality and performance
(with-eval-after-load 'lsp-mode
  ;; Network/communication optimizations
  (setq lsp-idle-delay 0.3) ; Quick response
  (setq lsp-response-timeout 30) ; Reasonable timeout
  (setq lsp-use-plists t) ; Faster JSON parsing
  
  ;; File watching - disable major performance killer
  (setq lsp-enable-file-watchers nil)
  
  ;; Keep essential features enabled
  (setq lsp-completion-provider :capf) ; LSP completion
  (setq lsp-diagnostics-provider :flycheck) ; Error checking
  (setq lsp-enable-snippet t) ; Code templates
  (setq lsp-signature-auto-activate t) ; Function signatures
  (setq lsp-eldoc-enable-hover t) ; Documentation on hover
  
  ;; UI features maintained
  (setq lsp-modeline-diagnostics-enable t) ; Error indicators
  (setq lsp-headerline-breadcrumb-enable t) ; Navigation
  
  ;; Disable only non-essential slow features
  (setq lsp-lens-enable nil) ; Often slow, not essential
  (setq lsp-enable-on-type-formatting nil) ; Can cause typing lag
```

#### 2. **File System Performance Fixes**
Resolved major slowdowns when entering Python projects:

```elisp
;; File system monitoring optimizations
(setq auto-revert-use-notify nil) ; Don't use FS notifications in large projects
(setq auto-revert-check-vc-info nil) ; Don't check VC info
(setq create-lockfiles nil) ; Don't create lock files
```

#### 3. **Python Mode Optimizations**
Prevent blocking during file operations:

```elisp
;; Python specific optimizations
(setq python-indent-guess-indent-offset nil) ; Don't guess indentation
(setq python-shell-completion-native-enable nil) ; Disable native completion
(setq python-shell-prompt-detect-enabled nil) ; Don't detect prompts
```

#### 4. **LSP-UI Optimizations (Functionality Preserved)**
```elisp
(with-eval-after-load 'lsp-ui
  ;; Keep essential features with optimized timing
  (setq lsp-ui-sideline-delay 0.3) ; Quick sideline updates
  (setq lsp-ui-doc-delay 0.3) ; Quick documentation
  (setq lsp-ui-doc-max-width 60) ; Limit size for performance
  (setq lsp-ui-peek-enable t) ; Keep excellent peek functionality
```

### Previous Optimizations (Still Applied):

1. **Garbage Collection**:
   ```elisp
   (setq gc-cons-threshold 20000000)
   (setq undo-limit 20000000)
   (setq undo-strong-limit 40000000)
   ```

2. **Auto-revert optimization**:
   ```elisp
   (setq auto-revert-interval 10) ; Increased interval
   (setq auto-revert-check-vc-info nil)
   (setq auto-revert-use-notify nil) ; Disabled for large projects
   (setq global-auto-revert-non-file-buffers nil)
   ```

3. **Font-lock optimization**:
   ```elisp
   (setq jit-lock-defer-time 0.25)
   (setq jit-lock-stealth-time 5)
   (setq jit-lock-chunk-size 1000)
   (setq font-lock-maximum-decoration '((t . 2)))
   (setq inhibit-compacting-font-caches t)
   ```

4. **Company-mode optimization**:
   ```elisp
   (setq company-idle-delay 0.5)
   (setq company-minimum-prefix-length 3)
   ```

5. **Large file handling**:
   - Files > 1MB: Disable bidi-display-reordering and line-move-visual
   - Files > 5MB: Disable font-lock-mode entirely

6. **General optimizations**:
   ```elisp
   (setq fast-but-imprecise-scrolling t)
   (setq highlight-nonselected-windows nil)
   (setq-default line-move-visual nil)
   (setq auto-window-vscroll nil)
   ```

7. **Beacon optimization** (Ultra-fast comet tail implementation):
   - Custom ultra-beacon with comet tail effect
   - Golden yellow color (#d4af37) for comfortable viewing
   - 8-step fade for smooth animation
   - Purely visual overlays that don't affect buffer content

### Fixed Issues:
- ✅ **LSP Performance**: Fixed 3+ second `lsp--get-body-length` delays while keeping full LSP functionality
- ✅ **Python Project Navigation**: Fixed 8+ second slowdowns when entering Python directories  
- ✅ **File System Blocking**: Eliminated file system notification slowdowns in large projects
- ✅ **Window Configuration**: Reduced LSP initialization delays on window changes
- ✅ **Semantic Mode**: Disabled (was causing hours of delay)
- ✅ **Python Configuration**: Fixed bug using 't' as variable name
- ✅ **Beacon Performance**: Replaced slow original beacon with ultra-fast implementation
- ✅ **Ultra-beacon Color Parsing**: Fixed "White" color parsing that stopped init execution

### LSP Feature Matrix (What's Enabled vs Disabled):

#### ✅ **Enabled (Full Functionality)**:
- Code completion via CAPF
- Real-time diagnostics and error checking
- Function signatures and documentation
- Hover information and eldoc
- Go-to-definition and find references
- Code actions and quick fixes
- Syntax highlighting with semantic tokens
- Breadcrumb navigation
- Symbol highlighting
- Code snippets
- Imenu integration
- Format on save
- Workspace management

#### ❌ **Disabled (Performance Only)**:
- File watchers (major performance impact)
- LSP lens (often slow, not essential)
- On-type formatting (can cause typing lag)
- Server tracing and I/O logging

### Performance Troubleshooting Commands:
- `M-x explain-pause-top` - Identify slow operations
- `M-x profiler-start` - Start Emacs profiler
- `M-x benchmark-run` - Benchmark specific functions
- `M-x ultra-beacon-benchmark` - Test beacon performance

## Global Keybindings Reference

This section documents all keybindings defined across the Tiqsi Emacs configuration. Keybindings are organized by category and source file.

### Navigation & Movement

#### Window Navigation (modes-avy.el, core-windows.el, core-navigation.el)
- `M-w`, `C-x C-w` - Jump between windows (sdev/jump-window)
- `C-t` - Jump to *vterm* buffer
- `C-c jj` - Avy goto word or subword
- `C-c jw` - Ace window selection
- `C-c js` - Ace swap window
- `C-M-<left>` - Move to window on the left
- `C-M-<right>` - Move to window on the right
- `C-M-<up>` - Move to window above
- `C-M-<down>` - Move to window below

#### Window Management (core-windows.el)
- `C-c .` - Compare entity across windows (function/line comparison)
- `C-c g` - Toggle window split orientation
- `C-c l` - Move buffer left
- `C-c r` - Move buffer right
- `C-c u` - Move buffer up
- `C-c d` - Move buffer down
- `C-c w` - Rotate windows

#### Text Navigation (core-navigation.el)
- `C-<right>` - Forward word
- `C-<left>` - Backward word
- `C-<up>` - Previous blank line
- `C-<down>` - Next blank line
- `M-<up>` - Previous blank line
- `M-<down>` - Next blank line
- `M-<right>` - Forward word
- `M-<left>` - Backward word
- `<home>` - Beginning of line
- `<end>` - End of line
- `<pgup>` - Forward page
- `<pgdown>` - Backward page
- `C-a` - Smart move to beginning of line
- `C-S-a` - Move to end of line
- `%` - Match parenthesis

#### Buffer Navigation (core-navigation.el)
- `C-c 3` - Switch to buffer
- `C-c 2` - Next buffer
- `C-c 1` - Previous buffer
- `C-c >` - End of buffer
- `C-c <` - Beginning of buffer

### Editing

#### Basic Editing (core-editing.el)
- `M-a` - Yank (paste)
- `M-z` - Kill region (cut)
- `<backtab>` - Un-indent by removing 4 spaces
- `C-S-<right>` - Shift right
- `C-S-<left>` - Shift left
- `C-c )` - Match parenthesis
- `C-=` - Expand region

#### Code Editing (various files)
- `C-c C-s` - Eval last sexp (Emacs Lisp mode)
- `<tab>` - Company indent or complete

### Search & Replace

#### Search (modes-helm.el, modes-ido.el)
- `M-x`, `C-x C-x` - Command execution (Helm or IDO based on config)
- `C-x b` - Buffer switching (Helm-mini or IDO)
- `C-x f` - Find file (IDO)
- `M-f` - Find file
- `M-F` - Find file in other window
- `M-i` - Helm swoop back to last point

### Mode-Specific Keybindings

#### Python Mode (modes-company.el, modes-flycheck.el, programming-python-lite.el)
- `C-.` - Conditional xref/LSP find definition
- `C->` - Find definition in side buffer
- `C-\`` - LSP UI peek definitions
- `C-,` - Go back (xref)
- `C-~` - LSP UI peek references
- `C-c n` - Flycheck next error
- `C-c b` - Flycheck previous error
- `C-c ee` - Flycheck list errors
- `C-c C-c` - Compile with UV/venv support
- `C-c C-a` - Send line to REPL
- `C-c C-s` - Send line wrapped in print()
- `C-c C-r` - Send region to REPL

#### Emacs Lisp Mode (modes-company.el)
- `C-.` - Xref find definitions
- `C-,` - Xref pop marker stack

#### Org Mode (modes-org.el)
- `C-l` - Store link
- `C-a` - Org agenda
- `C-x C-e` - Cider eval last sexp
- `C-c C-d` - Cider doc
- `C-c C-i` - Insert reveal.js image

#### Markdown Mode (modes-md.el)
- `C-c gg` - Writegood grade level
- `C-c ge` - Writegood reading ease

#### ERC Mode (modes-erc.el)
- `C-c e` - Insert emoticon
- `M-.` - ERC goto definition

#### Shell/Comint Mode (modes-helm.el)
- `C-c C-l` - Helm comint input ring

### Special Functions

#### Compilation (modes-compilation.el)
- `M-m` - Make without asking

#### Tags Navigation (modes-ctags.el)
- `M-.` - Find tag (conflicting definitions!)
- `M-*` - Helm etags history
- `M-,` - Helm etags history go back
- `M-/` - Helm etags history go forward

#### Version Control (core-navigation.el)
- `<f12>` - LSP goto type definition
- `<f10>` - Xref go back

#### UI Controls (modes-minimap.el)
- `C-#` - Toggle minimap mode

#### Hydras (modes-helm.el)
- `C-c h` - Helm menu hydra

#### Evil Mode (modes-evil.el)
- `<escape>` - Vim movement mode hydra

#### AI Assistance (modes-claude.el) - Hydra-based
- `M-c` - Open Claude hydra menu (primary)
- `C-c a` - Open Claude hydra menu (alternative)
- `<f8>` - Open Claude hydra menu (function key)
- `C-c C-a` - Quick explain error (language-specific modes)

**Claude Hydra Commands:**
- Sessions: `c` (start), `k` (kill), `l` (list), `m` (transient menu)
- Send Content: `r` (region), `f` (function), `d` (defun), `p` (paragraph), `b` (buffer), `s` (command)
- AI Analysis: `e` (explain error), `o` (optimize code), `t` (write tests), `a` (analyze code)
- Utility: `h` (help), `q` (quit), `ESC` (cancel)

#### Other
- `C-,` - Parinfer toggle mode (modes-parinfer.el)
- `M-<f9>` - Neotree toggle (modes-neotree.el)

### Keybinding Conflicts

The following keybindings have conflicts that need resolution:

1. **`M-.`** - Bound in both modes-ctags.el and modes-erc.el
2. **`C-x C-x`** - Bound in both modes-helm.el and modes-ido.el
3. **`C-a`** - Bound in both core-navigation.el and modes-org.el

### Undefined Function References

The following keybindings reference functions that are not defined:
- `my-find-tag` in modes-ctags.el
- `sk/smarter-move-beginning-of-line` in modes-evil.el
- `open-line-below` and `open-line-above` in modes-evil.el
- `sdev/set-windows` in modes-avy.el

## Reorganization Plan

Based on the comprehensive analysis of the modules/modes directory, the following reorganization is recommended:

### Functions to Move

#### From modes-avy.el to core-windows.el:
- `count-unique-visible-buffers`
- `sdev/other-window`
- `sdev/set-or-jump-windows`
- `sdev/jump-window`
- `sdev/jump-to-vterm`

#### From modes-company.el to programming-python-lite.el:
- `insert-colored-print`
- `install-pyright-in-uv`
- `activate-lsp-bridge-with-uv`
- Python-specific keybindings

#### From modes-company.el to core-editing.el:
- `indent-or-expand`
- `indent-and-complete`
- `tiqsi/indent-or-complete`

#### From modes-shell.el to core-windows.el:
- `frame--set-input-focus`
- `make-tip-frame`
- `close-tip-frame`
- `tooltip-command`

#### From modes-neotree.el to programming-java.el (new file):
- `tiqsi-java-hook`

#### From modes-md.el to core-functions.el:
- `count-occurences`
- `recursive-count`

#### From modes-org.el to modes-md.el:
- `markdown-display-inline-images`

### Issues to Fix

#### Duplicate Function Definitions:
1. `helm-posframe-cleanup` in modes-helm.el (remove duplicate)
2. `conditional-xref-lsp-find-definition-side-buffer` in modes-company.el (remove duplicate)

#### Undefined Function References:
1. Replace `my-find-tag` with `sdev/find-tag` in modes-ctags.el
2. Define `sk/smarter-move-beginning-of-line` or use the existing one from core-navigation.el
3. Define `open-line-below` and `open-line-above` in core-editing.el
4. Define `sdev/set-windows` in core-windows.el

#### Keybinding Conflicts to Resolve:
1. `M-.` conflict between modes-ctags.el and modes-erc.el
   - Solution: Use mode-specific maps instead of global bindings
2. `C-x C-x` conflict between modes-helm.el and modes-ido.el
   - Solution: Only one should be active at a time based on which is loaded
3. `C-a` conflict between core-navigation.el and modes-org.el
   - Solution: org-mode binding should use org-mode-map

#### Hardcoded Values to Extract:
1. Extract IP address from `jump-to-hetzner` in modes-dired.el to a configuration variable
2. Extract IP address from `connect-to-tramp-ssh` in modes-tramp.el to a configuration variable

### New Files to Create:
1. `modules/programming/programming-java.el` - For Java-specific functions
2. `modules/misc/misc-frame.el` - Alternative location for frame management functions

### Configuration Improvements:
1. Add a configuration section for remote servers (IPs, usernames)
2. Create a keybinding conflict resolution system
3. Add mode detection to conditionally load helm vs ido
4. Document which packages are mutually exclusive