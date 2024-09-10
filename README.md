# Tiqsi Emacs <a href="https://github.com/SerialDev/tiqsi-emacs/"> <img align="right" src="gifs/tiqsi.jpeg"></a> 
  

Fully self contained emacs IDE experience.  

Considers usage on windows(WSL) , linux, OSx, and,
cloud remote connection[for repl usage] a first class citizen.



### BNFC
```
Support for BNFC (Backus-Naur Form Compiler) grammars and enabling syntax highlighting.
```

### ASSEMBLY
```
  Mips and Arm ASM support

**_gas-mode_**: recognizes gas syntax (including embedded C preprocessor directives) and provides symbol highlighting, syntax highlighting, and indentation.

**Features**

* Symbol highlighting: recognizes symbols in label fields, argument fields, and some directives
* C Passthroughs: allows for C declarations to be embedded in assembler files
* Syntax highlighting: supports special forms of local labels (e.g. `55$`)
* Indentation: automatic indentation for assembler code
* To convert hexadecimal strings to decimal, use the `what-hexadecimal-value` function
* To convert between binary, hexadecimal, and octal representations, use the following functions:
	+ `bin-string-to-int`: converts binary string to integer
	+ `int-to-bin-string`: converts integer to binary string
	+ `int-to-hex-string`: converts integer to hexadecimal string
	+ `int-to-oct-string`: converts integer to octal string
	+ `bin-to-int`: converts binary string to integer
	+ `oct-to-int`: converts octal string to integer
	+ `hex-to-int`: converts hexadecimal string to integer
```

### C
```
### Features

With continuosly incremental support for:  
    -- Meson  
    -- Cmake --- [WIP] full parser  


* **Meson** and **CMake** support for project management
* **Compile** and **Run** functions for easy code execution
* **AddressSanitizer** integration for memory error detection
* **strace** and **clang-tidy** integrations for system call tracing and code analysis
* **coz** integration for causal profiling
* **GDB** integration for debugging
* **Smartparens** support for automatic parenthesis pairing

### Usage

* To compile a C file, use the `tiqsi-compile` function with the compile command as an argument.
* To run a compiled executable, use the `tiqsi-compile-run` function.
* To use AddressSanitizer, call the `tiqsi--tool-address-sanitizer--run` function with the file to compile as an argument.
* To use strace, call the `tiqsi--tool-strace--run` function with the command to trace as an argument.
* To use clang-tidy, call the `tiqsi--tool-clang-tidy` function with the files to analyze as arguments.
* To use coz, call the `tiqsi--tool-coz--run` function with the file to profile as an argument.

### Header and Implementation File Management

* To toggle between header and implementation files, use the `tiqsi-find-corresponding-file` function.
* To find the corresponding file in another window, use the `tiqsi-find-corresponding-file-other-window` function.

### GDB Integration

* To launch GDB in another buffer, use the `gdb-other-buffer` function.
```

### Rust
```
Rust Repl support through evxcr-mode
Extra racer tools

**Hydra Commands**

* `r`: Run
* `i`: Init
* `u`: Update
* `+r`: Release
* `x`: Run example
* `n`: New
* `c`: Repeat
* `b`: Build
* `f`: Current test
* `e`: Bench
* `l`: Clean
* `s`: Search
* `o`: Current file tests
* `d`: Doc
* `t`: Test
* `m`: Fmt
* `|`: Doc Tree
* `k`: Check
* `q`: Clippy

**LSP Configuration**

* `lsp-mode` is enabled for Rust files
* `lsp-rust-analyzer` is used as the LSP server
* `lsp-ui` is used for LSP UI
* `dap-mode` is used for debugging

**Keybindings**

* `C-c C-c` : Hydra Rust menu
* `C-t` : Racer UI tooltip
* `M-p` : Print Rust source code
* `M-i` : Insert struct point
* `C-c c` : Compile with no message
* `C-c C-r` : Compile and reset string
* `C-c n` : Go to next error

**Debugging**

* `M-x hover-debug-toggle` to toggle hover debug mode
* `M-x display-active-modes-and-functions` to display active modes and functions on hover


```
![Rust racer tweaks](gifs/racer-insert.gif)

### Python
```
AWS support and automatic remote repl execution

**Send Python Code to REPL**

* `send-py-line` (bound to `C-c C-a`): sends the current line of Python code to the REPL.
* `send-py-line-p` (bound to `C-c C-s`): sends the current line of Python code to the REPL, wrapped in a `print` statement.
* `send-py-region` (bound to `C-c C-r`): sends the selected region of Python code to the REPL.

**Compile and Run Python Code**

* `tiqsi-uv-compile` (bound to `C-c C-c`): compiles and runs the Python code in the current buffer.
* `custom-compile-go-to-error` (bound to `RET` and `g` in compilation mode): navigates to the file and line of the compilation error under the cursor, but keeps the focus on the compilation buffer.

**Extract Function Names**

* `extract-python-functions-to-clipboard` (interactive): extracts function names from the selected region and copies them to the clipboard in the desired import format.

**Find Functions Without Docstrings**

* `python-find-functions-without-docstrings-ag` (interactive): uses ag to search for Python functions without docstrings in the specified directory and displays the results in a compilation-mode buffer.

**Remote REPL**

* `sdev-use-venv` (interactive): sets up a remote REPL using a virtual environment.
* `sdev-use-remote` (interactive): sets up a remote REPL using a remote Python interpreter.

**Keybindings**

* `C-c C-s`: sends the current line of Python code to the REPL, wrapped in a `print` statement.
* `C-c C-a`: sends the current line of Python code to the REPL.
* `C-c C-0`: evaluates the last sexp.
* `C-c C-r`: sends the selected region of Python code to the REPL.
* `C-c C-_`: toggles camelcase underscores.
* `RET` and `g` in compilation mode: navigates to the file and line of the compilation error under the cursor, but keeps the focus on the compilation buffer.
* `C-c C-c`: compiles and runs the Python code in the current buffer.

```


### Java
```
[WIP]
```

### scala
```
[WIP]
```

### Clojure
```
CLJS Support  
Lein Hydras
### Hydra: hydra-clojure-usage

Activate the hydra menu by pressing `M-c`

**Hydra Shortcuts**:

* `C-<up>`: Move forward a parenthesis
* `M-<up>`: Wrap around a parenthesis
* `C-<down>`: Move backward a parenthesis
* `M-<down>`: Splice a sexp
* `M-<right>`: Forward slurp a parenthesis
* `C-<right>`: Backward barf a parenthesis
* `M-<left>`: Forward barf a parenthesis
* `C-<left>`: Backward slurp a parenthesis
* `C-s`: Evaluate the last sexp
* `<escape>`: Cancel
* `<f1>`: Start a new REPL
* `<f2>`: Connect to an existing REPL
* `<f3>`: Compile an uberjar
* `<f3>`: Run an uberjar


```


### Lisp 
```
Elisp, Common Lisp and, Scheme are widely supported
```

### GO
```
[WIP]
```

### Elm
```
WIP
```

### Nim
```
Added repl support
```

### Typescript
```
[WIP]
```
### C# 
```
C# & F# Supported
```

Heavy use of hydras 


<img align="right" width="100" height="100" src="gifs/tiqsi.jpeg">

