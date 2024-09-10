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
Rust Repl support through  
Extra racer tools
```
![Rust racer tweaks](gifs/racer-insert.gif)

### Python
```
AWS support and automatic remote repl execution
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

