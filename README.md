# C-- to MIPS Assembly Compiler

This project is part of the CSc 453: Spring 2024 course, focusing on developing a compiler for the C-- language that generates MIPS assembly code. 

## Project Overview

This program scans the C-- source code, does semantic checking using symbol tables, builds AST(abstract syntax tree), and produces MIPS code for the given source code. 



## Usage

1. **Clone the repository**:
    ```sh
    git clone https://github.com/Otabek1120/C---Compiler
    cd C---Compiler
    ```

2. **Build the project**:
    ```sh
    make compile
    ```

3. **Clean the project**:
    ```sh
    make clean
    ```

4. **Run the compiler without code generation**:
    ```sh
    ./compile --chk_decl --print_ast <test1.txt >&testout.txt
    ```
5. **Run the compiler with code generation**:
   ```sh
   ./compile --chk_decl --gen_code  --print_ast <test1.txt >&testout.txt
   ```

## Example

### Input Program

```c
/* codegen: expressions containing at most one operator */

/* ---REMOVE TO COMPILE AS C CODE---
#include <stdio.h>
void println(int x) { printf("%d\n", x); }
---REMOVE TO COMPILE AS C CODE--- */

int main() {
    int x;

    x = 123 + 456;
    println(x);
    
    x = 123 - 456;
    println(x);
    
    x = 123 * 3;
    println(x);
    
    x = 123 / 3;
    println(x);

    x = (123);
    println(x);

    x = -123;
    println(x);

}
```

## Full C-- Language Specification

https://github.com/Otabek1120/C---Compiler/blob/main/Full%20C--%20language%20-%20CSC%20453%20SP24%20001.pdf


