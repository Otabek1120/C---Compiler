# C-- to MIPS Assembly Compiler

This project is part of the CSc 453: Spring 2024 course, focusing on developing a compiler for the C-- language that generates MIPS assembly code. 

## Project Overview

This program scans the C-- source code, does semantic checking using symbol tables, builds AST(abstract syntax tree), and produces MIPS code for the given source code. 



## Usage

1. **Clone the repository**:
    ```sh
    git clone https://github.com/Otabek1120/C---Compiler
    cd cminus-to-mips-compiler
    ```

2. **Build the project**:
    ```sh
    make compile
    ```

3. **Clean the project**:
    ```sh
    make clean
    ```

4. **Run the compiler with code generation**:
    ```sh
    ./compile --gen_code <source-file>
    ```

## Example

### Input Program

```c
int main() {
    println(34567);
}
