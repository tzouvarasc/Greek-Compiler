# Greek++ Compiler

This repository contains a reference implementation of a compiler for **greek++**, a
pedagogical programming language that mixes Greek keywords with a C-like program
structure. The project performs a full compilation pipeline, from lexical analysis
all the way to final assembly generation.

## Language Overview

* **Purpose:** Demonstrates the core stages of a compiler while keeping the source
  language small and approachable.
* **Data types:** Integers only. There are no arrays or strings in the language.
* **Reserved words:** All control-flow constructs and declarations use Greek
  keywords (e.g. `πρόγραμμα`, `δήλωση`, `εάν`, `επανάλαβε`, `όσο`, `για`,
  `συνάρτηση`, `διαδικασία`). Identifiers may combine Greek and Latin letters,
  digits, and the underscore.
* **Subprograms:** Functions return integers, procedures have no return value. Both
  support parameter passing by value (`CV`), by reference (`REF`), and returning
  values through `RET`.
* **Control flow:** `εάν`/`αλλιώς`, `επανάλαβε … μέχρι`, `όσο … όσο_τέλος`, and
  `για … για_τέλος` are supported, including nested and recursive calls.
* **I/O:** `διάβασε` reads an integer from standard input, `γράψε` prints the
  evaluation of an expression.

For a complete informal language description, see the assignment statement in the
repository root or review the sample programs under [`tests/`](tests/).

## Compiler Architecture

The compiler is implemented in a single Python module
[`compiler.py`](compiler.py) and is structured into the following
major components:

1. **Lexical analysis (`lexicalAnalyzer`)** – Tokenises the source code, handling
   nested comments, Unicode identifiers, keywords, operators, and numeric literals.
2. **Symbol table** – Tracks scopes, offsets, and entities (variables, parameters,
   temporaries, functions, procedures). It also enforces redeclaration rules and
   writes a human-readable summary to `symbol_table.sym`.
3. **Syntax and semantic analysis (`SyntaxAnalyzer`)** – Implements a recursive
   descent parser for the language grammar. The parser performs semantic checks
   such as undeclared identifiers, incorrect parameter modes, and invalid use of
   routines as values.
4. **Intermediate code generation (`IntermediateCodeGenerator`)** – Produces a
   quad-based three-address representation. The compiler emits these quads to a
   `<program>.int` file for inspection.
5. **Final code generation (`FinalCode`)** – Translates quads into RISC-V-like
   assembly using stack frames, static links, and system calls for I/O. The output
   is saved as `<program>.asm`.

## Usage

Run the compiler by passing a greek++ source file (`.gr`) to the driver script:

```bash
python compiler.py tests/gtest1.gr
```

Compilation produces three artifacts in the working directory:

| Output                | Description                                        |
|-----------------------|----------------------------------------------------|
| `symbol_table.sym`    | Final snapshot of all scopes and declared symbols. |
| `<input>.int`         | Intermediate quads generated for the program.      |
| `<input>.asm`         | RISC-V-style assembly produced from the quads.     |

Any lexical, syntactic, or semantic errors are reported with line numbers to help
locate the issue in the source program.

## Testing and Examples

The [`tests/`](tests/) folder contains sample greek++ programs that exercise the
compiler features, including function calls, loops, parameter passing by reference,
I/O, and recursive structures. You can compile any of them using the command above
(e.g. `python compiler.py tests/gtestfinal.gr`).

These examples are a good starting point for understanding the language syntax and
verifying modifications to the compiler.

## Requirements

* Python 3.9 or newer.
* No third-party packages are required; the compiler relies solely on the Python
  standard library.

## Contributing

Feel free to fork the repository and extend the greek++ language or the compiler
backend. Possible directions include:

* adding new data types (arrays, booleans, strings),
* improving error diagnostics, or
* targeting a different assembly language.

Pull requests and issue reports are welcome!
