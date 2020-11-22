# While+ Language Interpreter

[![GitHub CI](https://github.com/jkomyno/whileplus/workflows/CI/badge.svg)](https://github.com/jkomyno/whileplus/actions)
[![MPL-2.0 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/jkomyno/whileplus/blob/main/LICENSE)

> Interpreter for the denotational semantics of the While+ educational programming language.
> The While language is described in first chapter of the
> book [Semantics with Applications: A Formal Introduction](http://www.cs.ru.nl/~herman/onderwijs/semantics2019/wiley.pdf). 
> While+ is simply an extension of the original While language.

This is an academic project for the first part of the Software Verification course at the University of Padova, taught by Prof. Francesco Ranzato.

The interpreter relies on the Knaster-Tarski-Kleene fixpoint sequence for evaluating the semantics of **while** and **repeat'** loops. For further information, please refer to the project [report](report.pdf).

## Build and Run

The interpreter is written in Haskell, and it depends on the [Stack](https://docs.haskellstack.org/en/stable/README/) tool for administering the third-party dependencies and the build system.

#### Install Stack

On Unix systems:

```
curl -sSL https://get.haskellstack.org/ | sh
```

On Windows systems, please download the [64-bit installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

#### Build the whileplus interpreter

```
make build
```

#### Run the interpreter REPL

```
make run
```

## REPL Commands

#### `:load FILE`

```
Load a While+ source code from FILE to memory.
```

#### `:interpret [LINE]`

```
Interpret the given LINE of While+ code.
If no LINE is given, it interprets the latest
loaded FILE
```

#### `:ast [LINE]`

```
Show the AST of the given LINE of While+ code.
If no LINE is given, it shows the AST of the
latest loaded FILE.
```

#### `:check [LINE]`

```
Check the variable declarations and references
in the given LINE of While+ code.
If no LINE is given, it checks the variables
of the latest loaded FILE.
```

#### `:desugar [LINE]`

```
Show the desugared AST of the given LINE of
While+ code. If no LINE is given, it shows the
desugared AST of the latest loaded FILE.
```

#### `:reset`

```
Reset the interpreter state.
```

#### `:state`

```
Show the content of the state.
```

#### `:memory`

```
Toggle memory mode on or off. If memory mode
is on, the AST, the desugared AST and the final
state of computation is automatically shown.
By default, memory mode is off.
```

#### `:verbose`

```
Toggle verbose mode on or off. If verbose mode
is on, the AST, the desugared AST and the final
state of computation is automatically shown.
By default, verbose mode is off.
```

#### `:quit`

```
Quit the While+ language REPL.
```

#### `:help`

```
Show the list of REPL commands with their description.
```

## While Grammar

The syntactic notation we use is based on BNF.
First we list the various syntactic categories and give a meta-variable that will be used to range over constructs of
each category.
For the While and While+ languages, the meta-variables and categories are as follows:

- *n* will range over numerals, **Num**,
- *x* will range over variables, **Var**,
- *a* will range over arithmetic expressions, **Aexp**,
- *b* will range over boolean expressions, **Bexp**, and
- *S* will range over statements, **Stm**.

The grammar of the language is:

*a* ::= *n* | *x* | *a* + *a'* | *a* ⋆ *a'* | *a* − *a'*

*b* ::= *true* | *false* | *a* = *a'* | *a* ≤ *a'* | ¬*b* | *b'* ∧ *b'*

S ::= *x* := *a* | skip | *S* ; *S'* | if *b* then *S* else *S'*
| while *b* do *S*

## While+ Additional Native Constructs

We decided to add some new constructs to the While+ language.
These constructs are *native*, i.e. they introduce a new semantic definition.

The additional grammar of the language is:

S ::= *x*, *x'* := *a*, *a'* <span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span> **pair assignment**<br />
<span>&nbsp;&nbsp;&nbsp;&nbsp;</span>| repeat' *S* until *b* <span>&nbsp;&nbsp;&nbsp;</span> **native repeat loop**<br />

The *pair assignment* constructs completely evaluates the arithmetic expressions *a* and *a'* before performing the assignments `x := a` and `x' := a'`. That semantics is similar to Python's unpacking tuple semantic. This little addition enables the user to write statements such as:

```
x := 0;
y := 1;

x, y := y, x;

// The variables are updated as follows:
// x -> 1
// y -> 0
```

## While+ Syntactic Sugar

The While+ language also adds some additional syntactic sugar to the While language.
Here is a brief description of how the additional constructs are desugared into
the original While constructs.

#### Boolean Expressions

|       Name       | Syntactic Sugar |   Desugared    |
|:----------------:|:---------------:|:--------------:|
|    Disjunction   | `(b \|\| b')`   | `!(!b && !b')` |
|     Not Equal    | `(a != a')`     | `!(a = a')`    |
| Greater or Equal | `(a >= a')`     | `(a' <= a)`    |
|      Greater     | `(a > a')`      | `!(a <= a')`   |
|     Less than    | `(a < a')`      | `!(a' <= a)`   |

#### Statements

|            Name           | Syntactic Sugar         |   Desugared                               |
|:-------------------------:| ----------------------- | ----------------------------------------- |
| Addition assignment       | `x += a`                | `x := x + a`                              |
| Subtraction assignment    | `x -= a`                | `x := x - a`                              |
| Multiplication assignment | `x *= a`                | `x := x * a`                              |
| Repeat loop               | `repeat S until b`      | `S; while !b do S`                        |
| For loop                  | `for x := a to a' do S` | `x := a; while x < a' do (S; x := x + 1)` |


## Syntax Conventions

The logical symbols used in the book are encoded in the familiar C-style in While+ programs:

- The "And" operator <img src="https://render.githubusercontent.com/render/math?math=(\wedge)"> is encoded as `&&`
- The "Or" operator <img src="https://render.githubusercontent.com/render/math?math=(\vee)"> is encoded as `||`
- The "Greater or Equal" operator <img src="https://render.githubusercontent.com/render/math?math=(\geq)"> is encoded as `>=`
- The "Less or Equal" operator <img src="https://render.githubusercontent.com/render/math?math=(\leq)"> is encoded as `<=`

## License

Built with :coffee: by Alberto Schiabel.
This project is [BSD3](LICENSE) licensed.
