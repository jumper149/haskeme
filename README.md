# haskeme

This compiler translates Scheme source code written with I-Expressions (indented expressions) into S-Expressions (symbolic expressions).

## Usage
Haskeme can read and write files:

    haskeme --input "program.hss" --output "program.ss"

Haskeme can also use `StdIn` and `StdOut`:

    cat "program.hss" | haskeme > "program.ss"

### Options
- `-i` or `--input` to specify input file
- `-o` or `--output` to specify output file

## I-Expressions
I-Expressions are indented expressions and are translated by few simple rules:

- a new line translates to a new S-Expression
- same indentation as the expression above translates to a new S-Expression following the prior one
- more indentation as the expression above translates to a new S-Expression as an argument to the prior one
- less indentation as the expression above, but still more indentation as another expression above translates to a new S-Expression, while the prior and further indented block gets wrapped in extra parentheses (look at the `let`-example)
- S-Expression can be mixed in while staying in a single line (look at the example for mixed expressions)


### Example
- I-Expressions:

    ```
    define
      f
        lambda
          x
          let
              y
                * x x
        + y 1
    ```

- S-Expressions:

    ```
    (define
      (f
        (lambda (x)
          (let ((y (* x x)))
            (+ y 1)))))
    ```

- mixed I- and S-Expressions:

    ```
    define
      f
        lambda (x)
          let
              y (* x x)
            + y 1
    ```
