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

## Dependencies
- `ghc` (make)
