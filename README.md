# Lambdo
## Description
Small lambda calculus interpreter that uses β-reduction and α-conversion.

## Grammar
```
program := expr

expr := 
| identifier
| lambda_symbol identifier -> expr
| (expr)
| expr expr

identifier :=
| [a-z A-Z _] [a-z A-Z 0-9 _]*

lambda_symbol := 
| lambda
| \
```

## Usage
* For REPL mode:

        dune exec lambdo
    or

        dune exec lambdo -- -r
* Using a file:

        dune exec lambdo -- [filename]