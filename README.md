# lilla

Lilla is a programming language written in Haskell. 
It is a functional language that has a Pythonic syntax.
The language doesn't have loops, only recursion.
The repository has the following structure:

```
|src/
    ├ lillaTests/
    |   ├ SmithNumbers.hs (1.)
    │   ⌊ SmithNumbers.li (2.)
    ├ standards/
    │   ⌊ standards.li    (3.)
    ├ Data.hs             (4.)
    ├ Evaluator.hs        (5.)
    ├ Main.hs             (6.)
    └ Parser.hs           (7.)
```

**(1.)** The [src/lillaTests/SmithNumbers.hs](https://github.com/habospace/Lilla/blob/master/src/lillaTests/SmithNumbers.hs) This is a Haskell code
that finds all smith numbers that are smaller than a given 'n' (see what 
smith numbers are here https://en.wikipedia.org/wiki/Smith_number). It is a 
ground truth reference to check if the lilla implementation of the same
task is executed correctly by the Evaluator.

**(2.)** The [src/lillaTests/SmithNumbers.li](https://github.com/habospace/Lilla/blob/master/src/lillaTests/SmithNumbers.li) This is the 
[src/lillaTests/SmithNumbers.hs](https://github.com/habospace/Lilla/blob/master/src/lillaTests/SmithNumbers.hs) code implemented in Lilla. It was made to 
test if the Lilla implementation of a program that finds all smith numbers below a
given 'n' is executed correctly by the Evaluator. 

**(3.)** The [src/standards/standards.li](https://github.com/habospace/Lilla/blob/master/src/standards/standards.li) This module contains 
some standard functions of the Lilla programming language such as:

**(3.1)** filter:

```
function filter(f, xs):
    if eqv(length(xs), 0):
        return xs
    else:
        xsHead = head(xs)
        xsTail = tail(xs)
        if f(xsHead):
            return cons(xsHead, filter(f, xsTail))
        else:
            return filter(f, xsTail)
```

**(3.2)** map:

```
function map(fxy, xs):
    if eqv(length(xs), 0):
        return xs
    else:
        xsHead = head(xs)
        xsTail = tail(xs)
        return cons(fxy(xsHead), map(fxy, xsTail))
```

**(3.3)** fold:

```
function fold(fAcc, acc, xs):
    if eqv(length(xs), 0):
        return acc
    else:
        xsHead = head(xs)
        xsTail = tail(xs)
        return fAcc(xsHead, fold(fAcc, acc, xsTail))
```

**(4.)** The [src/Data.hs](https://github.com/habospace/Lilla/blob/master/src/Data.hs) module contains the definitions of Lilla values and Lilla 
error types which are the building blocks of the Haskell representation of a 
Lilla program. The lilla error types are used during the evaluation and the 
evaluation of the lilla program (ParseLillaError & RuntimeLillaError). 

**(5.)** The [src/Evaluator.hs](https://github.com/habospace/Lilla/blob/master/src/Evaluator.hs) module contains the sourcecode of the Lilla program evaluation. For representing the iterative/step by step execution of the Lilla 
program the Evaluator module is using the State Monad Transformer extensively.

**(6.)** The [src/Main.hs](https://github.com/habospace/Lilla/blob/master/src/Main.hs) module contains the **runLillaProgram** function (and all
of its component functions) that essentially takes a path to a Lilla source file
reads all the standard functions defined for the Lilla language into the memory
(see: [src/standards/standards.li](https://github.com/habospace/Lilla/tree/master/src/standards)) and executes the Lilla program.

**(7.)** The [src/Parser.hs](https://github.com/habospace/Lilla/blob/master/src/Parser.hs) module contains the parser that translates a text file
representing a lilla program into the Haskell representation of the Lilla program
so basically it translates a String into a list of Lilla values **(String -> [LillaVal])**. 
The parser is implemented based on the paper "Monadic Parser Combinators" by Graham Hutton 
(see: https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf). 
