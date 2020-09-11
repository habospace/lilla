# Lilla

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

**(1.)** The **src/lillaTests/SmithNumbers.hs** This is a Haskell code
that finds all smith numbers that are smaller than a given 'n' (see what 
smith numbers are here https://en.wikipedia.org/wiki/Smith_number). It is a 
ground truth reference to check if the lilla implementation of the same
task is executed correctly.

**(2.)** The **src/lillaTests/SmithNumbers.li** This is the 
**src/lillaTests/SmithNumbers.hs** code implemented in Lilla. It was made to 
test if the code is running correctly.

**(3.)** The **src/standards/standards.li** This module contains 
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

**(4.)** The **src/Data.hs** module contains definition of Lilla value and Lilla 
error types which are the Haskell representations of the Lilla programming language.
This is what the parser translates the Lilla sourcecode to.

**(5.)** The **src/Evaluator.hs** module contains the definition of how the Haskell 
representation of a Lilla program (which is a list of Lilla values **[LillaVal]**) 
is evaluated & executed.

**(6.)** The **src/Main.hs** module contains the **runLillaProgram** function (and all
of its component functions) that essentially takes a path to a Lilla source file
reads all the standard functions defined for the Lilla language into the memory
(see: **src/standards/standards.li**) and executes the Lilla program.

**(7.)** The **src/Parser.hs** module contains the parser that translates a text file
representing a lilla program into the in Haskell representation of the Lilla program
so basically it translates a String into an list of Lilla values **(String -> [LillaVal])**.
