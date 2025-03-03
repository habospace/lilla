# lilla

Lilla is a programming language written in Haskell. 
It is a functional language that has a Pythonic syntax.
The language doesn't have loops, only recursion.

The [src/lillaTests/SmithNumbers.li](https://github.com/habospace/Lilla/blob/master/src/lillaTests/SmithNumbers.li) script is written in Lilla and is finding all the [smith numbers](https://en.wikipedia.org/wiki/Smith_number) below n. 

The [src/standards/standards.li](https://github.com/habospace/Lilla/blob/master/src/standards/standards.li) module contains 
some standard functions of the Lilla programming language such as:

**(1)** filter:

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

**(2)** map:

```
function map(fxy, xs):
    if eqv(length(xs), 0):
        return xs
    else:
        xsHead = head(xs)
        xsTail = tail(xs)
        return cons(fxy(xsHead), map(fxy, xsTail))
```

**(3)** fold:

```
function fold(fAcc, acc, xs):
    if eqv(length(xs), 0):
        return acc
    else:
        xsHead = head(xs)
        xsTail = tail(xs)
        return fAcc(xsHead, fold(fAcc, acc, xsTail))
```
