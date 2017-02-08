Lambda Interpreter
------------------

This is a compiler from a scheme-like language to the lambda calculus.

This is an interpreter for the lambda calculus.

This is an extractor for lambda calculus literals to haskell literals.

This is fun. You like it.


Using the Code
--------------

### Build the project
```
$> stack build
```

### Run the tests
```
$> stack test
```

### Run an example
```
$> stack ghci
ghci> runFile "resources/collatz.lc" (listExtractor intExtractor)
```
