Lambda Interpreter
------------------

This is a compiler from a scheme-like language to the lambda calculus.

This is an interpreter for the lambda calculus.

This is an extractor for lambda calculus literals to haskell literals.

This is fun. You like it.

You can read all about it [here](http://tobin.yehle.io/articles/lambda-compiler).


Using the Code
--------------

You will need [stack](https://docs.haskellstack.org/en/stable/README).

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
ghci> runFile "resources/collatz.lc" (extractList intExtractor)
```
