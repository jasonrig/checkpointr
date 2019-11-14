# checkpointr - A tool to checkpoint in R

Checkpointr is a tool to assist storing intermediate values during long-running calculations.
This package will either load a stored result or evaluate the code if any of the following conditions are satisfied:

* The expression to be evaluated has changed
* The `deps` object has changed
* The `force` parameter is set to `TRUE`

Each checkpointed result is stored in a file named by adding ".dat" to the end of `ckpt.id` (see the R documentation using `?checkpointr::checkpoint` after installation for more information).

## Installation
Installation depends on `devtools`, so once that's installed, the following command will fetch and install the checkpointr from git:
```
devtools::install_git("https://github.com/jasonrig/checkpointr.git")
```

## Usage
See the sample code below for a simple demo of checkpointr.
```
x <- seq(0, 100)
y <- checkpointr::checkpoint({
  m <- mean(x)
  s <- sd(x)
  42
}, "test_checkpoint1")
```
After running the code above, `y = 42`, `m = 50`, and `s=29.30017`. These values will be stored in `test_checkpoint1.dat`.
If this code were to be re-run, the expression inside the curly braces will be skipped.

The execution of the code can be further be triggered by changes to dependent variables:
```
x <- seq(0, 100)
y <- checkpointr::checkpoint({
  m <- mean(x)
  s <- sd(x)
  42
}, "test_checkpoint1", deps = x)
```
In the sample code above, if the value of x changes from what was used during the initial checkpoint, the code in the braces
will be re-evaluated and the checkpoint will be updated. Under the hood, the `deps` object is hashed and compared to the previously
stored hash. This means that any R object can be used to define a dependent variable.
here.
