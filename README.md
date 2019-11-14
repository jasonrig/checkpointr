# checkpointr - A tool to checkpoint in R

Checkpointr is a tool to assist storing intermediate values during long-running calculations.
This package will either load a stored result or evaluate the code if any of the following conditions are satisfied:

* The expression to be evaluated has changed
* The `deps` object has changed
* The `force` parameter is set to `TRUE`

Each checkpointed result is stored in a file named by adding ".dat" to the end of `ckpt.id`.

## Installation
Installation depends on `devtools`, so once that's installed, the following command will fetch and install the checkpointr from git:
```
devtools::install_git("https://github.com/jasonrig/checkpointr.git")
```
