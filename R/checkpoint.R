#' Evaluates an expression or loads a cached value
#'
#' Both the change to the environment and the expression's return value is stored.
#' The expression is re-evaluated if the expression or value of `deps` changes.
#'
#' @param expr any valid R expression
#' @param ckpt.id an identifier used for the checkpointed data
#' @param deps any R object that is monitored for changed and is used to trigger re-evaluation
#' @param force force re-evaluation of the expression regardless of the state of `deps` or `expr`
#' @param envir the environment in which the expression is evaluated
#' @param enclos the environment in which the expression is substituted
#' @return the result of the expression
#' @export
checkpoint <-
  function(expr,
           ckpt.id,
           deps = NA,
           force = FALSE,
           envir = parent.frame(),
           enclos = environment()) {
    # Prepare the checkpoint file name
    ckpt_file <- paste0(ckpt.id, ".dat")

    # Set up an environment in which to store the result of the evaluated expression
    tmp_env <- new.env(parent = envir)

    # Substitute the expression
    expr <- substitute(expr, enclos)

    # Calculate the hash used to determine whether a re-evaluation is necessary
    deps_hash <-
      digest::digest(list(deps = deps, expr = deparse(expr)), "sha256")

    if (!force && file.exists(ckpt_file)) {
      # Load the checkpoint if the checkpoint file exists
      message(paste0("Loading checkpoint ", ckpt.id, "."))
      load(ckpt_file, envir = tmp_env)
      ckpt_hash <- tmp_env$.hash

      # Compare the hash to determine whether re-evaluation is necessary
      if (deps_hash != ckpt_hash) {
        message("Dependent variables have changed!")
        return(checkpoint(
          expr,
          ckpt.id,
          deps = deps,
          force = TRUE,
          envir = envir,
          enclos = enclos
        ))
      }
      retval <- tmp_env$.retval
    } else {
      # Load the data in (re-)evaluation is necessary
      message("Evaluating expression.")
      retval <- eval(expr, envir = tmp_env)
      assign(".retval", retval, tmp_env)
      assign(".hash", deps_hash, tmp_env)
      save(
        list = ls(tmp_env, all.names = TRUE),
        envir = tmp_env,
        file = ckpt_file
      )
    }

    # Populate the parent environment
    copy.env(tmp_env, envir)

    return(retval)
  }

#' Helper function to copy the content of one environment to another
#' @param source_env the environment from which data will be copied
#' @param destination_env the environment to which data will be copied
#' @param all.names whether or not to include hidden variables
copy.env <-
  function(source_env, destination_env, all.names = FALSE) {
    sapply(ls(source_env, all.names = all.names), function(n) {
      assign(n, get(n, source_env), destination_env)
    })
  }
