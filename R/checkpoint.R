#' Evaluates an expression or loads a previously checkpointed value
#'
#' Both the change to the environment and the expression's return value is stored.
#' The expression is re-evaluated if the expression or value of the dependent variables change.
#'
#' @param expr any valid R expression
#' @param ckpt.id an identifier used for the checkpointed data
#' @param ... any one or more R objects that are monitored for changes and used to trigger re-evaluation
#' @param force force re-evaluation of the expression
#' @param check.deps whether or not to check the dependent variables in \code{...} for changes
#' @param envir the environment in which \code{expr} is evaluated
#' @param enclos the environment in which \code{expr} is substituted
#' @param file the checkpoint file name
#' @return the result of running \code{expr} or the checkpointed value
#' @examples
#' checkpointr::checkpoint({x <- 42}, "checkpoint1")
#' @export
checkpoint <-
  function(expr,
           ckpt.id,
           ...,
           force = FALSE,
           check.deps = TRUE,
           envir = parent.frame(),
           enclos = environment(),
           file = "checkpoint.dat") {

    # Set up an environment in which to store the result of the evaluated expression
    tmp_env <- new.env(parent = envir)

    # Substitute the expression
    expr <- substitute(expr, enclos)

    # Calculate the hash used to determine whether a re-evaluation is necessary
    deps_hash <-
      digest::digest(list(deps = ..., expr = deparse(expr)), "sha256")

    if (!force && has.cache(ckpt.id, file)) {
      # Load the checkpoint if the checkpoint file exists
      message(paste0("Loading checkpoint ", ckpt.id, "."))
      restore.env(tmp_env, ckpt.id, file)
      ckpt_hash <- tmp_env$.hash

      # Compare the hash to determine whether re-evaluation is necessary
      if (check.deps && (deps_hash != ckpt_hash)) {
        message("Dependent variables have changed!")
        return(checkpoint(
          expr,
          ckpt.id,
          ...,
          force = TRUE,
          envir = envir,
          enclos = enclos,
          file = file
        ))
      }
      retval <- tmp_env$.retval
    } else {
      # Load the data in (re-)evaluation is necessary
      message("Evaluating expression.")
      retval <- eval(expr, envir = tmp_env)
      assign(".retval", retval, tmp_env)
      assign(".hash", deps_hash, tmp_env)
      store.env(tmp_env, ckpt.id, file)
    }

    # Populate the parent environment
    copy.env(tmp_env, envir)

    return(retval)
  }

#' Copy an environment
#'
#' Copies an environment into a checkpoint file and associates it with an id.
#' Multiple environments can be stored in one file.
#'
#' @param envir the environment to copy
#' @param ckpt.id the identifier to associate the environment with
#' @param file the file that will contain the copy
store.env <- function(envir, ckpt.id, file) {
  cache <- list()
  if (file.exists(file)) {
    load(file)  # This will populate `cache`
  }

  cache[[ckpt.id]] <- list()
  for (var in ls(envir = envir, all.names = TRUE)) {
    cache[[ckpt.id]][[var]] <- get(var, envir = envir)
  }

  save(
    cache,
    file = file
  )
}

#' Restore an environment
#'
#' Loads an environment by its id from the given file
#'
#' @param envir the environment in which to restore the variables
#' @param ckpt.id the environment id to load
#' @param file the file from which to load the environment
restore.env <- function(envir, ckpt.id, file) {
  cache <- list()

  if (file.exists(file)) {
    load(file)  # This will populate `cache`
  }

  tmp_env <- new.env()
  ckpt <- cache[[ckpt.id]]

  sapply(names(ckpt), function(var) {
    assign(var, ckpt[[var]], envir = tmp_env)
  })

  copy.env(tmp_env, envir, all.names = TRUE)
}

#' Checks whether the checkpoint id exists
#'
#' @param ckpt.id the checkpoint identifier
#' @param file the checkpoint file to check
#' @return TRUE if the checkpoint id exists, FALSE if not
has.cache <- function(ckpt.id, file) {
  cache <- list()
  if (!file.exists(file)) {
    return(FALSE)
  }
  load(file)  # This will populate `cache`
  return(ckpt.id %in% names(cache))
}

#' Helper function to copy the content of one environment to another
#'
#' @param source_env the environment from which data will be copied
#' @param destination_env the environment to which data will be copied
#' @param all.names whether or not to include hidden variables
copy.env <-
  function(source_env, destination_env, all.names = FALSE) {
    sapply(ls(source_env, all.names = all.names), function(n) {
      assign(n, get(n, source_env), destination_env)
    })
  }
