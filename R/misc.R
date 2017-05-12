v_eval <- function(x, tryres, data) {
  if (!inherits(tryres, "try-error") && !inherits(x, "name"))
    return(x)

  res <- try(eval(x, data), silent = TRUE)

  if (inherits(res, "try-error")) {
    res <- try(eval(x), silent = TRUE)
    if (inherits(res, "try-error")) {
      stop("argument '", deparse(x), "' cannot be found")
    }
  }

  res
}
