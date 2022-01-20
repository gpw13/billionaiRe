get_right_params <- function(params, fn) {
  params_fn <- params[names(params) %in% names(formals(fn))]

  if (length(params_fn) > 0) {
    return(params_fn)
  }
}
