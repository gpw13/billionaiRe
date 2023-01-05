#' @noRd
#'
#' @keywords internal
get_right_parameters <- function(params, fn) {
  params_fn <- params[names(params) %in% names(formals(fn))]

  if (length(params_fn) > 0) {
    return(params_fn)
  }
}

#' @noRd
#'
#' @keywords internal
get_dots_and_call_parameters <- function(...,
                                         remove_df = TRUE){
  dots <- rlang::list2(...)

  params <- rlang::env_clone(rlang::caller_env()) %>%
    rlang::env_unbind("...") %>%
    as.list()

  if(remove_df){
    params <- params[names(params) != "df"]
  }

  if(!purrr::is_empty(dots)){
    set_parameters(params, !!!dots)
  }else{
    params
  }

}

#' @noRd
#'
#' @keywords internal
name_call_parameters <- function(call_parameters, fn_formals_names){

  for(i in 1:length(call_parameters)){
    if(nchar(names(call_parameters)[i]) == 0 && call_parameters[[i]] != "..."){
      names(call_parameters)[i] <- fn_formals_names[i]
    }
  }
  return(call_parameters)
}

#' @noRd
#'
#' @keywords internal
set_parameters <- function(parameters, ...){
  changes <- rlang::list2(...)

  assert_has_names(changes)

  replace(parameters, names(changes), changes)
}
