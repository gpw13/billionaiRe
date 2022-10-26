get_right_parameters <- function(params, fn) {
  params_fn <- params[names(params) %in% names(formals(fn))]

  if (length(params_fn) > 0) {
    return(params_fn)
  }
}

get_dots_and_env <- function(...){

  dots <- rlang::list2(...)

  call_params <- as.list(rlang::caller_call(1))

  named_params <- as.character(names(call_params)) %>%
    purrr::keep(~nchar(.x)>1)

  call_params <- call_params[names(call_params) %in% named_params] %>%
    purrr::discard(~is.data.frame(.x))

  if(!purrr::is_empty(dots)){
    set_parameters(call_params, dots)
  }else{
    call_params
  }
}

set_parameters <- function(parameters, changes){
  assert_list(changes)
  assert_has_names(changes)

  replace(parameters, names(changes), changes)
}
