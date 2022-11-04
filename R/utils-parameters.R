get_right_parameters <- function(params, fn) {
  params_fn <- params[names(params) %in% names(formals(fn))]

  if (length(params_fn) > 0) {
    return(params_fn)
  }
}

get_dots_and_call_parameters <- function(...,
                             remove_df = TRUE){
  dots <- rlang::list2(...)

  fn_formals_names <- rlang::fn_fmls_names(rlang::caller_fn())

  call_parameters <- rlang::call_args(rlang::caller_call()) %>%
    purrr::discard(rlang::is_missing)

  call_parameters <- call_parameters[call_parameters != "..."] %>%
    name_call_parameters(fn_formals_names) %>%
    purrr::map_if(is.call, rlang::eval_tidy)

  fn_formals <- rlang::fn_fmls(rlang::caller_fn())
  fn_formals <- fn_formals[names(fn_formals) != "..."] %>%
    purrr::map_if(is.call, rlang::eval_tidy)

  if(remove_df){
    call_parameters <- call_parameters[names(call_parameters) != "df"]
    fn_formals <- fn_formals[names(fn_formals) != "df"]
  }

  params <- c(call_parameters, fn_formals)[!duplicated(c(names(call_parameters), names(fn_formals)), fromLast = FALSE)]

  if(!purrr::is_empty(dots)){
    set_parameters(params, !!!dots)
  }else{
    params
  }

}

name_call_parameters <- function(call_parameters, fn_formals_names){

  for(i in seq_along(call_parameters)){
    if(nchar(names(call_parameters)[i]) == 0 && call_parameters[[i]] != "..."){
      names(call_parameters)[i] <- fn_formals_names[i]
    }
  }
  return(call_parameters)
}

set_parameters <- function(parameters, ...){
  changes <- rlang::list2(...)

  assert_has_names(changes)

  replace(parameters, names(changes), changes)
}
