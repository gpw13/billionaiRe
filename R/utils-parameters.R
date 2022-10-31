get_right_parameters <- function(params, fn) {
  params_fn <- params[names(params) %in% names(formals(fn))]

  if (length(params_fn) > 0) {
    return(params_fn)
  }
}

get_dots_and_call_parameters <- function(...,
                             call_parameters = rlang::caller_call(),
                             fn_formals = rlang::fn_fmls(rlang::caller_fn()),
                             remove_df = TRUE,
                             remove_unnamed = TRUE){
  dots <- rlang::list2(...)

  call_parameters <- as.list(call_parameters)

  fn_formals <- fn_formals[names(fn_formals) != "..."]


  for(i in 1:length(fn_formals)){
    if(class(fn_formals[[i]]) == "call"){
      fn <- as.character(fn_formals[[i]][1])

      args <- rlang::list2(as.character(fn_formals[[i]][2:length(fn_formals[[i]])]))

      fn_formals[[i]] <- rlang::exec(fn, !!!args)
    }else{
      fn_formals[[i]] <- fn_formals[[i]]
    }
  }

  if(remove_unnamed){
    call_parameters <- call_parameters[nchar(names(call_parameters)) > 0]
    dots <- dots[nchar(names(dots)) > 0]
  }

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

set_parameters <- function(parameters, ...){
  changes <- rlang::list2(...)

  assert_has_names(changes)

  replace(parameters, names(changes), changes)
}
