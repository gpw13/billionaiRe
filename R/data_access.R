#' @export
billion_ind_codes <- function(billion = c("hep", "hpop", "uhc")) {
  billion <- rlang::arg_match(billion)
  df <- billionaiRe::indicator_df
  codes <- df[["analysis_code"]][df[[billion]]]
  names(codes) <- codes
  codes
}

get_sdi_ratio <- function(iso3) {
  sdi_ratio[['sdiratio']][match(iso3, sdi_ratio[['iso3']])]
}
