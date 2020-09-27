get_gho_codes <- function(billion = c("uhc", "hpop", "hep")) {
  billion <- rlang::arg_match(billion)

  df <- dplyr::filter(indicator_df,
                      .data[[billion]],
                      .data[["storage_location"]] %in% c("gho", "both"))
  codes1 <- df[["gho_code"]]
  codes2 <- df[["gho_code_2"]]
  x <- c(codes1, codes2)
  x[!is.na(x)]
}

get_gho_queries <- function(gho_codes) {
  idx <- match(gho_codes, indicator_df[["gho_code"]])
  idx <- ifelse(is.na(idx), match(gho_codes, indicator_df[["gho_code_2"]]), idx)
  x <- indicator_df[["gho_queries"]]
  x[idx]
}

get_xmart_codes <- function(billion = c("uhc", "hpop", "hep")) {
  billion <- rlang::arg_match(billion)

  df <- dplyr::filter(indicator_df,
                      .data[[billion]],
                      .data[["storage_location"]] %in% c("xmart", "both"))
  x <- df[["xmart_code"]]
  x[!is.na(x)]
}

db_id_to_analysis_id <- function(db_ids) {
  idx <- match(db_ids, indicator_df[["dashboard_id"]])
  x <- indicator_df[["analysis_code"]]
  x[idx]
}

analysis_id_to_db_id <- function(inds) {
  idx <- match(inds, indicator_df[["analysis_code"]])
  x <- indicator_df[["dashboard_id"]]
  x[idx]
}

gho_codes_to_db_id <- function(gho_codes) {
  idx <- match(gho_codes, indicator_df[["gho_code"]])
  idx <- ifelse(is.na(idx), match(gho_codes, indicator_df[["gho_code_2"]]), idx)
  x <- indicator_df[["analysis_code"]]
  x[idx]
}

inds_to_source <- function(inds) {
  idx <- match(inds, indicator_df[["analysis_code"]])
  x <- indicator_df[["data_source"]]
  x[idx]
}
