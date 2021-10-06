#' Backup GHO data to the WHDH Data Lake
#'
#' This function simplifies the process of creating backups snapshots of GHO data
#' and saving these to the WHDH data lake.
#'
#' It returns the input data frame, without any changes.
#'
#' @param df The response from the GHO API, typically the result of calling
#' `ghost::gho_data()`.
#' @param billion The billion to which the indicator belongs
#' @param ind_code The GPW13 code for the indicator
#'
#' @return The original data frame
#' @export
save_gho_backup_to_whdh = function(df,
                                   billion,
                                   ind_code) {
  # Generate the upload path for the data lake
  gho_backup_path <-  get_whdh_path(billion = billion,
                                    ind_code = ind_code,
                                    file_type = "gho_backup")

  # Save the data frame to a temporary parquet file
  temp_file <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, temp_file, compression = "gzip")

  # Upload the parquet file to the data lake
  whdh::upload_to_data_lake(data_lake_name = get_data_lake_name(),
                            container = "whdh",
                            source_path = temp_file,
                            destination_path = gho_backup_path,
                            skip_validation_checks = TRUE)

  # Return the input data frame
  df
}

#' Generate file paths for Triple Billions WHDH data lake
#'
#' `get_whdh_path()` simplifies the process of generating accurate file paths for
#' downloading/uploading files from the Triple Billions WHDH data lake.
#'
#' Using this function where working with the data lake is highly recommended as
#' it ensures file paths abide by the established standard and directory structure
#' for the data lake.
#'
#' @param billion The billion to which the indicator belongs.
#' @param ind_code The GPW13 code for the indicator.
#' @param file_type The type of file/data asset. Cane be one of "ingestion",
#'   "gho_backup", "unproj_data", "proj_data", "calc_data" and "full_data"
#' @param file_name The name of the file to download. Only relevant for
#'   ingestion files. NULL by default
#'
#' @return A string with the properly formatted file path, abiding by the standardised
#'   directory structure and naming conventions for the 3B WHDH data lake.
#' @export
get_whdh_path = function(billion = c("hep", "hpop", "uhc"),
                         ind_code,
                         file_type = c("ingestion", "gho_backup", "unproj_data", "proj_data", "calc_data", "full_data"),
                         file_name = NULL) {

  # Argument checks and assertions
  billion = rlang::arg_match(billion)
  file_type = rlang::arg_match(file_type)
  assert_string(ind_code, 1)
  assert_string(file_name, 1)

  tstamp <- whdh::get_formatted_timestamp()

  if (file_type == "ingestion") {

    # Ensure the file_name argument exists
    assert_arg_exists(file_name, "The %s argument is required when file_type = 'ingestion' and cannot be NA or NULL")

    # Ensure that the file has the right extension
    assert_fileext(file_name, c("csv", "xls", "xlsx", "parquet", "feather"))

    stringr::str_glue("3B/Bronze/{billion}/{billion}_{ind_code}/{file_name}")

  } else if (file_type == "gho_backup") {

    stringr::str_glue("3B/Bronze/{billion}/{billion}_{ind_code}/{billion}_{ind_code}_gho_{tstamp}.parquet")

  } else {

    stringr::str_glue("3B/Silver/{file_type}/{billion}/{billion}_{ind_code}/{billion}_{ind_code}_{tstamp}.parquet")

  }
}

#' Return the name of the 3B data lake
#'
#' @return A string. The name of the Triple Billions WHDH data lake.
#' @export
get_data_lake_name = function() {
  "srhdteuwstdsa"
}
