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
  # Generate the right upload path for the data lake
  tstamp <- whdh::get_formatted_timestamp()
  gho_backup_path <-  stringr::str_glue("3B/Bronze/{billion}/{billion}_{gpw13_code}/{billion}_{gpw13_code}_gho_{tstamp}.parquet")

  # Save the data frame to a temporary parquet file
  temp_file <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, temp_file, compression = "gzip")

  # Upload the parquet file to the data lake
  whdh::upload_to_data_lake(data_lake_name = Sys.getenv("DATA_LAKE_NAME"),
                            source_path = temp_file,
                            destination_path = gho_backup_path,
                            remove = TRUE)

  # Return the input data frame
  df
}
