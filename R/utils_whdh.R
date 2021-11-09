#' Return the name of the 3B data lake
#'
#' @return A string. The name of the Triple Billions WHDH data lake.
#' @export
get_data_lake_name = function() {
  "srhdteuwstdsa"
}

#' Backup GHO data to the WHDH Data Lake
#'
#' This function simplifies the process of creating backups snapshots of GHO data
#' and saving these to the WHDH data lake.
#'
#' It returns the input data frame, without any changes.
#'
#' @param df The response from the GHO API, typically the result of calling
#' [ghost::gho_data()].
#' @param billion The billion to which the indicator belongs
#' @param ind_code The GPW13 code for the indicator
#' @param silent Passed to [whdh::upload_to_data_lake()]. Determines if upload
#'   messages and progress bar are shown
#'
#' @return The original data frame
#' @export
save_gho_backup_to_whdh = function(df,
                                   billion,
                                   ind_code,
                                   silent = FALSE) {

  # Generate the upload path for the data lake
  tstamp = whdh::get_formatted_timestamp()
  file_name = sprintf("%s_%s_gho_%s.parquet", billion, ind_code, tstamp)
  gho_backup_path <-  get_whdh_path(operation = "upload",
                                    billion = billion,
                                    ind_code = ind_code,
                                    data_type = "ingestion_data",
                                    file_name = file_name)

  # Save the data frame to a temporary parquet file
  temp_file <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, temp_file, compression = "gzip")

  # Upload the parquet file to the data lake
  whdh::upload_to_data_lake(data_lake_name = get_data_lake_name(),
                            container = "dropzone",
                            source_path = temp_file,
                            destination_path = gho_backup_path,
                            validate_user_input = FALSE,
                            silent = silent)

  # Return the input data frame
  df
}

#' Generate file paths for the Triple Billions WHDH data lake
#'
#' `get_whdh_path()` simplifies the process of generating accurate file paths for
#' downloading/uploading files from the Triple Billions WHDH data lake.
#'
#' Using this function where working with the data lake is highly recommended as
#' it ensures file paths abide by the established standard and directory structure
#' for the data lake.
#'
#' @param operation Either "download" or "upload".
#' @param billion The billion to which the indicator belongs.
#' @param ind_code The GPW13 code for the indicator.
#' @param data_type The type of file/data asset. Cane be one of "ingestion_data",
#'   "gho_backup", "unproj_data", "proj_data", "calc_data" and "full_data"
#' @param file_name The name of the file to download. NULL by default
#'
#' @return A string with the properly formatted file path, abiding by the standardised
#'   directory structure and naming conventions for the 3B WHDH data lake.
#' @export
get_whdh_path = function(operation = c("download", "upload"),
                         data_type = c("ingestion_data", "wrangled_data", "projected_data", "final_data"),
                         billion = c("hep", "hpop", "uhc"),
                         ind_code,
                         file_name = NULL) {

  # Argument checks and assertions
  operation = rlang::arg_match(operation)
  data_type = rlang::arg_match(data_type)
  assert_string(file_name, 1)

  data_layer = data_layer = get_data_layer(data_type)

  # If final_data, no data_asset is needed
  if (data_type == "final_data") {
    message("`billion` and `ind_code` arguments ignored when data_type = \"final_data\"")
    data_asset = ""
  }
  # Otherwise check billion and ind_code are provided and
  # set data_asset = {billion}_{ind_code}
  else {
    billion = rlang::arg_match(billion)
    assert_arg_exists(ind_code, "The %s argument is required for all data types except \"final_data\".")
    data_asset = sprintf("%s_%s/", billion, ind_code)
  }

  if (operation == "download") {

    # When downloading_ingestion data, require a file_name because this folder
    # is not versioned an does not abide by WHDH naming conventions
    if (data_type == "ingestion_data") {
      assert_arg_exists(file_name, "The %s argument is required when data_type = \"ingestion_data\".")
    }

  } else {
    # A file_name is required for uploads
    assert_arg_exists(file_name, "The %s argument is required when uploading to the data lake.")

  }

  # If a file name is provided, append it to the data_asset
  if (!is.null(file_name) && !is.na(file_name)) {
    # Ensure that the file has the right extension
    assert_fileext(file_name, c("csv", "xls", "xlsx", "parquet", "feather"))
    data_asset = paste0(data_asset, file_name)
  }

  sprintf("3B/%s/%s/%s", data_layer, data_type, data_asset)
}

# #' Returns the correct download paths for the Triple Billions WHDH data lake
# #'
# #' @noRd
# get_whdh_download_path = function(data_type = c("ingestion_data", "wrangled_data", "projected_data", "final_data"),
#                                   billion = c("hep", "hpop", "uhc"),
#                                   ind_code,
#                                   file_name = NULL) {
#
#   data_layer = data_layer = get_data_layer(data_type)
#
#   # Ensure that a file_name is always provided for ingestion_data
#   if (data_type == "ingestion_data") {
#     assert_arg_exists(file_name, "The %s argument is required when data_type = 'ingestion_data' and cannot be NA or NULL")
#   }
#
#   # If final_data, no data_asset is needed
#   if (data_type == "final_data") {
#     message("`billion` and `ind_code` arguments ignored when data_type = \"final_data\"")
#     data_asset = ""
#   } # Otherwise check ind_code is provided and set data_asset = {billion}_{ind_code}
#   else {
#     assert_arg_exists(ind_code, "The %s argument is required for all data types except \"final_data\".")
#     data_asset = sprintf("%s_%s/", billion, ind_code)
#   }
#
#   # If a file name is provided, append it to the data_asset
#   if (!is.null(file_name) && !is.na(file_name)) {
#     # Ensure that the file has the right extension
#     assert_fileext(file_name, c("csv", "xls", "xlsx", "parquet", "feather"))
#     data_asset = paste0(data_asset, file_name)
#   }
#
#   # if (data_type == "final_data") {
#   #   path = sprintf("3B/%s/%s/", data_layer, data_type)
#   # } else {
#   #   path = sprintf("3B/%s/%s/%s/", data_layer, data_type, data_asset)
#   # }
#   #
#   # if (data_type == "ingestion_data") {
#   #
#   #   # Ensure the file_name argument exists
#   #   assert_arg_exists(file_name, "The %s argument is required when data_type = 'ingestion_data' and cannot be NA or NULL")
#   #
#   #   # Ensure that the file has the right extension
#   #   assert_fileext(file_name, c("csv", "xls", "xlsx", "parquet", "feather"))
#   #
#   #   path = sprintf("3B/%s/%s/%s/%s", data_layer, data_type, data_asset, file_name)
#   #
#   # } else if (data_type == "final_data") {
#   #
#   #   # TODO: Decide on file structure for gold layer/final_data
#   #   return(NULL)
#   #
#   # } else {
#   #
#   #   # If a file_name is not provided, then return the data asset folder
#   #   if (is.null(file_name)) {
#   #
#   #     path = sprintf("3B/%s/%s/%s/", data_layer, data_type, data_asset)
#   #
#   #   } else {
#   #
#   #     # Ensure that the file has the right extension
#   #     assert_fileext(file_name, c("csv", "xls", "xlsx", "parquet", "feather"))
#   #
#   #     path = sprintf("3B/%s/%s/%s/%s", data_layer, data_type, data_asset, file_name)
#   #   }
#   # }
#
#   sprintf("3B/%s/%s/%s", data_layer, data_type, data_asset)
# }
#
# #' Returns the correct upload paths for the Triple Billions WHDH data lake
# #'
# #' @noRd
# get_whdh_upload_path = function(data_type = c("ingestion_data", "wrangled_data", "projected_data", "final_data"),
#                                 billion = c("hep", "hpop", "uhc"),
#                                 ind_code,
#                                 file_name = NULL) {
#
#   # All upload paths must have a file_name with a valid extension
#   assert_arg_exists(file_name, "The %s argument is required when uploading to the data lake.")
#   assert_fileext(file_name, c("csv", "xls", "xlsx", "parquet", "feather"))
#
#   data_layer = get_data_layer(data_type)
#   data_asset = sprintf("%s_%s", billion, ind_code)
#
#   path = sprintf("3B/%s/%s/%s/%s/%s", data_layer, data_type, billion, data_asset, file_name)
#   return(path)
# }

#' @noRd
get_data_layer = function(data_type) {
  switch(data_type,
         ingestion_data = "Bronze",
         wrangled_data = "Silver",
         projected_data = "Silver",
         final_data = "Gold"
  )
}
