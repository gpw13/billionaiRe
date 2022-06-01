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
save_gho_backup_to_whdh <- function(df,
                                    billion,
                                    ind_code,
                                    silent = FALSE) {

  # Generate the upload path for the data lake
  tstamp <- whdh::get_formatted_timestamp()
  file_name <- sprintf("%s_%s_gho_%s.parquet", billion, ind_code, tstamp)
  gho_backup_path <- get_whdh_path(
    operation = "upload",
    billion = billion,
    ind_codes = ind_code,
    data_type = "ingestion_data",
    file_names = file_name
  )

  # Save the data frame to a temporary parquet file
  temp_file <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, temp_file, compression = "gzip")

  # Upload the parquet file to the data lake
  whdh::upload_to_data_lake(
    data_lake_name = get_data_lake_name(),
    container = "dropzone",
    source_path = temp_file,
    destination_path = gho_backup_path,
    validate_user_input = FALSE,
    silent = silent
  )

  # Return the input data frame
  df
}

#' Generate file paths for the Triple Billions WHDH data lake
#'
#' `get_whdh_path()` simplifies the process of generating accurate file paths for
#' downloading/uploading files from the Triple Billions WHDH data lake.
#'
#' Using this function when working with the data lake is highly recommended as
#' it ensures file paths abide by the established standards and directory structure
#' for the data lake.
#'
#' @param operation (string) Either "download" or "upload".
#' @param data_type (string) The type of data to load.
#' * `wrangled_data` (default): raw data that has been wrangled into a suitable
#'   form for analysis.
#' * `projected_data`:  data that has been fully projected to the target year but
#'   has not yet been transformed or calculated upon.
#' * `final_data`: the complete set of billions data with transformed values, contributions,
#'   and all calculations available.
#' * `ingestion_data`: raw data in its original form as received from the technical
#'  program, GHO, or other sources. These files have not been wrangled or modified
#'  in any way.
#' @param billion (string) One of "all" (default), "hep", "hpop", or "uhc". If "all",
#' the file paths for all indicators in all three bilions are returned.
#' @param ind_codes (character vector) The name of the indicator (or indicators) to load data for.
#'   If `all`, returns paths for all indicators for a given billion. If `billion = "all"`,
#'   this argument is ignored and the file paths for all indicators in all three bilions
#'   are returned.
#' @param file_names (character vector) The name(s) of the file(s) to download.
#'   NULL by default. Ignored if either `billion = "all"` or `ind_codes = "all"`.
#' @param experiment (string) Either `NULL` or a string ("unofficial" by default).
#' * If `NULL`, the root folder for the data layers is the 3B folder (i.e., where
#'   the "official" data is stored (e.g., `3B/...`).
#' * If a string, the root folder for the data layers is a sub-folder within the
#'   Sandbox layer of the 3B data lake (e.g., if `experiment = "my_exp"`, then
#'   paths would be of the form `3B/Sandbox/my_exp/Silver/...`)
#'
#' @return A character vector.
#' @export
#'
get_whdh_path <- function(operation = c("download", "upload"),
                          data_type = c("wrangled_data", "projected_data", "final_data", "ingestion_data"),
                          billion = c("all", "hep", "hpop", "uhc"),
                          ind_codes = "all",
                          file_names = NULL,
                          experiment = "unofficial") {

  # Argument checks and assertions
  operation <- rlang::arg_match(operation)
  data_type <- rlang::arg_match(data_type)
  billion <- rlang::arg_match(billion)
  assert_type(billion, "character")
  assert_type(ind_codes, "character")
  assert_type(experiment, c("NULL", "character"))
  assert_equals(experiment, "", identical = TRUE, reverse = TRUE)

  # For billion = "all", recursively call the function with each billion
  if (billion == "all" && data_type != "final_data") {
    message("`ind_codes` and `file_names` arguments ignored when billion = \"all\"")
    paths <- c("hep", "hpop", "uhc") %>%
      # rlang::set_names() %>%
      purrr::map(~ get_whdh_path(operation, data_type, .x, "all", NULL, experiment)) %>%
      unlist() %>%
      unique() %>%
      suppressMessages()

    return(paths)
  }

  # file_names is required for uploads
  # And only single files can be uploaded so billion and ind_codes arguments cannot be "all"
  if (operation == "upload") {
    # TODO: removing these assertions for now. Dealing with billion and ind_codes for final_data is ambiguous
    # assert_equals(billion, "all", reverse = TRUE, msg_suffix = "when uploading.")
    # assert_equals(ind_codes, "all", reverse = TRUE, msg_suffix = "when uploading.")
    assert_arg_exists(file_names, "The %s argument is required when uploading to the data lake.")
  }

  # When downloading_ingestion data, require file_names because this folder is not
  # versioned an does not abide by WHDH naming conventions
  if (operation == "download" && data_type == "ingestion_data") {
    assert_arg_exists(file_names, "The %s argument is required when data_type = \"ingestion_data\".")
  }

  # Amend root folder based on experiment argument
  if (is.null(experiment)) {
    root <- "3B"
  } else {
    assert_length(experiment, 1)
    root <- paste("3B", "Sandbox", experiment, sep = "/")
  }

  data_layer <- get_data_layer(data_type)

  # If final_data, data_asset is just final_data
  if (data_type == "final_data") {
    message("`billion` and `ind_codes` arguments ignored when data_type = \"final_data\"")
    data_asset <- "final_data"
  } else {

    # Remove later
    # billion = rlang::arg_match(billion)
    # assert_arg_exists(ind_codes, "The %s argument is required for all data types except \"final_data\".")

    valid_inds <- get_valid_inds(data_type, billion)

    # Using identical instead of == to circumvent issues when ind_codes is a vector
    if (identical(ind_codes, "all")) {

      # Set ind_codes equal to list of all valid indicators
      message("`file_names` argument ignored when ind_codes = \"all\"")
      ind_codes <- valid_inds
      file_names <- NULL
    } else {
      # Or check that given indicators are valid indicators
      assert_x_in_y(ind_codes, valid_inds)
    }

    data_asset <- paste(billion, ind_codes, sep = "_")
  }

  # If none of file_names is NULL or NA
  if (!is.null(file_names) && any(is.na(file_names))) {
    stop("`file_names` cannot be NA")
  } else if (!is.null(file_names) && all(!is.na(file_names))) {

    # Ensure that ind_codes and file_names have a 1-to-1 mapping or that one of them
    # is of length 1 and can be recycled to match the length of the other
    assert_same_length(ind_codes, file_names, recycle = TRUE, remove_null = FALSE)

    # Ensure that file_names have the right extensions
    assert_fileext(file_names, c("csv", "xls", "xlsx", "parquet", "feather"))
  } else if (is.null(file_names)) {
    # Otherwise, set file_names to be an empty string so data asset folders end with /
    file_names <- ""
  } else {
    stop("Invalid value passed to `file_names`.")
  }

  path <- paste(root, data_layer, data_type, data_asset, file_names, sep = "/") %>%
    stringr::str_replace("uhc_espar", "hep_espar") %>%
    stringr::str_replace("//+", "/")

  return(path)
}

#' Get correct set of indicators for WHDH operations
#'
#' @inheritParams get_whdh_path
#' @param billion (string) One of "hep", "hpop", and "uhc".
#'
#' @return (character vector) a list of the indicators belonging to the given billion,
#'   as used by the WHDH functions.
#'
get_valid_inds <- function(data_type, billion) {
  assert_type(data_type, "character")
  assert_type(billion, "character")
  assert_length(data_type, 1)
  assert_length(data_type, 1)

  # include_covariates = TRUE to include pneumo_mort for wrangled_data and
  # ingestion_data but not other data types
  incl_covars <- ifelse(data_type %in% c("ingestion_data", "wrangled_data"), TRUE, FALSE)

  valid_inds <- billion_ind_codes(billion,
    include_covariates = incl_covars,
    include_calculated = FALSE,
    include_subindicators = FALSE
  )
  # Remove surviving_infants from the list of valid_inds as it's not dealt with in whdh anymore.

  valid_inds <- purrr::discard(valid_inds, ~ .x == "surviving_infants")

  # Remove hwf from the list of valid_inds for wrangled_data because hwf is only
  # added during the 02_projecting stage. We only wrangle doctors and nurses in the
  # 01_ingestion stage.
  if (data_type == "wrangled_data") {
    valid_inds <- purrr::discard(valid_inds, ~ .x == "hwf")
  }

  valid_inds
}

#' @noRd
get_data_layer <- function(data_type) {
  switch(data_type,
    ingestion_data = "Bronze",
    wrangled_data = "Silver",
    projected_data = "Silver",
    final_data = "Gold"
  )
}

#' Return the name of the 3B data lake
#'
#' @return A string. The name of the Triple Billions WHDH data lake.
#' @export
get_data_lake_name <- function() {
  "srhdteuwstdsa"
}
