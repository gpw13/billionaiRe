#' Upload Billions indicator data
#'
#' `upload_billion_data()` allows you to easily upload a data frame to the correct
#' location in the World Health Data Hub. By default, it appends the current
#' timestamp to the file name, but it can also accept a timestamp specified by the
#' user.
#'
#' This function requires that the user to have the
#' [whdh](https://github.com/WorldHealthOrganization/whdh) package installed
#' and setup to access the data. For quetions about getting the relevant permissions,
#' please contact <kanjim@who.int> or <messeillere@who.int>.
#'
#' @param df (data.frame) A data frame
#' @param data_type (string) The type of data to load.
#' * `wrangled_data` (default): raw data that has been wrangled into a suitable
#'   form for analysis.
#' * `projected_data`:  data that has been fully projected to the target year but
#'   has not yet been transformed or calculated upon.
#' * `final_data`: the complete set of billions data with transformed values, contributions,
#'   and all calculations available.
#' @param billion (string) One of "hep", "hpop", or "uhc". Ignored when
#'   `data_type = "final_data"`.
#' @param ind_code (string) The name of the indicator to upload data
#'  for. Ignored when `data_type = "final_data"`.
#' @param version A `yyyy-mm-ddTHH-MM-SS` formatted string. The default is the current
#' date time, as returned by [whdh::get_formatted_timestamp()].
#' @param na_rm (logical) Specifies whether to remove rows where `value` is missing.
#'   Defaults to `FALSE`.
#' @param experiment (string) Either `NULL` or a string ("unofficial" by default).
#' Identifies where the Bronze/Silver/Gold data layers to which data is uploaded
#' are located. Cannot be an empty string.
#' * If `NULL`, the root folder for the data layers is the 3B folder (i.e., where
#' the "official" data is stored). For example, `3B/Silver/...`.
#' * If a string, the root folder for the data layers is a sub-folder within the
#' Sandbox layer of the 3B data lake (e.g., if `experiment = "my_exp"`, then
#' data is download from `3B/Sandbox/my_exp/{data_layer}/...`)
#' @param silent (logical) Specifies whether to show authentication messages and
#'   a progress bar. Defaults to `TRUE`.
#' @param retry_upload (logical) Retry the upload request to WHDH.
#' @param max_upload_retries (integer) Maximum number of times to retry upload to WHDH. Defaults to `3`.
#' @param upload_retry_interval (integer) Amount of time in seconds to wait before retrying upload to WHDH. Defaults to `3`.
#'
#' @return A data frame. Note that this is the modified version of in the input
#    data frame that is uploaded to WHDH. As such, any modifications required by
#'   the function (such as from removing empty rows when `na_rm = TRUE` or from
#'   the call to [save_wrangled_output()]) are carried over to the output.
#'
#' @export
#'
#' @family load_data
#'
upload_billion_data <- function(df,
                                data_type = c("wrangled_data", "projected_data", "final_data"),
                                billion = c("hep", "hpop", "uhc"),
                                ind_code,
                                version = whdh::get_formatted_timestamp(),
                                na_rm = TRUE,
                                experiment = NULL,
                                silent = TRUE,
                                retry_upload = TRUE,
                                max_upload_retries = 3,
                                upload_retry_interval = 3) {
  data_type <- rlang::arg_match(data_type)
  billion <- rlang::arg_match(billion)
  assert_df(df)
  assert_timestamp(version)

  output_file <- tempfile(fileext = ".parquet")

  output_df <- df %>%
    save_wrangled_output(output_file, data_type, na_rm, "gzip")

  if(data_type == "final_data"){
    file_names <- sprintf("%s_%s.parquet", data_type, version)
  }else{
    file_names <- sprintf("%s_%s_%s.parquet", billion, ind_code, version)
  }

  upload_path <- get_whdh_path(
    operation = "upload",
    data_type = data_type,
    billion = billion,
    ind_codes = ind_code,
    file_names = file_names,
    experiment = experiment
  )

  if (file.size(output_file) == 0) {
    cli::cli_abort("The file being uploaded is empty.")
  }

  if (retry_upload) {
    retry::retry({
      whdh::upload_to_data_lake(
        data_lake_name = get_data_lake_name(),
        container = "whdh",
        source_path = output_file,
        destination_path = upload_path,
        silent = FALSE
      )
    },
    when = "Failed to complete Storage Services operation.",
    interval = upload_retry_interval,
    max_tries = max_upload_retries)
  } else {
    whdh::upload_to_data_lake(
      data_lake_name = get_data_lake_name(),
      container = "whdh",
      source_path = output_file,
      destination_path = upload_path,
      silent = FALSE
    )
  }

  return(output_df)
}
