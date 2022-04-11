#' Load Billions indicator data
#'
#' `load_billion_data()` provides access to the raw inputs to the Billions stored
#' within the World Health Data Hub or the World Health Organization's xMart4 database.
#' By default, it filters the loaded data to take the most recently uploaded data
#' for each indicator, country, and year. The user can also specify to take the
#' latest data before a certain time period, or to extract all data from the database.
#'
#' This function requires that the user have the [xmart4](https://github.com/gpw13/xmart4)
#' and [whdh](https://github.com/WorldHealthOrganization/whdh) packages installed
#' and setup to access the data. For quetions about getting the relevant permissions,
#' please contact <kanjim@who.int> or <messeillere@who.int>.
#'
#' @param data_type (string) The type of data to load.
#' * `wrangled_data` (default): raw data that has been wrangled into a suitable
#'   form for analysis.
#' * `projected_data`:  data that has been fully projected to the target year but
#'   has not yet been transformed or calculated upon.
#' * `final_data`: the complete set of billions data with transformed values, contributions,
#'   and all calculations available.
#' @param billion (string) One of "all" (default), "hep", "hpop", or "uhc". If "all",
#'   downloads all indicators for all three billions.
#' @param ind_codes (character vector) The name of the indicator (or indicators) to load data for.
#'   If `all`, downloads data for all indicators for a given billion. Ignored if
#'   billion = "all".
#' @param version (string) Either `latest`  or a `yyyy-MM-dd` or `yyyy-mm-ddTHH-MM-SS`
#' formatted string.
#' * If `latest`, the latest version of the data will be downloaded.
#' * If a `yyyy-MM-dd` formatted string, the latest version of the data on or
#' before the provided date will be downloaded.
#' * If a `yyyy-mm-ddTHH-MM-SS` formatted string, an exact match for the given
#' time stamp is sought, if it exists; otherwise, raises an error.
#' @param na_rm (logical) Specifies whether to filter the data to only rows
#'   where `value` is not missing. Defaults to `TRUE`.
#' @param experiment (string) Either `NULL` or a string ("unofficial" by default).
#' Identifies where the Bronze/Silver/Gold data layers from which data is downloaded
#' are located.
#' * If `NULL`, the root folder for the data layers is the 3B folder (i.e., where
#' the "official" data is stored). For example, `3B/Silver/...`.
#' * If a string, the root folder for the data layers is a sub-folder within the
#' Sandbox layer of the 3B data lake (e.g., if `experiment = "my_exp"`, then
#' data is download from `3B/Sandbox/my_exp/{data_layer}/...`)
#' * If an empty string, the root folder for the data layers is the Sandbox itself
#'   (i.e., if `experiment = ""`, then data is download from `3B/Sandbox/{data_layer}/...`)
#' @param silent (logical) Specifies whether to show authentication messages and
#'   a download progress bar. Defaults to `TRUE`.
#' @param data_source (string) Ether "whdh" or "xmart". Indicates where to download
#'   the data from
#' @param ... Additional arguments passed to `xmart4::xmart4_table()`. Use if
#'   you need to provide additional token specifications for Azure authentication.
#'
#' @return A data frame.
#'
#' @export
#'
load_billion_data <- function(data_type = c("wrangled_data", "projected_data", "final_data"),
                              billion = c("all", "hep", "hpop", "uhc"),
                              ind_codes = "all",
                              version = "latest",
                              na_rm = TRUE,
                              experiment = NULL,
                              silent = TRUE,
                              data_source = c("whdh", "xmart"),
                              ...) {
  data_type <- rlang::arg_match(data_type)
  billion <- rlang::arg_match(billion)
  data_source <- rlang::arg_match(data_source)
  assert_equals(version, "all", reverse = TRUE)

  if (data_source == "whdh") {
    load_billion_data_whdh(data_type, billion, ind_codes, version, na_rm, experiment, silent, ...)
  } else {
    message("`experiment` and `silent` arguments ignored when `data_source = \"xmart\"`.")
    load_billion_data_xmart(data_type, billion, ind_codes, version, na_rm, ...)
  }
}

#' Load Billions indicator data from WHDH
#'
#' @inheritParams load_billion_data
#'
#' @return A data frame.
#'
load_billion_data_whdh <- function(data_type = c("wrangled_data", "projected_data", "final_data"),
                                   billion = c("all", "hep", "hpop", "uhc"),
                                   ind_codes = "all",
                                   version = "latest",
                                   na_rm = TRUE,
                                   experiment = NULL,
                                   silent = TRUE) {

  # Assertions and checks
  requireNamespace("whdh", quietly = TRUE)
  billion <- rlang::arg_match(billion)
  data_type <- rlang::arg_match(data_type)
  assert_arg_exists(ind_codes, "The %s argument is required and cannot be NA or NULL")
  assert_equals(version, "all", reverse = TRUE)

  # Paths of items to download
  paths <- get_whdh_path("download", data_type, billion, ind_codes, experiment = experiment)
  assert_unique_vector(paths)

  # Ensure that each path has a corresponding data asset in the data lake
  data_lake <- get_data_lake_name()
  root <- if (is.null(experiment)) "3B" else paste("3B", "Sandbox", experiment, sep = "/")
  data_layer <- get_data_layer(data_type)
  dir_path <- sprintf("%s/%s/%s/", root, data_layer, data_type) %>%
    stringr::str_replace("//+", "/")

  valid_data_assets <- whdh::list_blobs_in_directory(data_lake,
    dir_path,
    silent = TRUE
  ) %>%
    dplyr::filter(.data[["isdir"]]) %>%
    dplyr::pull("name") %>%
    paste0(., "/")

  assert_x_in_y(paths, valid_data_assets)

  df <- purrr::map_dfr(paths, ~ {
    temp_dir <- tempdir()
    file_name <- stringr::str_match(.x, "^.+/(\\w+)/?$")[, 2]
    temp_file <- file.path(temp_dir, paste(file_name, "parquet", sep = "."))

    whdh::download_data_asset(
      data_lake_name = data_lake,
      data_asset_folder = .x,
      version = version,
      destination_dir = temp_dir,
      strip_timestamp = TRUE,
      overwrite_destination = TRUE,
      silent = silent
    )

    # whdh::download_from_data_lake(
    #   data_lake_name = data_lake,
    #   source_path = .x,
    #   destination_path = temp_file,
    #   latest_version_only = TRUE,
    #   silent = silent
    # )

    arrow::read_parquet(temp_file)
  }) %>%
    dplyr::rename_with(tolower)

  df %>%
    filter_billion_na(na_rm)
}

#' Load Billions indicator data from xMart
#'
#' @inheritParams load_billion_data
#'
#' @return A data frame.
#'
load_billion_data_xmart <- function(data_type = c("wrangled_data", "projected_data", "final_data"),
                                    billion = c("all", "hep", "hpop", "uhc"),
                                    ind_codes = "all",
                                    version = "latest",
                                    na_rm = TRUE,
                                    experiment = NULL,
                                    silent = TRUE) {
  # Temporary error
  stop("For loading data from xMart, please use the legacy version of this function: load_billion_data_legacy()")
}

#' Load Raw Billions Indicator Data
#'
#' `load_billion_data_legacy()` is the legacy version of [load_billion_data()].
#' It provides access to the raw inputs to the Billions stored within the World
#' Health Organization's xMart4 database. By default, it filters
#' the loaded data to take the most recently uploaded data for each indicator,
#' country, and year. The user can also specify to take the latest data before a
#' certain time period, or to extract all data from the database.
#'
#' This function requires that the user have the [xmart4](https://github.com/caldwellst/xmart4) package
#' installed and setup to access the GPW13 mart. Details are available on the GitHub
#' landing page linked above.
#'
#' @param billion Billions data to load, one of "hep" (default), "hpop", "uhc", or "all".
#' @param mart_table Name of xMart4 table in the GPW13 mart to load data from.
#' * `full_data`: Full Billions data with transformed values, contributions,
#' and all calculations available. (default)
#' * `raw_data`: Raw input Billions data, that has yet to be transformed or
#' calculated upon, but has been fully projected. Includes full history.
#' * `projected_data`: A copy of `raw_data` with only the latest data.
#' * `proj_data`: Raw input Billions data that has been projected by the
#' technical programme(s).
#' * `unproj_data`: Raw input Billions that has has not been projected by the
#' technical programme(s) and needs projection by DDI.
#' @param date_filter One of `NULL`, "latest", or a single date string. The date
#'     string needs to be in ISO6801 format, such as "1989-4-4" or "1988-06-21".
#'     If a date is provided, will only take values loaded on or prior to that
#'     date into the xMart4 database.
#' @param na_rm Logical value, specifying whether to filter the data to only rows
#'     where `value` is not missing. Defaults to `TRUE`.
#' @param format Specification of the output format to be returned by the xMart API.
#'     Defaults to `"csv"` for faster download and processing times.
#'     Passed to [xmart4::xmart4_table()]. See the
#'     [xMart4 API documentation](https://portal-uat.who.int/xmart4/docs/xmart_api/use_API.html)
#'     for details on all three options ("csv", "streaming", and "none").
#' @param ... Additional arguments passed to `xmart4::xmart4_table()`. Use if
#'     you need to provide additional token specifications for Azure authentication.
#'
#' @return A data frame.
#'
#' @export
#'
load_billion_data_legacy <- function(billion = c("hep", "hpop", "uhc", "all"),
                                     mart_table = c("full_data", "raw_data", "projected_data", "unproj_data", "proj_data"),
                                     date_filter = "latest",
                                     na_rm = TRUE,
                                     format = c("csv", "streaming", "none"),
                                     ...) {
  requireNamespace("xmart4", quietly = TRUE)
  billion <- rlang::arg_match(billion)
  mart_table <- rlang::arg_match(mart_table)
  mart_match <- c(
    "full_data" = "FULL_BILLIONS",
    "raw_data" = "RAW_INDICATOR",
    "projected_data" = "PROJECTED_DATA",
    "unproj_data" = "RAW_UNPROJ_DATA",
    "proj_data" = "RAW_PROJ_DATA"
  )
  mart_table <- mart_match[mart_table]
  assert_date_filter(date_filter)
  format <- rlang::arg_match(format)

  df <- load_billion_table(mart_table, format, auth_type = "client", ...)
  df <- dplyr::rename_with(df, tolower)
  df <- filter_billion_inds(df, billion)
  df <- filter_billion_date(df, date_filter)
  df <- filter_billion_na(df, na_rm)
  df
}

#' @noRd
filter_billion_inds <- function(df, billion) {
  if (billion != "all") {
    inds <- billion_ind_codes(billion, include_covariates = TRUE, include_calculated = TRUE)
    df <- dplyr::filter(df, .data[["ind"]] %in% !!inds)
  }
  df
}

#' @noRd
filter_billion_date <- function(df, date_filter) {
  if (!is.null(date_filter)) {
    df <- dplyr::group_by(
      df,
      .data[["ind"]],
      .data[["iso3"]],
      .data[["year"]]
    )
    if (date_filter != "latest") {
      date_filter <- lubridate::as_date(date_filter)
      if (date_filter < min(df[["upload_date"]])) {
        warning("`date_filter` is before the first upload date of the Billions data, returning an empty data frame.",
          call. = FALSE
        )
      }
      df <- dplyr::filter(df, .data[["upload_date"]] <= !!date_filter)
    }
    df <- dplyr::filter(df, .data[["upload_date"]] == max(.data[["upload_date"]], -Inf))
    df <- dplyr::ungroup(df)
  }
  df
}

#' @noRd
filter_billion_na <- function(df, na_rm) {
  if (na_rm) {
    df <- dplyr::filter(df, !is.na(.data[["value"]]))
  }
  df
}

#' @noRd
assert_date_filter <- function(fltr) {
  if (!is.null(fltr)) {
    if (fltr != "latest") {
      if (!is.character(fltr) | length(fltr) > 1 | !stringr::str_detect(fltr, "[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}")) {
        stop("`date_filter` needs to be either 'latest', NULL, or a single hyphen delimited ISO 8601 date string (e.g. '1988-06-21').",
          call. = FALSE
        )
      }
    }
  }
}

#' @noRd
load_billion_table <- function(tbl, format, ...) {
  xmart4::xmart4_table(
    mart = "GPW13",
    table = tbl,
    col_types = readr::cols(
      ISO3 = readr::col_character(),
      YEAR = readr::col_double(),
      IND = readr::col_character(),
      UPLOAD_DATE = readr::col_date(format = ""),
      VALUE = readr::col_double(),
      LOW = readr::col_double(),
      HIGH = readr::col_double(),
      USE_DASH = readr::col_logical(),
      USE_CALC = readr::col_logical(),
      SOURCE = readr::col_character(),
      TYPE = readr::col_character(),
      TYPE_DETAIL = readr::col_character(),
      OTHER_DETAIL = readr::col_character(),
      UPLOAD_DETAIL = readr::col_character()
    ),
    format = format,
    ...
  )
}

#' Load miscellaneous data
#'
#' This function fetches and read data stored in the 3B/Bronze/misc/ folder in the
#' WHDH data lake.
#'
#' It automatically selects between `readr::read_csv()`, `arrow::read_parquet()`,
#' and `readxl::read_excel()` based on the file extension.
#'
#' @param file_path The path to the file inside the `3B/Bronze/misc` folder. File
#' paths must end with an extension (e.g., .csv)
#' @param ... Any additionally arguments to pass on to the appropriate `read_` function.
#'
#' @return a data frame
#' @export
load_misc_data <- function(file_path, ...) {
  f <- tempfile()

  whdh::download_from_data_lake(
    data_lake_name = "srhdteuwstdsa",
    source_path = paste("3B/Bronze/misc", file_path, sep = "/"),
    destination_path = f,
    silent = TRUE
  )

  ext <- stringr::str_match(file_path, "(.+)\\.(.+)")[, 3]

  output_df <- switch(ext,
    xls = readxl::read_xls(f, ...),
    xlsx = readxl::read_xlsx(f, ...),
    csv = readr::read_csv(f, show_col_types = FALSE, ...),
    parquet = arrow::read_parquet(f, ...)
  )

  output_df
}
