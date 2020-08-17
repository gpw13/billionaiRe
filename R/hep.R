#' @export
hep_index <- function(df,
                      prevent = "prevent",
                      prepare = "prepare",
                      detect_respond = "detect_respond") {
  assert_strings(iso3, year, prevent, prepare, detect_respond)
  assert_columns(iso3, year, prevent, prepare, detect_respond)
  assert_numeric(prevent, prepare, detect_respond)
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hepi = mean(c_across(all_of(c(prevent, prepare, detect_respond))), na.rm = TRUE))
}

#' @export
prevent <- function(df,
                    iso3 = "iso3",
                    year = "year",
                    vaccinated = "vaccinated",
                    target_pop = "target_pop") {
  assert_strings(iso3, year, vaccinated, target_pop)
  assert_columns(df, iso3, year, vaccinated, target_pop)
  assert_numeric(df, vaccinated, target_pop)
  df %>%
    dplyr::group_by(.data[[iso3]], .data[[year]]) %>%
    dplyr::summarize(prevent = sum(.data[[vaccinated]], na.rm = T) / sum(.data[[target_pop]], na.rm = T))
}

#' @export
prepare <- function(df,
                    iso3 = "iso3",
                    year = "year",
                    ...) {
  args <- c(...)
  assert_columns(df, args)
  assert_numeric(df, args)
  df %>%
    dplyr::group_by(.data[[iso3]], .data[[year]]) %>%
    dplyr::summarize(prepare = mean(dplyr::all_of(args), na.rm = T))
}

#' @export
detect_respond <- function(df,
                           iso3 = "iso3",
                           year = "year",
                           detect = "detect",
                           notify = "notify",
                           respond = "respond") {
  assert_strings(iso3, year, detect, notify, respond)
  assert_columns(df, iso3, year, detect, notify, respond)
  assert_numeric(df, detect, notify, respond)
  df %>%
    dplyr::mutate(detect_level = timeliness_levels(.data[[detect]]),
                  notify_level = timeliness_levels(.data[[notify]]),
                  respond_level = timeliness_levels(.data[[respond]]),
                  event_value = rowMeans(select(.,
                                                detect_level,
                                                notify_level,
                                                respond_level))) %>%
    dplyr::group_by(.data[[iso3]], .data[[year]]) %>%
    dplyr::summarize(year_value = mean(event_value),
                     .groups = "drop_last") %>%
    dplyr::arrange(.data[[iso3]], .data[[year]]) %>%
    dplyr::mutate(detect_respond = zoo::rollmean(year_value,
                                                 k = 5))
}

#' @noRd
timeliness_levels <- function(x) {
  dplyr::case_when(
    x <= 1 ~ 5,
    x <= 7 ~ 4,
    x <= 14 ~ 3,
    x > 14 ~ 2,
    is.na(x) ~ 1
  )
}
