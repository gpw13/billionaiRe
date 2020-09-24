#' Dataset of indicators used within the Billions calculations.
#'
#' A dataset containing various indicator codes and their uses within the GPW13
#' Billions. Its internal use in the package ensures that switching storage locations
#' for data (between GHO and xMart) does not impact the data pipeline, all that is
#' needed is to update this data frame.
#'
#' This file will eventually be stored within the GPW13 xMart4 instance, but until
#' a more stable version is realized, it is being managed within the billionaiRe
#' R package.
#'
#' @format A data frame with `r nrow(indicator_df)` rows and `r ncol(indicator_df)` variables:
#' \describe{
#'   \item{unique_id}{Unique ID for each indicator}
#'   \item{dashboard_id}{Dashboard ID used within the GPW13 xMart4 instance}
#'   \item{year}{Year, currently 1970 - 2030}
#'   \item{total}{Total population}
#'   \item{under_1}{Population under the age of 1}
#'   \item{1_4}{Population aged 1 - 4}
#'   \item{5_9}{Population aged 5 - 9}
#'   \item{10_14}{Population aged 10 - 14}
#'   \item{15_19}{Population aged 15 - 19}
#'   \item{20_24}{Population aged 20 - 24}
#'   \item{25_29}{Population aged 25 - 29}
#'   \item{30_34}{Population aged 30 - 34}
#'   \item{35_39}{Population aged 35 - 39}
#'   \item{40_44}{Population aged 40 - 44}
#'   \item{45_49}{Population aged 45 - 49}
#'   \item{50_54}{Population aged 50 - 54}
#'   \item{55_59}{Population aged 55 - 59}
#'   \item{60_64}{Population aged 60 - 64}
#'   \item{65_69}{Population aged 65 - 69}
#'   \item{70_74}{Population aged 70 - 74}
#'   \item{75_79}{Population aged 75 - 79}
#'   \item{80_84}{Population aged 80 - 84}
#'   \item{85_89}{Population aged 85 - 89}
#'   \item{90_94}{Population aged 90 - 94}
#'   \item{95_99}{Population aged 95 - 99}
#'   \item{over_99}{Population over the age of 99}
#'   \item{under_5}{Population under the age of 5}
#'   \item{btwn_5_14}{Population aged 5 - 14}
#'   \item{btwn_5_19}{Population aged 5 - 19}
#'   \item{over_14}{Population over the age of 14}
#'   \item{under_20}{Population under the age of 20}
#'   \item{over_19}{Population over the age of 19}
#'   \item{urban_percent}{Proportion of the population in urban areas, mid-year 2018}
#' }
#' @inherit get_population source
"wpp_population"
