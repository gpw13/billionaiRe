
#' Non-Communicable Disease (NCD) indicator update
#'
#' This edits the core `indicator_df` .csv file to update the GHO codes and meta-data
#' for the UHC fasting plasma glucose (`fpg`) and blood pressure (`bp`) indicators.
#'
#' This function will overwrite the existing .csv file in the `data-raw/` directory.
#'
#' @return NULL
#'
uhc_ncd_update_2022 <- function() {

  indicator_df <- read_csv("indicator_df.csv")



  updated_indicator_df <- indicator_df %>%
    mutate(gho_code = if_else(ind == "fpg", "NCD_GLUC_04", gho_code),
           medium_name = if_else(ind == "fpg", "Raised fasting blood glucose (age-standardized estimate)", medium_name),
           unit_raw = if_else(ind == "fpg", ">=7.0 mmol/L", unit_raw),
           gho_query = if_else(ind == "fpg", "$filter=SpatialDimType eq 'COUNTRY' and  Dim1 eq 'BTSX'", gho_query)) %>%
    mutate(gho_code = if_else(ind == "bp", "NCD_HYP_TREATMENT_A", gho_code),
           medium_name = if_else(ind == "bp", "Prevalence of treatment (taking medicine) for hypertension among adults aged 30-79 with hypertension
", medium_name),
           transformed_name = if_else(ind == "bp", "Hypertension treatment", transformed_name),
           gho_query = if_else(ind == "bp", "$filter=SpatialDimType eq 'COUNTRY' and  Dim1 eq 'BTSX'", gho_query))

  updated_indicator_df %>%
    write_csv("indicator_df.csv")

}
