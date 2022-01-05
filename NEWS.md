# billionaiRe 0.6.1
* Add recycle_data functions to efficiently recycle data. See
vignette("scenarios", "billionaiRe") for an overview of the functionality and 
logic behind data recycling.


# billionaiRe 0.5.2
* Added further definitions to `indicator_df.xlsx`
* Renamed column `analysis_code` by `ind` of `indicator_df.xlsx`. 
* Removed column `ind` of `indicator_df.xlsx`.
* Modified code to use `ind` instead of `analysis_code`.

# billionaiRe 0.5.1
* Combined `indicator_df.xlsx` and `indicator_order.xlsx` into one file.
* Updated dashboard_id to better align with current dashboard backend.
* Changes to DESCRIPTION reflecting `wppdistro` transfer to WorldHealthOrganization
GitHub org.

# billionaiRe 0.5.0
* Added `export_all_countries_summaries_xls` to export all country summary Excel
files for all billions or a specific billion.
* Added `export_country_summary_xls` to export one country summary Excel file for
all billions or a specific billion.
* Added support functions for the two preceding functions.
* Modified `indicator_df`, `indicator_df.xlsx`, and `indicator_df.rda` to include
additional information about indicators definition, names, etc.
* Added `indicator_order.xlsx` to be joined with `indicator_df`.
* Added `affected_pathogens`, `affected_pathogens.xlsx`, and `affected_pathogens.rda`
to indicate by which pathogens countries are affected.
* Added `inst/extdata/country_summary_template.xlsx` to store the Excel template
for country profiles.
* Modified all default values to 2025 instead of 2023.
* Added `pre-commit` hooks to style and ensure consistency in code on commit. See
[pre-commit repository](https://github.com/lorenzwalthert/precommit) for more
information.

# billionaiRe 0.4.4
* Added `calculate_contribution_sums` with the ability to calculate regional or
global sums for HEP, HPOP, or UHC billions for a given year.

# billionaiRe 0.4.3
* Added `wrangled_rural_urban_gho_data()` with functionality to supplant `wrangle_gho_data()`
for indicators where the data has an additional RESIDENCEAREATYPE dimension with possible values
of `TOTL`, `URB` and `RUR`.
* Added `xmart_cols()`, `has_xmart_cols()`, `save_wrangled_output()` utility functions
for easier wrangling of data updates.
* Full Billions data table added to `load_billion_data()`, allowing user to directly
download latest Billions data and resultant calculations.

# billionaiRe 0.4.2
* Updated `indicator_df.xlsx` and `indicator_df.rda` with the GHO code for 
polio_routine(WHS4_544).
* All Billions calculations now return `contribution_percent` as well as `contribution`
values, for use in the dashboard and other presentations.
* HPOP Billion calculation returns total Billion without double counting correction,
as well as standard.

# billionaiRe 0.4.1
* Updated `wrangle_who_data` function with additional assertions and warnings
* Updated `indicator_df.xlsx` and `indicator_df.rda` with the GHO code for
measles_routine (WHS8_110)

# billionaiRe 0.4.0
* Updated all three Billions functions to produce data in long format for immediate
upload to xMart.
* Developed all three Billions to be calculated for various scenarios, defined in
long format through a `scenario` column or in wide format by having multiple
`value` columns.
* Expanded HEP Billion to allow for the use of ebola, COVID, and measles campaign
data within the Prevent indicator.

# billionaiRe 0.3.0
* Updated data loading functions so that all 3 xMart databases are available: unprojected
data, projected data, and final input data for the Billions.
* Added in `untransform_hpop_data()` and `untransform_uhc_data()` so that transformed
data could be converted back into the original domain of the raw data.
* Allow multiple columns to be transformed or untransformed at once using the
`transform_...` and `untransform_...` functions for UHC and HPOP Billions.
* Clear error messages added if there are non-distinct rows in `df` for `ind`, `iso3`,
and `year`.

# billionaiRe 0.2.1

* Changed `transform_glucose()` to have a domain of 5.1 to 7.4, previously one of
5.1 to 7.4.
* Fixed pop_links to ensure that IPV double counting correction was correct

# billionaiRe 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Implemented HEP Billions into pipeline.

# billionaiRe 0.1.0

* Initial release of package. UHC and HPOP Billions available for calculation through the
billionaiRe API.
