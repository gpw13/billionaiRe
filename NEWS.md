# billionaiRe 0.6.8
* Adds support of `covid_shock`
* `make_default_scenario()` now doesn't require to have `default_scenario` in `scenario` column.

# billionaiRe 0.6.7
* Adds `upload_billion_data` function for easy uploads to the 3B data lake in the
  World Health Data Hub (WHDH).
* Adds `assert_timestamp` and `assert_class` utilities for checks.
* Removed `testit` dependencies.

# billionaiRe 0.6.6
* Updates to `load_billion_data` functions incorporating the `whdh::download_data_asset`
function and the use of a `version` argument to replace the `date_filter` placeholder.
* `load_billion_data` now has an `experiment` argument, which replaces `sandbox`.

# billionaiRe 0.6.5
* Replace names of scenarios with more informative names:
  - `none` becomes `routine`
  - `tp` becomes `reference_infilling`
* add scenario and scenario_details to the wrangled columns in wrangling functions
and xmart_cols as they will be expected to be present from now on.
* Adding COVID-19 scenarios.

# billionaiRe 0.6.4
* Adding Sustainable Development Goals acceleration scenarios.

# billionaiRe 0.6.3
* Corrected problems with `fh` where the indicator was not transformed correctly. See [issue #35]{https://github.com/gpw13/billionaiRe/issues/35} for more details.
* Updated testing data frames accordingly.

# billionaiRe 0.6.2
* Add scenario functions to billionaiRe. Those allow to calculate different
scenarios based either in the application of one scenario to all indicators or
through special-case scenarios where calculations might differ for all indicators.
See the vignette("scenario", "billionaiRe")
* Bring improvements to data_recycling

# billionaiRe 0.6.1
* Add recycle_data functions to efficiently recycle data. See
vignette("scenarios", "billionaiRe") for an overview of the functionality and 
logic behind data recycling.

# billionaiRe 0.6.0
Various updates to facilitate migration of the Triple Billion data pipeline into the WHDH
* Updated `load_billion_data` to now pull data from the WHDH data lake by default
  and optionally set xMart as the `data_source`. Using xMart currently redirects the user
  to the legacy version of `load_billion_data`, while the xMart functionality is built.
* Added `get_whdh_path` for easy generation of download/upload paths for the WHDH data lake
* Updated `save_wrangled_output` to work with parquet format.
* Added `save_gho_backup_to_whdh`, making it easier to backup snapshots of GHO data
to the WHDH data lake.
* Updated `billion_ind_codes` with a toggle for subindicators (like `water_rural`,
  `water_urban` and the eSpar categories and sub-categories).
* Added `get_ind_billion` for determining which billion a given indicato belongs to.
* Updated `xmart_cols` to return different sets of columns based for different types of data.
* Added new assertions: `assert_type`, `assert_fileext`, `assert_unique_vector`, 
  `assert_length`, `assert_min_length`, `assert_same_length`, `assert_equals`, 
  and `assert_x_in_y`.
* Updated `assert_arg_exists` to have custom error messages.
* Rearranged `utils_asserts.R` to categorise the different assertions.

# billionaiRe 0.5.3
* Export functions introduced in version 0.5.0 were moved the code to
a new package: [rapporteur](https://github.com/gpw13/rapporteur). This reduces
the size of the billionaiRe package and maintain a better logic in the package
environment.
* Introduce basic testing (input-output) of the package.

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
