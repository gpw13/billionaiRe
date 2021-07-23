# billionaiRe 0.4.3
* Added `wrangled_rural_urban_gho_data()` with functionality to supplant `wrangle_gho_data()`
for indicators where the data has an additional RESIDENCEAREATYPE dimension with possible values
of `TOTL`, `URB` and `RUR`.
* Added `xmart_cols()`, `has_xmart_cols()`, `save_wrangled_output()` utility functions
for easier wrangling of data updates.

# billionaiRe 0.4.2
* Updated `indicator_df.xlsx` and `indicator_df.rda` with the GHO code for 
polio_routine(WHS4_544).

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

