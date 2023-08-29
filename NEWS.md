# findSVI (development version)

# findSVI 0.1.2

* Wrapping data retrieval and SVI calculation, `find_svi()` supports multiple entries of year-state pairs.

* `get_census_data()` (and `find_svi()`) uses zcta-state crosswalks to support state-specific ZCTA-level data retrieval from 2019-2021. (This functionality is supported by `tidycensus::get_acs()` until 2019 as Census Bureau no longer makes ZCTA shapefiles available by state.)

* `get_svi()` replicates results from CDC/ATSDR SVI database by matching number of decimals (#12).

* More user-friendly CLI error messages in the functions (#3, #4, #7).

