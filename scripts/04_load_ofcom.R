# =============================================================
# Script 04 — Load and Filter Ofcom Connected Nations Wales
# Rural Health Compass Placement 2026
# Nina Akporiaye | University of Aberdeen
# Licence: CC-BY 4.0
# =============================================================
library(tidyverse)    # filter, mutate, summarise, joins, ggplot2
library(data.table)   # fast loading of large Ofcom CSV
library(sf)           # spatial data handling for LSOA shapefiles
library(tmap)         # choropleth maps
# Load full UK-wide Ofcom residential broadband file
ofcom_uk_raw <- fread("data/raw/202407_fixed_oa_res_coverage_r01.csv")
# Check it loaded correctly
nrow(ofcom_uk_raw)
ncol(ofcom_uk_raw)
names(ofcom_uk_raw)
# Filter to Welsh Output Areas only (codes starting with "W")
ofcom_wales <- ofcom_uk_raw |>
  filter(str_starts(output_area, "W"))

# Check the result
nrow(ofcom_wales)

# Confirm ONLY Welsh codes came through
ofcom_wales |>
  count(str_sub(output_area, 1, 1))
# Check for missing values in the OA code column
sum(is.na(ofcom_wales$output_area))
# Save the filtered Wales-only file
write_csv(ofcom_wales, "data/processed/ofcom_wales_fixed_broadband.csv")
# Confirm it saved
file.exists("data/processed/ofcom_wales_fixed_broadband.csv")
# Clean column names
ofcom_wales_clean <- ofcom_wales |>
  rename(
    oa_code                    = `output_area`,
    all_premises               = `All Premises`,
    matched_premises           = `All Matched Premises`,
    pct_sfbb                   = `SFBB availability (% premises)`,
    pct_ufbb_100               = `UFBB (100Mbit/s) availability (% premises)`,
    pct_ufbb                   = `UFBB availability (% premises)`,
    pct_below_2mbps            = `% of premises unable to receive 2Mbit/s`,
    pct_below_5mbps            = `% of premises unable to receive 5Mbit/s`,
    pct_below_10mbps           = `% of premises unable to receive 10Mbit/s`,
    pct_below_30mbps           = `% of premises unable to receive 30Mbit/s`,
    pct_gigabit                = `Gigabit availability (% premises)`,
    pct_below_uso              = `% of premises below the USO`,
    pct_nga                    = `% of premises with NGA`,
    pct_decent_fwa             = `% of premises able to receive decent broadband from FWA`,
    n_sfbb                     = `Number of premises with SFBB availability`,
    n_ufbb_100                 = `Number of premises with UFBB (100Mbit/s) availability`,
    n_ufbb                     = `Number of premises with UFBB availability`,
    n_below_2mbps              = `Number of premises unable to receive 2Mbit/s`,
    n_below_5mbps              = `Number of premises unable to receive 5Mbit/s`,
    n_below_10mbps             = `Number of premises unable to receive 10Mbit/s`,
    n_below_30mbps             = `Number of premises unable to receive 30Mbit/s`,
    n_gigabit                  = `Number of premises with Gigabit availability`,
    n_below_uso                = `Number of premises below the USO`,
    n_nga                      = `Number of premises with NGA`,
    n_decent_fwa               = `Number of premises able to receive decent broadband from FWA`,
    pct_30_to_300mbps          = `% of premises with 30<300Mbit/s download speed`,
    pct_above_300mbps          = `% of premises with >=300Mbit/s download speed`,
    pct_0_to_2mbps             = `% of premises with 0<2Mbit/s download speed`,
    pct_2_to_5mbps             = `% of premises with 2<5Mbit/s download speed`,
    pct_5_to_10mbps            = `% of premises with 5<10Mbit/s download speed`,
    pct_10_to_30mbps           = `% of premises with 10<30Mbit/s download speed`,
    n_30_to_300mbps            = `Number of premises with 30<300Mbit/s download speed`,
    n_above_300mbps            = `Number of premises with >=300Mbit/s download speed`,
    n_0_to_2mbps               = `Number of premises with 0<2Mbit/s download speed`,
    n_2_to_5mbps               = `Number of premises with 2<5Mbit/s download speed`,
    n_5_to_10mbps              = `Number of premises with 5<10Mbit/s download speed`,
    n_10_to_30mbps             = `Number of premises with 10<30Mbit/s download speed`
  )

# Confirm the new names
names(ofcom_wales_clean)
# Save cleaned file
write_csv(ofcom_wales_clean, "data/processed/ofcom_wales_fixed_broadband.csv")

# Final sense check
glimpse(ofcom_wales_clean)