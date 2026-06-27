# =============================================================================
# Script:      05_stratification_check.R
# Project:     Rural Health Compass — NHS Wales Health Equity Analysis
# Author:      Nina Akporiaye
# Supervisor:  Dr Veronika Rasic
# Date:        June 2026
# Purpose:     Stratification checks prior to main Spearman correlation analysis
#              for RQ3: Does digital exclusion associate with higher deprivation
#              in rural Welsh LSOAs?
# Input:       ofcom_wimd (master dataset, 1,917 LSOAs, created in 01_load_data.R)
# Output:      Stratified subsets, row count verification, rural/urban flags
# =============================================================================
# =============================================================================
# Section 1: Load Libraries
# =============================================================================
library(tidyverse)

# =============================================================================
# Section 2: Verify Master Dataset
# =============================================================================
# Confirm dataset dimensions
nrow(ofcom_wimd_ruc)   # Expect 1917
ncol(ofcom_wimd_ruc)   # Expect 25
names(ofcom_wimd_ruc)  # Confirm variable names

# =============================================================================
# Section 3: Filter by Local Authority
# =============================================================================
ceredigion <- ofcom_wimd_ruc |> 
  filter(`Local Authority name (Eng)` == "Ceredigion")

powys <- ofcom_wimd_ruc |> 
  filter(`Local Authority name (Eng)` == "Powys")

pembrokeshire <- ofcom_wimd_ruc |> 
  filter(`Local Authority name (Eng)` == "Pembrokeshire")

swansea <- ofcom_wimd_ruc |> 
  filter(`Local Authority name (Eng)` == "Swansea")

# Verify row counts
cat("Ceredigion LSOAs:", nrow(ceredigion), "\n")
cat("Powys LSOAs:", nrow(powys), "\n")
cat("Pembrokeshire LSOAs:", nrow(pembrokeshire), "\n")
cat("Swansea LSOAs:", nrow(swansea), "\n")
cat("Total:", nrow(ceredigion) + nrow(powys) + 
      nrow(pembrokeshire) + nrow(swansea), "\n")
# =============================================================================
# Section 4: Remove Ghost Column
# =============================================================================
# Remove empty artefact column if it exists
if("...13" %in% names(ofcom_wimd_ruc)) {
  ofcom_wimd_ruc <- ofcom_wimd_ruc |> select(-`...13`)
  cat("Ghost column removed\n")
} else {
  cat("No ghost column found - dataset already clean\n")
}

# Confirm column count
ncol(ofcom_wimd_ruc)
names(ofcom_wimd_ruc)
# =============================================================================
# Section 5: Rural/Urban Distribution by County
# =============================================================================
# Check rural/urban breakdown for each county
cat("\n--- Ceredigion ---\n")
table(ceredigion$rural_urban)

cat("\n--- Powys ---\n")
table(powys$rural_urban)

cat("\n--- Pembrokeshire ---\n")
table(pembrokeshire$rural_urban)

cat("\n--- Swansea ---\n")
table(swansea$rural_urban)
# =============================================================================
# Section 6: Create Pooled Rural and Urban Groups
# =============================================================================

# Combine all four county subsets
all_counties <- bind_rows(ceredigion, powys, pembrokeshire, swansea)

# Create pooled rural group
# Excludes Swansea rural LSOAs — Swansea serves as urban comparator only
rural_pooled <- all_counties |> 
  filter(rural_urban == "Rural" & 
           `Local Authority name (Eng)` != "Swansea")

# Create pooled urban group
# Includes urban LSOAs from all four counties
urban_pooled <- all_counties |> 
  filter(rural_urban == "Urban")

# Verify counts
cat("Rural pooled LSOAs:", nrow(rural_pooled), "\n")
cat("Urban pooled LSOAs:", nrow(urban_pooled), "\n")
cat("Total:", nrow(rural_pooled) + nrow(urban_pooled), "\n")
# =============================================================================
# Section 7: Spearman Rank Correlation
# =============================================================================

# Rural pooled correlation
rural_cor <- cor.test(rural_pooled$pct_below_30mbps,
                      rural_pooled$`WIMD 2025`,
                      method = "spearman",
                      exact = FALSE)

# Urban pooled correlation
urban_cor <- cor.test(urban_pooled$pct_below_30mbps,
                      urban_pooled$`WIMD 2025`,
                      method = "spearman",
                      exact = FALSE)

# Print results
cat("\n--- Rural Pooled Spearman Correlation ---\n")
print(rural_cor)

cat("\n--- Urban Pooled Spearman Correlation ---\n")
print(urban_cor)
# =============================================================================
# Section 8: Descriptive Comparison — Broadband Exclusion Rural vs Urban
# =============================================================================

# Mean broadband exclusion by group
cat("\n--- Broadband Exclusion Summary ---\n")
cat("Rural mean pct_below_30mbps:", 
    round(mean(rural_pooled$pct_below_30mbps, na.rm=TRUE), 2), "\n")
cat("Urban mean pct_below_30mbps:", 
    round(mean(urban_pooled$pct_below_30mbps, na.rm=TRUE), 2), "\n")

# Median broadband exclusion by group
cat("\nRural median pct_below_30mbps:", 
    round(median(rural_pooled$pct_below_30mbps, na.rm=TRUE), 2), "\n")
cat("Urban median pct_below_30mbps:", 
    round(median(urban_pooled$pct_below_30mbps, na.rm=TRUE), 2), "\n")

# Range
cat("\nRural range pct_below_30mbps:", 
    round(range(rural_pooled$pct_below_30mbps, na.rm=TRUE), 2), "\n")
cat("Urban range pct_below_30mbps:", 
    round(range(urban_pooled$pct_below_30mbps, na.rm=TRUE), 2), "\n")

# Mean WIMD rank by group
cat("\n--- Deprivation Summary (WIMD 2025 Rank) ---\n")
cat("Rural mean WIMD rank:", 
    round(mean(rural_pooled$`WIMD 2025`, na.rm=TRUE), 0), "\n")
cat("Urban mean WIMD rank:", 
    round(mean(urban_pooled$`WIMD 2025`, na.rm=TRUE), 0), "\n")

