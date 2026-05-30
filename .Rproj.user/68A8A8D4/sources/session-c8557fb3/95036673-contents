# ============================================================
# RHC Wales Health Equity Project
# Script 02: Data Dictionary
# Author: Nina
# Date: June 2026
# Purpose: Document all datasets used in this project
# ============================================================

library(tidyverse)

# ── DATASET 1: WIMD 2019 Ranks ───────────────────────────────
cat("=== DATASET 1: WIMD 2019 Ranks ===\n")
cat("Source: Welsh Government\n")
cat("URL: https://gov.wales/welsh-index-multiple-deprivation-full-index-update-ranks-2019\n")
cat("Date downloaded: June 2026\n")
cat("Geographic level: LSOA (Lower Super Output Area)\n")
cat("Number of areas: 1,909 LSOAs in Wales\n")
cat("File: data/raw/wimd2019_ranks.ods\n\n")

cat("Columns:\n")
cat("LSOA code          - Unique identifier for each small area (e.g. W01000001)\n")
cat("LSOA name (Eng)    - Place name in English\n")
cat("Local Authority    - Which of 22 Welsh local authorities the LSOA belongs to\n")
cat("WIMD 2019          - Overall deprivation rank (1=most deprived, 1909=least)\n")
cat("Income             - Rank for income deprivation domain\n")
cat("Employment         - Rank for employment deprivation domain\n")
cat("Health             - Rank for health deprivation domain\n")
cat("Education          - Rank for education deprivation domain\n")
cat("Access to Services - Rank for access to services domain\n")
cat("Housing            - Rank for housing deprivation domain\n")
cat("Community Safety   - Rank for community safety domain\n")
cat("Physical Env       - Rank for physical environment domain\n\n")

# ── DATASET 2: WIMD 2019 Scores ──────────────────────────────
cat("=== DATASET 2: WIMD 2019 Scores ===\n")
cat("Source: Welsh Government\n")
cat("URL: https://gov.wales/welsh-index-multiple-deprivation-full-index-update-ranks-2019\n")
cat("Date downloaded: June 2026\n")
cat("Geographic level: LSOA\n")
cat("Number of areas: 1,909 LSOAs in Wales\n")
cat("File: data/raw/wimd2019_scores.ods\n\n")

cat("Note: Scores are continuous values used to calculate ranks.\n")
cat("Higher scores = greater deprivation in that domain.\n")
cat("Scores should NOT be aggregated to larger geographies by averaging.\n\n")

# ── Quick summary statistics ──────────────────────────────────
cat("=== QUICK DATA CHECKS ===\n")
cat("Ranks rows:", nrow(wimd_ranks), "\n")
cat("Scores rows:", nrow(wimd_scores), "\n")
cat("Missing values in ranks:\n")
print(colSums(is.na(wimd_ranks)))

message("Data dictionary complete!")