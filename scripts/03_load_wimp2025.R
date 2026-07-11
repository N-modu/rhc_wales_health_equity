# ============================================================
# Project: Rural Health Equity in Wales
# Script 03: Load and Inspect WIMD 2025 Data
# Author: Nina Akporiaye, University of Aberdeen
# Organisation: Rural Health Compass (RHC)
# Supervisor: Dr Veronika Rasic
# Date: June 2026
# Licence: CC BY 4.0
# https://creativecommons.org/licenses/by/4.0/
# ============================================================
# load packages
library(tidyverse)
library(readODS)

# ── Step 1: Inspect sheet names in each file ─────────────────
ods_sheets("data/raw/wimd2025_ranks.ods")
ods_sheets("data/raw/wimd2025_scores.ods")
ods_sheets("data/raw/wimd2025_deep_rooted.ods")
ods_sheets("data/raw/wimd2025_postcode_lookup.ods")
# ── Step 2: Load WIMD 2025 ranks ─────────────────────────────
wimd2025_ranks <- read_ods(
  "data/raw/wimd2025_ranks.ods",
  sheet = "WIMD_2025_ranks",
  skip = 2
)

# ── Step 3: Load WIMD 2025 scores ────────────────────────────
wimd2025_scores <- read_ods(
  "data/raw/wimd2025_scores.ods",
  sheet = "Data",
  skip = 2
)

# ── Step 4: Load deep-rooted deprivation ─────────────────────
wimd2025_deep_rooted <- read_ods(
  "data/raw/wimd2025_deep_rooted.ods",
  sheet = "Data",
  skip = 2
)

# ── Step 5: Load postcode lookup ─────────────────────────────
wimd2025_postcode_lookup <- read_ods(
  "data/raw/wimd2025_postcode_lookup.ods",
  sheet = "Postcode_to_LSOA_to_WIMD_rank",
  skip = 2
)

# ── Step 6: Inspect each dataset ─────────────────────────────
glimpse(wimd2025_ranks)
glimpse(wimd2025_scores)
glimpse(wimd2025_deep_rooted)
glimpse(wimd2025_postcode_lookup)
# ── Load deep-rooted deprivation ─────────────────────────────
wimd2025_deep_rooted <- read_ods(
  "data/raw/wimd2025_deep_rooted.ods",
  sheet = "Data",
  skip = 4
)

# ── Check it now ─────────────────────────────────────────────
nrow(wimd2025_deep_rooted)
names(wimd2025_deep_rooted)
head(wimd2025_deep_rooted, 5)
# ── Load WIMD 2025 scores (corrected skip) ───────────────────
wimd2025_scores <- read_ods(
  "data/raw/wimd2025_scores.ods",
  sheet = "Data",
  skip = 3
)

# ── Validate ─────────────────────────────────────────────────
nrow(wimd2025_scores)
names(wimd2025_scores)
head(wimd2025_scores, 5)
wimd2025_ranks <- wimd2025_ranks |> 
  select(-`...13`)

ncol(wimd2025_ranks)
message("WIMD 2025 data loaded successfully!")
# ── Load Ofcom residential coverage data ─────────────────────
ofcom_coverage <- read_csv(
  "data/raw/202407_fixed_oa_res_coverage_r01.csv"
)

# ── Inspect ───────────────────────────────────────────────────
nrow(ofcom_coverage)
names(ofcom_coverage)
head(ofcom_coverage, 5)