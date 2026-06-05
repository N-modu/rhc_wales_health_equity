# ============================================================
# Project: Rural Health Equity in Wales
# Script 01: Load & Inspect WIMD Data
# Author: Nina Akporiaye, University of Aberdeen
# Organisation: Rural Health Compass (RHC)
# Supervisor: Dr Veronika Rasic
# Date: June 2026
# Licence: CC BY 4.0
# https://creativecommons.org/licenses/by/4.0/
# ============================================================
library(tidyverse)
library(readODS)

# ── Load ranks data ──────────────────────────────────────────
wimd_ranks <- read_ods("data/raw/wimd2019_ranks.ods", 
                       sheet = "WIMD_2019_ranks", 
                       skip = 2)
# ── Load scores data ─────────────────────────────────────────
wimd_scores <- read_ods("data/raw/wimd2019_scores.ods", 
                        sheet = "Data", 
                        skip = 3)
# ── Inspect ranks ────────────────────────────────────────────
glimpse(wimd_ranks)
head(wimd_ranks, 10)
names(wimd_ranks)
nrow(wimd_ranks)

# ── Inspect scores ───────────────────────────────────────────
glimpse(wimd_scores)
head(wimd_scores, 10)
names(wimd_scores)

message("Data loaded successfully!")

