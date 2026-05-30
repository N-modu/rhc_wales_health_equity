# ============================================================
# RHC Wales Health Equity Project
# Script 00: Setup & Package Check
# Author: Nina
# Date: June 2026
# Purpose: Load all packages and confirm setup is working
# ============================================================

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(readxl)
library(viridis)
library(janitor)

# Confirm working directory is correct
getwd()

# Confirm folder structure exists
dir.exists("data/raw")
dir.exists("data/clean")
dir.exists("scripts")
dir.exists("outputs/maps")
dir.exists("outputs/charts")
dir.exists("docs")

message("Setup complete — all packages loaded and folders confirmed!")
