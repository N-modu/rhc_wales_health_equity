# =============================================================================
# Script:      06_choropleth_maps.R
# Project:     Rural Health Compass — NHS Wales Health Equity Analysis
# Author:      Nina Akporiaye
# Supervisor:  Dr Veronika Rasic
# Date:        June 2026
# Purpose:     Choropleth maps of broadband exclusion and deprivation
#              across Ceredigion, Powys, Pembrokeshire and Swansea
# Input:       wales_map_data (sf object, created in 04_load_ofcom.R)
# Output:      Two choropleth maps for Assessment 3 presentation
# =============================================================================

library(tidyverse)
library(tmap)
library(sf)

# Remove ghost column if present
if("...13" %in% names(wales_map_data)) {
  wales_map_data <- wales_map_data |> select(-`...13`)
}

# Filter to four study counties only
study_map <- wales_map_data |>
  filter(`Local Authority name (Eng)` %in% 
           c("Ceredigion", "Powys", "Pembrokeshire", "Swansea"))

# Verify
cat("Study area LSOAs:", nrow(study_map), "\n")
# =============================================================================
# Section 1: Map 1 — Broadband Exclusion (pct_below_30mbps)
# =============================================================================

tmap_mode("plot")

map1 <- tm_shape(study_map) +
  tm_polygons(
    fill = "pct_below_30mbps",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      values = "Reds"
    ),
    fill.legend = tm_legend(title = "% Premises Below 30mbps"),
    col = "grey60",
    lwd = 0.3
  ) +
  tm_title("Digital Exclusion in Rural Wales") +
  tm_compass(position = c("left", "bottom"), size = 1.5) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_layout(legend.outside = TRUE, frame = FALSE)

# Display map
map1
# =============================================================================
# Section 2: Map 2 — Overall Deprivation (WIMD 2025 Rank)
# =============================================================================

map2 <- tm_shape(study_map) +
  tm_polygons(
    fill = "WIMD 2025",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      values = "Blues"
    ),
    fill.legend = tm_legend(title = "WIMD 2025 Rank\n(1 = most deprived)"),
    col = "grey60",
    lwd = 0.3
  ) +
  tm_title("Deprivation in Rural Wales (WIMD 2025)") +
  tm_compass(position = c("left", "bottom"), size = 1.5) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_layout(legend.outside = TRUE, frame = FALSE)

# Display map
map2
# =============================================================================
# Section 3: Add County Labels to Maps
# =============================================================================

# Create county centroids for labels
county_labels <- study_map |>
  group_by(`Local Authority name (Eng)`) |>
  summarise(geometry = st_union(geometry)) |>
  st_centroid()

# Map 1 with county labels
map1_labelled <- tm_shape(study_map) +
  tm_polygons(
    fill = "pct_below_30mbps",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      values = "Reds"
    ),
    fill.legend = tm_legend(title = "% Premises Below 30mbps"),
    col = "grey60",
    lwd = 0.3
  ) +
  tm_shape(county_labels) +
  tm_text("Local Authority name (Eng)", size = 0.7, fontface = "bold") +
  tm_title("Digital Exclusion in Rural Wales") +
  tm_compass(position = c("left", "bottom"), size = 1.5) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_layout(legend.outside = TRUE, frame = FALSE)

# Map 2 with county labels
map2_labelled <- tm_shape(study_map) +
  tm_polygons(
    fill = "WIMD 2025",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      values = "Blues"
    ),
    fill.legend = tm_legend(title = "WIMD 2025 Rank\n(1 = most deprived)"),
    col = "grey60",
    lwd = 0.3
  ) +
  tm_shape(county_labels) +
  tm_text("Local Authority name (Eng)", size = 0.7, fontface = "bold") +
  tm_title("Deprivation in Rural Wales (WIMD 2025)") +
  tm_compass(position = c("left", "bottom"), size = 1.5) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_layout(legend.outside = TRUE, frame = FALSE)

# Display both
map1_labelled
map2_labelled
# =============================================================================
# Section 4: Export Maps
# =============================================================================

tmap_save(map1_labelled, 
          filename = "outputs/map1_broadband_exclusion.png",
          width = 20, 
          height = 15, 
          units = "cm", 
          dpi = 300)

tmap_save(map2_labelled, 
          filename = "outputs/map2_wimd_deprivation.png",
          width = 20, 
          height = 15, 
          units = "cm", 
          dpi = 300)

cat("Maps saved to outputs folder\n")

map1