# ============================================================
# 09_domain_divergence_maps.R
# Two side-by-side choropleths: Access to Services vs Health domain
# National WIMD rank, diverging scale centred on Wales median (959)
#
# ASSUMPTIONS TO VERIFY BEFORE TRUSTING OUTPUT:
# - join key column matches between geojson and CSV (LSOA21CD assumed)
# - geojson CRS is known (check st_crs() below; reproject if not EPSG:4326)
# - "Health" and "Access to Services" columns are national ranks (1-1917),
#   confirmed rank 1 = most deprived (verified 2026-07-04)
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)  # for side-by-side layout

# ---- LOAD ----------------------------------------------------------
boundaries <- st_read("data/raw/lsoa_boundaries_2021.geojson")
cat("CRS check:\n"); print(st_crs(boundaries))
# If not EPSG:4326 or 27700, reproject explicitly:
# boundaries <- st_transform(boundaries, 27700)

master <- read_csv("data/processed/master_analytical_dataset.csv", show_col_types = FALSE)
# CONFIRM this join key exists in BOTH boundaries and master before running.
cat("Boundaries join col candidates:\n"); print(names(boundaries))
cat("Master join col: LSOA21CD present?", "LSOA21CD" %in% names(master), "\n")

rural_counties <- c("Powys", "Ceredigion", "Pembrokeshire")
rural_master <- master %>%
  filter(`Local Authority name (Eng)` %in% rural_counties, rural_urban == "Rural")

rural_geo <- boundaries %>%
  inner_join(rural_master, by = "LSOA21CD")

# EXPECTATION CHECK — must equal 157. If not, the join key or filter is wrong.
cat("Rural LSOAs successfully joined to geometry:", nrow(rural_geo), "(expect 157)\n")
stopifnot(nrow(rural_geo) == 157)

# ---- DIVERGING SCALE, CENTRED ON NATIONAL MEDIAN (959) --------------
national_median <- 959

make_map <- function(data, domain_col, title) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[domain_col]]), color = "grey40", linewidth = 0.1) +
    scale_fill_gradient2(
      name = "National rank\n(1=most deprived)",
      midpoint = national_median,
      low = "#e34948", mid = "#f0efec", high = "#2a78d6",
      limits = c(1, 1917)
    ) +
    labs(title = title, subtitle = paste("Rural n =", nrow(data))) +
    theme_void() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          legend.text = element_text(size = 8))
}

map_access <- make_map(rural_geo, "Access to Services", "Access to Services")
map_health <- make_map(rural_geo, "Health", "Health domain")

combined <- map_access + map_health +
  plot_annotation(
    title = "Digital exclusion and WIMD domain rank, rural Wales",
    subtitle = "Red = more deprived than Wales median | Blue = less deprived than Wales median",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave("outputs/domain_divergence_maps.png", combined, width = 12, height = 6, dpi = 300)
print(combined)

# REMINDER: 85.4% of rural LSOAs fall below the national median on Access
# to Services (more deprived); only 16.6% do on Health (mostly less
# deprived). If the rendered maps don't visually reflect roughly that
# split, something upstream (join, CRS, or column) is wrong — stop and
# check before using these maps anywhere.
