# =============================================================================
# Script:      11_domain_level_analysis.R
# Project:     Rural Health Compass — NHS Wales Health Equity Analysis
# Author:      Nina Akporiaye
# Supervisor:  Dr Veronika Rasic
# Date:        July 2026
# Purpose:     Domain-level WIMD analysis + digital exclusion, using a 4-area
#              grouping (Powys, Ceredigion, Pembrokeshire, Swansea) - distinct
#              from 08's 2-group pooled design and 10's three-group design.
# Input:       master_analytical_dataset.csv (created in 04_load_ofcom.R)
# =============================================================================

library(tidyverse)

# ── Section 1: Load the master dataset fresh ─────────────────────────────
master <- read_csv("data/processed/master_analytical_dataset.csv", show_col_types = FALSE)

nrow(master)
ncol(master)
names(master)

# ── Section 2: Define the 4-area grouping ────────────────────────────────
three_counties <- c("Powys", "Ceredigion", "Pembrokeshire")

grouped <- master |>
  mutate(area = case_when(
    `Local Authority name (Eng)` %in% three_counties & rural_urban == "Rural" ~ `Local Authority name (Eng)`,
    `Local Authority name (Eng)` == "Swansea" & rural_urban == "Urban" ~ "Swansea (urban)",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(area))

# ── Section 3: Verify before trusting it ─────────────────────────────────
count(grouped, area)
cat("Total analytic sample:", nrow(grouped), "LSOAs\n")

stopifnot(anyDuplicated(grouped$LSOA21CD) == 0)
cat("No duplicate LSOAs across groups - check passed.\n")

# ── Section 4: Domain construction metadata ──────────────────────────────
# Bias split refined 2026-07-13 after supervision meeting with Dr Rasic:
# distance-based bias (geography/density built into the measure) is now
# separated from definitional bias (what the measure counts as
# "deprived" may fit urban patterns better than rural ones). See
# RQ3_interpretation_policy_note.docx and process_report.md for full reasoning.
domain_metadata <- tribble(
  ~domain,                 ~wimd_weight, ~construction_type,                                          ~bias_type,      ~bias_note,
  "Income",                 0.22,        "administrative (means-tested benefit/tax credit counts)",   "definitional",  "benefit-claim based; may undercount rural seasonal/precarious work as deprivation",
  "Employment",              0.20,        "administrative (unemployment-related benefit claims)",      "definitional",  "same as Income - together 42% of WIMD weight, worth treating as an open question not a settled finding",
  "Health",                  0.15,        "GP-recorded conditions, mental health, cancer; age-sex standardised", "none",  "no obvious bias identified",
  "Education",               0.14,        "attainment scores and absenteeism records",                 "none",          "no obvious bias identified",
  "Access to Services",      0.10,        "average travel time to services by public/private transport", "distance",    "90% travel-time, 10% broadband; favours dense areas by construction",
  "Housing",                 0.09,        "affordability (market entry cost) and physical condition",  "gap",           "does not measure second homes, holiday lets, or seasonal vacancy at all - may be missing a real rural housing dynamic entirely",
  "Community Safety",        0.05,        "police-recorded crime rate (violence, theft, robbery)",      "distance",     "favours low-density areas by construction",
  "Physical Environment",    0.05,        "air quality, noise pollution, green space (NDVI)",          "distance",      "favours low-density areas by construction",
  "Digital Exclusion",       NA,          "Ofcom premises-level broadband availability (% below 30 Mbps)", "distance",  "infrastructure/distance-linked, similar to Access to Services"
)

# ── Section 5: Kruskal-Wallis test across 4 areas, all 9 variables ───────
domains <- c("Income", "Employment", "Health", "Education",
             "Access to Services", "Housing",
             "Community Safety", "Physical Environment")

run_kruskal <- function(variable_name) {
  test <- kruskal.test(grouped[[variable_name]] ~ grouped$area)
  n <- nrow(grouped)
  k <- length(unique(grouped$area))
  eps_sq <- (test$statistic - k + 1) / (n - k)
  tibble(
    domain = variable_name,
    chi_sq = round(unname(test$statistic), 1),
    df = unname(test$parameter),
    p_value = test$p.value,
    epsilon_sq = round(unname(eps_sq), 3)
  )
}

kruskal_results <- map_dfr(c(domains, "pct_below_30mbps"), run_kruskal) |>
  mutate(domain = ifelse(domain == "pct_below_30mbps", "Digital Exclusion", domain))

kruskal_results$p_adjusted <- p.adjust(kruskal_results$p_value, method = "BH")

kruskal_results <- kruskal_results |>
  left_join(domain_metadata, by = "domain") |>
  select(domain, chi_sq, p_adjusted, epsilon_sq, construction_type, bias_type, bias_note) |>
  arrange(desc(epsilon_sq))

print(kruskal_results)

# ── Section 6: Pairwise comparisons ───────────────────────────────────────
# Post-hoc: pairwise.wilcox.test() used instead of dunn.test (bug found in
# dunn.test v1.4.1 - see process_report.md)

for (d in c(domains, "Digital Exclusion")) {
  varname <- if (d == "Digital Exclusion") "pct_below_30mbps" else d
  cat("\n---", d, "---\n")
  result <- pairwise.wilcox.test(grouped[[varname]], grouped$area,
                                 p.adjust.method = "BH")
  print(result)
}