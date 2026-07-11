# ============================================================
# 08_domain_reanalysis.R
# RQ3 — Domain-level disaggregation and confound checks
#
# WHY THIS SCRIPT EXISTS:
# The composite WIMD score produced a null result (rho=-0.12, p=0.128,
# rural n=157) that was initially interpreted as underpowered. Disaggregating
# by WIMD domain revealed this was masking directionally opposed effects.
# This script reproduces that disaggregation in the committed pipeline,
# rather than leaving it as one-off exploratory analysis outside GitHub

# STATUS: exploratory / hypothesis-generating. Multiple-comparisons
# correction checked by hand 2026-07 (BH, per-group) - all rural
# p-values remain significant by many orders of magnitude, and the
# same 2 of 8 urban domains (Access to Services, Housing) remain the
# only significant ones. Correction does not change any conclusion,
# so it was not added to the script; this note documents that the
# check was done rather than skipped.
# ============================================================

library(dplyr)
library(readr)

# ---- CONFIG: update these if the study scope changes ----------
# Path to master analytical dataset. UPDATE to match your repo's actual
# data/ folder convention (this currently points at a placeholder).
DATA_PATH <- "data/processed/master_analytical_dataset.csv"

# Rural comparator counties. If RQ2/RQ3 overlap changes your grouping
# (e.g. after reviewing Ruthikssha's distance-to-facility data), update here.
RURAL_COUNTIES <- c("Powys", "Ceredigion", "Pembrokeshire")

# Urban comparator authority (in addition to urban LSOAs within the
# rural counties themselves).
URBAN_COMPARATOR <- "Swansea"

# WIMD domain columns to test individually. Add/remove domains here if
# additional WIMD sub-indices become relevant.
WIMD_DOMAINS <- c("Income", "Employment", "Health", "Education",
                   "Access to Services", "Housing",
                   "Community Safety", "Physical Environment")

# Digital exclusion measure. Swap this if the operational definition
# (continuous vs binary threshold) is finalised differently.
EXCLUSION_VAR <- "pct_below_30mbps"

# Rank direction confirmed 2026-07: WIMD rank 1 = MOST deprived.
# This affects how you interpret sign(rho) below — do not change without
# re-verifying against the current WIMD documentation.
RANK_1_IS_MOST_DEPRIVED <- TRUE

# ---- LOAD DATA ---------------------------------------------------
df <- read_csv(DATA_PATH, show_col_types = FALSE)

rural <- df %>%
  filter(`Local Authority name (Eng)` %in% RURAL_COUNTIES,
         rural_urban == "Rural")

urban <- df %>%
  filter((`Local Authority name (Eng)` %in% RURAL_COUNTIES & rural_urban == "Urban") |
           (`Local Authority name (Eng)` == URBAN_COMPARATOR & rural_urban == "Urban"))

cat("Rural n:", nrow(rural), " | Urban n:", nrow(urban), "\n")
# EXPECTATION CHECK: rural n should be 157 for the original 3-county design.
# If it isn't, the county list or rural_urban flag has changed — stop and
# find out why before trusting anything downstream.
stopifnot(nrow(rural) > 0, nrow(urban) > 0)

# ---- DOMAIN-LEVEL CORRELATIONS ------------------------------------
run_domain_correlations <- function(data, exclusion_var, domains) {
  purrr::map_dfr(domains, function(domain) {
    test <- suppressWarnings(cor.test(data[[exclusion_var]], data[[domain]],
                                        method = "spearman"))
    tibble(domain = domain, rho = unname(test$estimate), p_value = test$p.value)
  })
}

rural_results <- run_domain_correlations(rural, EXCLUSION_VAR, WIMD_DOMAINS) %>%
  mutate(group = "rural")
urban_results <- run_domain_correlations(urban, EXCLUSION_VAR, WIMD_DOMAINS) %>%
  mutate(group = "urban")

domain_results <- bind_rows(rural_results, urban_results)
print(domain_results)

# MULTIPLE COMPARISONS: 8 domains x 2 groups = 16 tests run here.
# This script does NOT apply a correction (Bonferroni, BH, etc.) — that
# decision belongs in the write-up, not silently in code. If you decide
# to report these as confirmatory rather than exploratory, add a
# correction step here and update the STATUS note at the top of this file.

# ---- REMOTENESS (RUC21NM) STRATIFICATION --------------------------
# NOTE: within "further from town" LSOAs only, total_premises (settlement
# size) did NOT explain the split (rho~0.09, ns, tested 2026-07-04).
# The RUC21NM label captures something beyond raw premises count — likely
# nearest-settlement type/size, which is not present as a variable in
# this dataset. This is a genuine open gap, not yet resolved.
ruc_summary <- rural %>%
  group_by(RUC21NM) %>%
  summarise(
    n = n(),
    mean_exclusion = mean(.data[[EXCLUSION_VAR]], na.rm = TRUE),
    mean_access = mean(`Access to Services`, na.rm = TRUE),
    mean_health = mean(Health, na.rm = TRUE),
    mean_income = mean(Income, na.rm = TRUE),
    mean_premises = mean(total_premises, na.rm = TRUE)
  )
print(ruc_summary)
# FLAG: any category with n < 10 is decoration, not evidence — check
# ruc_summary$n before citing a row in the write-up.

# ---- OUTPUT --------------------------------------------------------
write_csv(domain_results, "outputs/domain_correlations.csv")
write_csv(ruc_summary, "outputs/ruc21nm_summary.csv")

cat("\nDone. Remember: this is exploratory. Update STATUS note above if",
    "you apply a multiple-comparisons correction or resolve the",
    "remoteness-driver gap with new data.\n")
