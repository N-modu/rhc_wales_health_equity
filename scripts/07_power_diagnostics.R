# =============================================================================
# Script:      07_power_diagnostics.R
# Project:     Rural Health Compass — NHS Wales Health Equity Analysis
# Author:      Nina Akporiaye
# Supervisor:  Dr Veronika Rasic
# Date:        June 2026
# Purpose:     Post-hoc power analysis for RQ3 Spearman correlation —
#              assess whether non-significant result reflects true null
#              or insufficient sample size (n=157 rural LSOAs)
# Input:       Spearman correlation results from Section 7,
#              05_stratification_check.R
# Output:      Power estimate, required n for 80% power
# =============================================================================
library(pwr)
pwr.r.test(n = 157, r = -0.12, sig.level = 0.05)
pwr.r.test(r = -0.12, sig.level = 0.05, power = 0.80)