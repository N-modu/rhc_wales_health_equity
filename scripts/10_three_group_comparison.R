# ============================================================
# 10_three_group_comparison.R
# Descriptive stats + pattern plots: rural vs Swansea-urban vs
# county-urban (market towns), BEFORE any formal statistical test.
#
# WHY THREE GROUPS, NOT TWO:
# Swansea-urban (n=131) and county-urban (n=38) were tested and found
# to differ significantly on exclusion (1.00% vs 0.23%, p<0.0001) and
# Access to Services (817 vs 1152, p=0.0002) — 2026-07-04. Pooling them
# as one "urban" group was rejected as methodologically unsound.
#
# CAUTION: county-urban n=38 is thin. Treat any pattern here as
# suggestive, not confirmatory, until you've decided how (or whether)
# to report power for a group this small.
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)

three_counties <- c("Powys", "Ceredigion", "Pembrokeshire")

master <- df  # adjust to your loaded object name

grouped <- master %>%
  mutate(comparator_group = case_when(
    `Local Authority name (Eng)` %in% three_counties & rural_urban == "Rural" ~ "Rural (3 counties)",
    `Local Authority name (Eng)` %in% three_counties & rural_urban == "Urban" ~ "County-urban (market towns)",
    `Local Authority name (Eng)` == "Swansea" & rural_urban == "Urban" ~ "Swansea-urban",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(comparator_group))

cat("Group sizes:\n")
print(table(grouped$comparator_group))
# EXPECTATION CHECK: 157 / 38 / 131. If different, filter logic broke.

# ---- DESCRIPTIVE STATS ---------------------------------------------
vars_of_interest <- c("pct_below_30mbps", "Access to Services", "Health")

descriptives <- grouped %>%
  group_by(comparator_group) %>%
  summarise(across(all_of(vars_of_interest),
                    list(mean = ~mean(.x, na.rm=TRUE),
                         median = ~median(.x, na.rm=TRUE),
                         sd = ~sd(.x, na.rm=TRUE)),
                    .names = "{.col}_{.fn}"))
print(descriptives)

# ---- PATTERN PLOT ----------------------------------------------------
plot_data <- grouped %>%
  select(comparator_group, all_of(vars_of_interest)) %>%
  pivot_longer(-comparator_group, names_to = "variable", values_to = "value")

ggplot(plot_data, aes(x = comparator_group, y = value, fill = comparator_group)) +
  geom_boxplot(outlier.alpha = 0.4) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Digital exclusion and domain rank by comparator group",
       subtitle = "County-urban n=38 — interpret with caution given small sample",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("outputs/three_group_pattern.png", width = 10, height = 5, dpi = 300)

# NEXT STEP (not run here): if the pattern looks meaningfully different
# across all three groups, decide Kruskal-Wallis (one test, 3 groups)
# vs. two separate pairwise comparisons — and confirm you're prepared
# for county-urban (n=38) to show no significant pattern simply due to
# sample size, not because no effect exists.
