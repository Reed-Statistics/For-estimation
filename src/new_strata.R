library(tidyverse)

# Load data ---------------------------

stratadf <- readRDS("data/tidy/strata_df.rds")

# Load mean/var functions ---------------------------

source("src/s_mean.R")
source("src/s_var.R")

# Mode function ---------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Collapse function ---------------------------

collapse_strata <- function(state, county, strata){
  strata <- as.character(strata)
  df <- as.data.frame(cbind(state, county, strata))
  df$strata <- as.character(df$strata)
  df <- df %>%
    group_by(state, county, strata) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    group_by(state, county) %>%
#    arrange(state, county, strata) %>%
    mutate(collapse = ifelse(n >= 10, strata, Mode(strata)))
  df$collapse
}

# Collapse "good" schemes ---------------------------

stratadf <- stratadf %>%
  mutate(S2_collapse = as.factor(collapse_strata(state, county_code, S2_oldstrata_mountain_3)),
         S3_collapse = as.factor(collapse_strata(state, county_code, S3_oldstrata_mountain_4)),
         S4_collapse = as.factor(collapse_strata(state, county_code, S4_veg_type)),
         S6_collapse = as.factor(collapse_strata(state, county_code, S6_ns_tree_canopy)),
         S7_collapse = as.factor(collapse_strata(state, county_code, S7_fgroup_bins)),
         S9_collapse = as.factor(collapse_strata(state, county_code, S9_veg_bins_mountain)),
         S10_collapse = as.factor(collapse_strata(state, county_code, S10_tree_canopy_mountain)),
         S11_collapse = as.factor(collapse_strata(state, county_code, S11_old_strata_tree_canopy)),
         S12_collapse = as.factor(collapse_strata(state, county_code, S12_old_strata_veg_bins)),
         S13_collapse = as.factor(collapse_strata(state, county_code, S13_forest_prob)),
         S14_collapse = as.factor(collapse_strata(state, county_code, S14_mountain_biomass)))

# Export new dataset ---------------------------

saveRDS(stratadf, "data/tidy/collapse_df.rds")

