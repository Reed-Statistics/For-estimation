# Code to import, merge, and tidy the two datasets we need for initial analysis

library(tidyverse)
library(stringr)

# Load data ---------------------------

# make sure to set working directory to math343_s19_forests
plot_response <- read.csv("data/raw/plot_level/plot_response.csv")
plot_spatial <- read.csv("data/raw/plot_level/plt_spatial.csv")

# Merge data ---------------------------

shared_names <- intersect(colnames(plot_response), colnames(plot_spatial))
plot_full <- inner_join(plot_response, plot_spatial, by = shared_names)

# Rename columns ---------------------------

colnames(plot_full) <- tolower(colnames(plot_full)) # convert to lower snake-case

plot_full <- plot_full %>% # change some of the names to make data more readable
  rename(plot_id = plt_cn,
         prev_plot_id = prev_plt_cn,
         year = invyr,
         state = statecd,
         county_code = countycd,
         longitude = lon_public,
         latitude = lat_public,
         elevation = elev_public,
         # Predictor variables
         eco_region = ecosubcd, # Ecoregion (from Cleland)
         old_strata = fiastrat, # FIA's current stratification system
         forest_type_group = forgrp, # Forest type
         tree_canopy = nlcd11, # NLCD tree canopy cover
         existing_veg = evtlf, # Landfire existing vegetation
         forest_prob = forprob, # forest probability
         biomass = forbio, # forest biomass
         # Response variables
         basal_area = balive_tpa, # basal area in sqft
         tpa_unadj = cntlive_tpa, # trees per acre, unadjusted
         ag_biomass = biolive_tpa, # above-ground biomass in lbs
         sawlog_vol = volnlive_tpa) # volume of sawlog portion of sawtimber trees in cubic ft

# Fix state codes, mutate ecoregion ---------------------------

plot_full$state <- as.factor(plot_full$state)

plot_full <- plot_full %>%
  mutate(state = fct_recode(state,
                            "AZ" = "4",
                            "CO" = "8",
                            "ID" = "16",
                            "MT" = "30",
                            "NV" = "32",
                            "NM" = "35",
                            "UT" = "49",
                            "WY" = "56"))
  

# Mutate ecoregion mountain/province ---------------------------

plot_full <- plot_full %>%
  mutate(mountain = if_else(str_starts(eco_region, "M"), 
                            "mountain", "nonmountain")) %>%
  mutate(eco_prov = gsub("[^0-9]", "", eco_region))

plot_full <- plot_full %>%
  mutate(eco_province = ifelse(mountain == "nonmountain", eco_prov,
                               paste("M", eco_prov, sep = "")))

# Landfire EVT codes ---------------------------

# import evt lookup table
evt_codes <- read.csv("data/raw/_luts/spatial/evtLF_lut.csv")
# select codes, value
evt_codes <- evt_codes %>%
  select(VALUE, EVT_LF)

# Join two datasets, rename column
plot_full <- plot_full %>%
  left_join(evt_codes, by = c("existing_veg" = "VALUE")) %>%
  rename(veg_type = EVT_LF)

# Bin existing veg, forest group ---------------------------

plot_full <- plot_full %>%
  mutate(veg_bins = ifelse(veg_type %in% c("Herb", "Shrub", "Tree"),
                           as.character(veg_type), "Other"))

keep_groups <- c("0", "180", "260", "200", "220", "280", "900")

plot_full <- plot_full %>%
  mutate(fgroup_bins = ifelse(forest_type_group %in% keep_groups, forest_type_group, "Other"))

plot_full$fgroup_bins <- as.factor(plot_full$fgroup_bins)
levels(plot_full$fgroup_bins) <- c("Nonforest", "Pinyon/Juniper", "Douglas-fir", "Ponderosa Pine",
                                 "Fir/Spruce/Hemlock", "Lodgepole Pine", "Aspen/Birch", "Other")

# Remove missing values ---------------------------

plot_full <- plot_full %>%
  filter(!is.na(forest_prob)) %>%
  filter(old_strata != 0)

# Change vars from numeric to factor ---------------------------

# isolate columns we DONT want to convert to factor
keep_cols <- c("state", "longitude", "latitude", "elevation", "eco_region", "evalid",
               "ccliveplt", "fornonsamp", "condprop_unadj", "slope", "aspect",
               "live_canopy_cvr_pct", "condmethod", "forest_prob", "tree_canopy",
               "demlf", "biomass", "basal_area", "tpa_unadj", "ag_biomass", "sawlog_vol")

plot_full[, -which(names(plot_full) %in% keep_cols)] <- 
  lapply(plot_full[, -which(names(plot_full) %in% keep_cols)], as.factor)

# Export data ---------------------------

saveRDS(plot_full, "data/tidy/plot_tidy.rds")

# Testing and Training Split ---------------------------

#test_tidy <- plot_full %>%
#  filter(state %in% c("UT", "AZ", "CO", "ID", "NV", "WY"))

#train_tidy <- plot_full %>%
#  filter(state %in% c("MT", "NM"))

# Export Test and Training ---------------------------

#write.csv(test_tidy, "data/tidy/test_tidy.csv", row.names = F)

#write.csv(train_tidy, "data/tidy/train_tidy.csv", row.names = F)
