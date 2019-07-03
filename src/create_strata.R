# Procedure to wrangle data into strata, where new columns indicate strata

library(tidyverse)
library(stringr)

# Load data ---------------------------

# Make sure to set working directory to math343_s19_forests
plotdf <- readRDS("~/math343_s19_forests/data/tidy/plot_tidy.rds")

# Collapse old strata
plotdf <- plotdf %>%
  mutate(old_strata = as.factor(ifelse(old_strata == "3", "onfor", 
                                       ifelse(old_strata == "2", "onfor",
                                              "ofor"))))

# S1: mountain and nonmountain ---------------
plotdf <- plotdf %>%
  mutate(S1_mountain = as.factor(ifelse(mountain == "mountain", "mt", "nmt")))

# S2: mountain*forest + nonforest, water ---------------
plotdf <- plotdf %>%
  mutate(S2_oldstrata_mountain_3 = as.factor(ifelse(old_strata == "onfor", "onfor",
                                                           ifelse(mountain == "mountain", "ofor_mt", "ofor_nmt"))))

# S3: mountain*forest + water ---------------
plotdf <- plotdf %>%
  mutate(S3_oldstrata_mountain_4 = as.factor(ifelse(old_strata == "ofor",
                                                           ifelse(mountain == "mountain", "ofor_mt", "ofor_nmt"),
                                                           ifelse(mountain == "mountain", "onfor_mt", "onfor_nmt"))))
# S4: base vegetation types ---------------
plotdf <- plotdf %>%
  mutate(S4_veg_type = as.factor(ifelse(veg_type == "Agriculture", "ag",
                                        ifelse(veg_type == "Barren", "bar",
                                               ifelse(veg_type == "Developed", "dev",
                                                      ifelse(veg_type == "Herb", "her",
                                                             ifelse(veg_type == "Shrub", "shr",
                                                                    ifelse(veg_type == "Sparse", "sp",
                                                                           ifelse(veg_type == "Tree", "tr", "wat")))))))))

# S5: biomass bins ---------------
plotdf <- plotdf %>%
  mutate(S5_biomass = as.factor(ifelse(biomass < 7, "bio1",
                                       ifelse(biomass < 21, "bio2",
                                              ifelse(biomass < 76, "bio3", "bio4")))))
# S6: tree_canopy bins ---------------
plotdf <- plotdf %>%
  mutate(S6_ns_tree_canopy = cut(tree_canopy, c(0, 6, 51, 66, 100), 
                                        labels = c("can1", "can2", "can3", "can4"), 
                                        include.lowest = TRUE))

# S7: forest group bins ---------------
plotdf <- plotdf %>%
  mutate(S7_fgroup_bins = as.factor(ifelse(fgroup_bins == "Nonforest", "nonf",
                                           ifelse(fgroup_bins == "Pinyon/Juniper", "pinj",
                                                  ifelse(fgroup_bins == "Douglas-fir", "dfir",
                                                         ifelse(fgroup_bins == "Ponderosa Pine", "ppin",
                                                                ifelse(fgroup_bins == "Fir/Spruce/Hemlock", "fsh",
                                                                       ifelse(fgroup_bins == "Lodgepole Pine", "lpin",
                                                                              ifelse(fgroup_bins == "Aspen/Birch", "asb", "oth")))))))))
# S8: veg_bin ---------------
plotdf <- plotdf %>%
  mutate(S8_veg_bins = as.factor(ifelse(veg_bins == "Herb", "her",
                                                 ifelse(veg_bins =="Other", "oth",
                                                        ifelse(veg_bins == "Shrub", "shr",
                                                               "tr")))))


# S9: veg_bin * mountain/nonmountain ---------------
plotdf <- plotdf %>%
  mutate(S9_veg_bins_mountain = as.factor(ifelse(veg_bins == "Herb", paste("her", S1_mountain, sep = "_"), 
                                                 ifelse(veg_bins =="Other", paste("oth", S1_mountain, sep = "_"),
                                                        ifelse(veg_bins == "Shrub", paste("shr", S1_mountain, sep = "_"),
                                                               paste("tr", S1_mountain, sep = "_"))))))

# S10: tree_canopy*mountain ---------------
plotdf <- plotdf %>%
  mutate(S10_tree_canopy_mountain = interaction(S6_ns_tree_canopy, mountain, sep = "_"))


# S11: old_strata*tree_canopy + nonforest + water ---------------
plotdf <- plotdf %>%
  filter(old_strata != "0") %>%
  mutate(S11_old_strata_tree_canopy = interaction(old_strata, S6_ns_tree_canopy, sep = "_"))
plotdf$S11_old_strata_tree_canopy <- fct_collapse(plotdf$S11_old_strata_tree_canopy, 
                                                  nf = c("2_can1", "2_can2", "2_can3", "2_can4"), 
                                                  wat = c("3_can1", "3_can2", "3_can3", "3_can4"))


# S12: old_strata*veg_bin ---------------
plotdf <- plotdf %>%
  filter(old_strata != "0") %>%
  mutate(S12_old_strata_veg_bins = interaction(old_strata, veg_bins, sep = "_"))
plotdf$S12_old_strata_veg_bins <- fct_collapse(plotdf$S12_old_strata_veg_bins, 
                                                     wat = c("3_Herb", "3_Other", "3_Tree", "3_Shrub"))
plotdf$S12_old_strata_veg_bins <- fct_recode(plotdf$S12_old_strata_veg_bins, 
                                                   ofor_her = "1_Herb", onfor_her = "2_Herb", ofor_oth = "1_Other", onfor_oth = "2_Other", ofor_shr = "1_Shrub", onfor_shrub = "2_Shrub", ofor_tr = "1_Tree", onfor_tr = "2_Tree")


# S13: bin forest_prob ---------------
plotdf <- plotdf %>%
  mutate(S13_forest_prob = cut(forest_prob, c(0, 0.07, 0.27, 0.57, 1), 
                               labels = c("forest_prob_1", "forest_prob_2", "forest_prob_3", "forest_prob_4"), 
                               include.lowest = TRUE))


# S14: mountain*biomass ---------------
plotdf <- plotdf %>%
  mutate(S14_mountain_biomass = as.factor(ifelse(mountain == "mountain",
                                                 paste("mt", S5_biomass, sep = "_"), 
                                                 paste("nmt", S5_biomass, sep = "_"))))


# S15: forest_prob*mountain + nonmountain ---------------
plotdf <- plotdf %>%
  mutate(S15_forest_prob_mountain = interaction(S13_forest_prob, mountain, sep = "_"))

plotdf$S15_forest_prob_mountain <- fct_collapse(plotdf$S15_forest_prob_mountain, 
                                                nmt = c("forest_prob_1_nonmountain", "forest_prob_2_nonmountain", 
                                                        "forest_prob_3_nonmountain", "forest_prob_4_nonmountain"))


#levels(plotdf$S1_mountain)
#levels(plotdf$S2_oldstrata_mountain_3)
#levels(plotdf$S3_oldstrata_mountain_4)
#levels(plotdf$S4_veg_type)
#levels(plotdf$S5_biomass)
#levels(plotdf$S6_ns_tree_canopy)
#levels(plotdf$S7_fgroup_bins)
#levels(plotdf$S8_veg_bins)
#levels(plotdf$S9_veg_bins_mountain)
#levels(plotdf$S10_tree_canopy_mountain)
#levels(plotdf$S11_old_strata_tree_canopy)
#levels(plotdf$S12_old_strata_veg_bins)
#levels(plotdf$S13_forest_prob)
#levels(plotdf$S14_mountain_biomass)
#levels(plotdf$S15_forest_prob_mountain)


# Export data ---------------------------

saveRDS(plotdf, "data/tidy/strata_df.rds")










