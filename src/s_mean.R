# Equation 4.13 in Green: Population Mean of the Attribute of Interest
# a - At - total area in the population in acres
# y - y - population attribute of interest - must be specified as column (df$col)
# strata - column name correspondign to strata of interest - must be column
# note produces final_mean and df_v as outputs in environment, used for s_var()
library(tidyverse)

s_mean <- function(a, response, strata){
  a <- 1
  df <- data.frame(response, strata)
  names(df) <- c("response", "strata")
  
  prop <- df %>%
    group_by(strata) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  
  df <- df %>%
    group_by(strata) %>%
    summarise(mean = mean(response))
  
  df <- inner_join(df, prop, by = "strata")
  df_v <<- df
  
  final_mean <<- df %>%
    mutate(w_mean = mean * freq) %>%
    summarise(pop_w_mean = a*sum(w_mean))
  final_mean <- as.data.frame(final_mean)
  return(final_mean$pop_w_mean)
}
