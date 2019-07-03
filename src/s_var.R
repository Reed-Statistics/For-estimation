# Equation 4.14 in Green: Est. Variance of the Mean of Attribute of Interest
# a - At - total area in the population in acres
# y - y - population attribute of interest - must be specified as column (df$col)
# strata - column name correspondign to strata of interest - must be column
# uses function s_mean to get objects df_v, and final_mean (contains pop_w_mean)

library(tidyverse)

s_var <- function(a, response, strata){
  a <- 1
  s_mean(response = response, strata = strata)
  df <- data.frame(response, strata)
  names(df) <- c("response", "strata")
  
  prop <- df %>%
    group_by(strata) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n),
           freq = if_else(is.na(freq), 0, freq))
  
  df <- df %>%
    group_by(strata) %>%
    summarise(var = var(response))
  
  df <- inner_join(df, prop, by = "strata")
  
  df <- inner_join(df, df_v, by = c("strata", "n", "freq"))
  
  sum_first_half <- df %>%
    group_by(strata) %>%
    mutate(sum1 = freq * n * var) %>%
    ungroup() %>%
    summarise(sum1_tot = sum(sum1))
  
  sum_second_half <- df %>%
    group_by(strata) %>%
    mutate(int = (1 - freq) * freq * var) %>%
    ungroup() %>%
    summarise(sum2_tot = sum(int))
  
  final_var <- (sum_first_half + sum_second_half) / sum(df$n)
  names(final_var) <- "variance"
  final_var <- final_var * a * a
  final_var <- mutate(final_var, variance = 
                        if_else(is.na(variance), 0, variance))
  return(final_var$variance)
}
