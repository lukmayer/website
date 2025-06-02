#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(tidyverse)
library(lme4)

#setwd("./blog/posts")

gini_wide <- read_csv("../assets/world_bank_gini.csv")

gini_wide <- gini_wide %>% select(-c("Indicator Name", "Indicator Code", "...70"))

number_names1 <- colnames(gini_wide)[grepl("^\\d", colnames(gini_wide))]

gini_long <- gini_wide %>% pivot_longer(names_to = "Year", values_to = "Gini", cols = number_names1)
#
#
#
#
gdp_wide <- read_csv("../assets/world_bank_gdp_pc.csv")

gdp_wide <- gdp_wide %>% select(-c("Indicator Name", "Indicator Code", "...70"))

number_names2 <- colnames(gdp_wide)[grepl("^\\d", colnames(gdp_wide))]

gdp_long <- gdp_wide %>% pivot_longer(names_to = "Year", values_to = "Gini", cols = number_names2)

#
#
#
#
combined_df_merge <- merge(df1, df2, by = c("Year", "Country_Code"))
print(combined_df_merge)
#
#
#
