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

number_names <- colnames(gini_wide)[grepl("^\\d", colnames(gini_wide))]

df_long <- gini_wide %>% pivot_longer(names_to = "Year", values_to = "Gini", cols = number_names)
#
#
#
