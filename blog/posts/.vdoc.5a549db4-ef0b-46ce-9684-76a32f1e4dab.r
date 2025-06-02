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

#setwd("./blog/posts")

df <- read_csv("../assets/world_bank_gini.csv")

df <- df %>% select(-c("Indicator Name", "Indicator Code", "...70"))

number_names <- colnames(df)[grepl("^\\d", all_column_names)]

df_long <- df %>% pivot_longer(names_to = "Year", values_to = "Gini", cols = )
#
#
#
