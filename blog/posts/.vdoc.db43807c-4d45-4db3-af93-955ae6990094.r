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

gdp_long <- gdp_wide %>% pivot_longer(names_to = "Year", values_to = "Gdp_pc", cols = number_names2)

#
#
#
#
colnames(gini_long) <- gsub(" ", "_", colnames(gini_long))
colnames(gdp_long) <- gsub(" ", "_", colnames(gdp_long))

df <- merge(gini_long, gdp_long, by = c("Year", "Country_Code", "Country_Name"))
#
#
#
#
df <- df[!is.na(df$Gini) & !is.na(df$Gdp_pc), ]
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
df <- df %>% filter(Year >= 1980)

country_summary <- df %>%
  group_by(Country_Code) %>%
  summarise(
    row_count = n(),
    has_values_after_2020 = any(Year > 2020)
  ) %>%
  ungroup()

# Filter countries with < 10 rows AND no values after 2020
countries_to_exclude <- country_summary %>%
  filter(row_count < 10 & !has_values_after_2020) %>%
  pull(Country_Code) 

filtered_df <- df %>%
  filter(!Country_Code %in% countries_to_exclude)


#
#
#
#
ggplot(filtered_df, aes(x = Year, y = Gdp_pc, color = Country_Code, group = Country_Code)) +
geom_point() + 
geom_line() +
theme(legend.position = "none")
#
#
#
#
ggplot(filtered_df, aes(x = Year, y = Gini, color = Country_Code, group = Country_Code)) +
geom_point() + 
geom_line() +
theme(legend.position = "none")
#
#
#
#
#
#
filtered_df$year_centered <- scale(filtered_df$year, scale = FALSE)[,1]
filtered_df$gini_scaled <- scale(df_complfiltered_dfete$gini)[,1]
filtered_df$gdp_scaled <- scale(df_complete$gdp)[,1]
#
#
#
