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
library(broom.mixed)

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
corrs <- filtered_df %>% group_by(Country_Code) %>% 
summarise(gini_gdp_corr = cor(Gini, Gdp_pc, use = "complete.obs")) %>% ungroup()
#
#
#
#
recent_values <- filtered_df %>%
  group_by(Country_Code) %>%
  filter(!is.na(Gini) & !is.na(Gdp_pc)) %>%
  slice_max(Year, n = 1) %>%
  select(Country_Name, Country_Code, Year, Gini, Gdp_pc) %>%
  rename(most_recent_year = Year,
         most_recent_gini = Gini,
         most_recent_gdp = Gdp_pc) %>%
  ungroup()

corrs_with_recent <- corrs %>%
  left_join(recent_values, by = "Country_Code")
#
#
#
#
ggplot(corrs_with_recent, aes(x = most_recent_gini, y = most_recent_gdp, color = gini_gdp_corr)) + geom_point() +
geom_text(aes(label = Country_Code), vjust = -0.5, size = 3, color = "black") +
scale_color_gradient2(low = "green", mid = "white", high = "red", midpoint = 0)
#
#
#
#
#
#
#

filtered_df$Year <- as.numeric(filtered_df$Year)
filtered_df$Country_Code <- as.factor(filtered_df$Country_Code)
filtered_df$Country_Name <- as.factor(filtered_df$Country_Name)

model_gdp <- lm(Gdp_pc ~ Year*Country_Code, filtered_df)
model_gini <- lm(Gini ~ Year*Country_Code, filtered_df)

summary(model_gdp)
summary(model_gini)

#
#
#
#

gdp_coefs <- coef(model_gdp)
gini_coefs <- coef(model_gini)


# Get unique country codes
countries <- levels(filtered_df$Country_Code)

# Calculate GDP slopes for each country
gdp_slope <- data.frame(
  Country_Code = countries,
  slope = sapply(countries, function(country) {
    if (country == countries[1]) {
      # Reference country (first level) - just the Year coefficient
      gdp_coefs["Year"]
    } else {
      # Other countries - Year coefficient + interaction term
      interaction_term <- paste0("Year:Country_Code", country)
      gdp_coefs["Year"] + ifelse(interaction_term %in% names(gdp_coefs), 
                                gdp_coefs[interaction_term], 0)
    }
  })
)

# Calculate Gini slopes for each country
gini_slope <- data.frame(
  Country_Code = countries,
  slope = sapply(countries, function(country) {
    if (country == countries[1]) {
      # Reference country (first level) - just the Year coefficient
      gini_coefs["Year"]
    } else {
      # Other countries - Year coefficient + interaction term
      interaction_term <- paste0("Year:Country_Code", country)
      gini_coefs["Year"] + ifelse(interaction_term %in% names(gini_coefs), 
                                 gini_coefs[interaction_term], 0)
    }
  })
)

final_df <- merge(gini_slope, gdp_slope, by = "Country_Code")
colnames(final_df) <- c("country", "gini", "gdp")
#
#
#
#
ggplot(final_df, aes(x = -gini, y = gdp)) +
geom_point() + 
geom_text(aes(label = country), vjust = -0.5, size = 3, color = "black")

#
#
#
#
final_df %>% filter(gdp > 0, gini < 0) %>% arrange(desc(gdp), gini)
#
#
#
