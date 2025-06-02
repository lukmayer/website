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

# Fit LMM for GDP per capita
gdp_model <- lmer(Gdp_pc ~ Year + (Year | Country_Code), 
                  data = filtered_df)

# Fit LMM for Gini coefficient  
gini_model <- lmer(Gini ~ Year + (Year | Country_Code), 
                   data = filtered_df)

# Check model summaries
summary(gdp_model)
summary(gini_model)


#
#
#
#
# Extract random effects (country-specific deviations from overall trend)
gdp_slopes <- ranef(gdp_model)$Country_Code %>%
  rownames_to_column("Country_Code") %>%
  rename(gdp_intercept_dev = `(Intercept)`, 
         gdp_slope_dev = Year) %>%
  mutate(
    # Total country-specific slope = fixed effect + random effect
    gdp_slope_total = fixef(gdp_model)["Year"] + gdp_slope_dev
  )

gini_slopes <- ranef(gini_model)$Country_Code %>%
  rownames_to_column("Country_Code") %>%
  rename(gini_intercept_dev = `(Intercept)`, 
         gini_slope_dev = Year) %>%
  mutate(
    # Total country-specific slope = fixed effect + random effect  
    gini_slope_total = fixef(gini_model)["Year"] + gini_slope_dev
  )

#
#
#
#
# Create prediction grid for common time period
prediction_years <- seq(min(filtered_df$Year), max(filtered_df$Year), by = 1)
countries <- unique(filtered_df$Country_Code)

pred_grid <- expand.grid(Year = prediction_years, Country_Code = countries)

# Generate predictions
pred_grid$gdp_pred <- predict(gdp_model, newdata = pred_grid)
pred_grid$gini_pred <- predict(gini_model, newdata = pred_grid)

# Combine slope information with country names
country_performance <- gdp_slopes %>%
  left_join(gini_slopes, by = "Country_Code") %>%
  left_join(filtered_df %>% select(Country_Code, Country_Name) %>% distinct(), 
            by = "Country_Code") %>%
  mutate(
    # Convert GDP slope from log scale (% growth per year)
    gdp_growth_annual_slope = gdp_slope_total * 100,
    # Gini slope (negative = reduction per year)
    gini_reduction_annual_slope = -gini_slope_total
  ) %>%
  # Filter for countries with growth AND inequality reduction
  filter(gdp_growth_annual_slope > 0 & gini_reduction_annual_slope > 0) %>%
  mutate(
    # Normalize and combine
    gdp_norm = (gdp_growth_annual_slope - min(gdp_growth_annual_slope)) / 
               (max(gdp_growth_annual_slope) - min(gdp_growth_annual_slope)),
    gini_norm = (gini_reduction_annual_slope - min(gini_reduction_annual_slope)) / 
                (max(gini_reduction_annual_slope) - min(gini_reduction_annual_slope)),
    combined_score = (gdp_norm + gini_norm) / 2
  ) %>%
  arrange(desc(combined_score))

# Top performers based on model slopes
country_performance %>%
  select(Country_Name, gdp_growth_annual_slope, gini_reduction_annual_slope, 
         combined_score) %>%
  head(10)

#
#
#
