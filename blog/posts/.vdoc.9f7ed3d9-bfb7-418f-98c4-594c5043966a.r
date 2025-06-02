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

# Ensure Year is numeric
filtered_df$Year <- as.numeric(filtered_df$Year)

# Function to impute missing values using country-specific linear regression
impute_country_trends <- function(df, value_col) {
  # Get all possible year-country combinations
  all_years <- seq(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE))
  all_countries <- unique(df$Country_Code)
  complete_grid <- expand.grid(Year = all_years, Country_Code = all_countries)

  # Merge with original data
  df_complete <- merge(complete_grid, df, by = c("Year", "Country_Code"), all.x = TRUE)

  # Impute for each country
  df_imputed <- df_complete %>%
    group_by(Country_Code) %>%
    do({
      country_data <- .
      value_data <- country_data[[value_col]]

      # Only impute if we have at least 2 non-missing values
      if(sum(!is.na(value_data)) >= 2) {
        # Fit linear model for this country
        lm_model <- lm(value_data ~ Year, data = country_data)

        # Predict missing values
        missing_idx <- is.na(value_data)
        country_data[[value_col]][missing_idx] <- predict(lm_model, 
                                                          newdata = country_data[missing_idx, ])
      }
      country_data
    }) %>%
    ungroup()

  return(df_imputed)
}

# Impute GDP per capita
df_gdp_imputed <- impute_country_trends(filtered_df, "Gdp_pc")

# Impute Gini coefficient  
df_gini_imputed <- impute_country_trends(filtered_df, "Gini")

# Merge imputed datasets
df_final <- merge(df_gdp_imputed[, c("Year", "Country_Code", "Gdp_pc")],
                  df_gini_imputed[, c("Year", "Country_Code", "Gini")],
                  by = c("Year", "Country_Code"))

# Remove rows where both variables are still missing (countries with <2 observations)
df_final <- df_final[!(is.na(df_final$Gdp_pc) & is.na(df_final$Gini)), ]


control_opts <- lmerControl(optimizer = "bobyqa", 
                           optCtrl = list(maxfun = 100000))

# Fit LMMs on imputed data
gdp_model <- lmer(Gdp_pc ~ Year + (Year | Country_Code), 
                  data = df_final,
                        control = control_opts)

gini_model <- lmer(Gini ~ Year + (Year | Country_Code), 
                   data = df_final)

summary(gdp_model)
summary(gini_model)

#
#
#


# Create prediction data for each country
countries <- unique(filtered_df$Country_Code)
year_range <- range(filtered_df$Year, na.rm = TRUE)
pred_data <- expand.grid(
  Year = seq(year_range[1], year_range[2], length.out = 50),
  Country_Code = countries
)

# Generate predictions
pred_data$gdp_pred <- predict(gdp_model, newdata = pred_data)
pred_data$gini_pred <- predict(gini_model, newdata = pred_data)

# Plot all GDP trends in one plot
p1 <- ggplot() +
  geom_line(data = pred_data, aes(x = Year, y = gdp_pred, group = Country_Code), 
            alpha = 0.7, color = "steelblue") +
  labs(title = "GDP per Capita Trends - All Countries",
       x = "Year", y = "GDP per Capita") +
  theme_minimal()

# Plot all Gini trends in one plot  
p2 <- ggplot() +
  geom_line(data = pred_data, aes(x = Year, y = gini_pred, group = Country_Code), 
            alpha = 0.7, color = "darkred") +
  labs(title = "Gini Coefficient Trends - All Countries", 
       x = "Year", y = "Gini Coefficient") +
  theme_minimal()

# Display plots
print(p1)
print(p2)


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


#
#
#
#
# Combine slope information with country names
country_performance <- gdp_slopes %>%
  left_join(gini_slopes, by = "Country_Code") %>%
  left_join(filtered_df %>% select(Country_Code, Country_Name) %>% distinct(), 
            by = "Country_Code") %>% select(Country_Code, Country_Name, gini_slope_total, gdp_slope_total)


#
#
#
#
ggplot(country_performance, aes(x = -gini_slope_total, y = gdp_slope_total)) +
geom_point() +
geom_text(aes(label = Country_Code), vjust = -0.5, size = 3, color = "black")
#
#
#
