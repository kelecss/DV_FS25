library(readr)
library(dplyr)
library(tidyr)
library(countrycode)


tag <- function(file, field, sex) {
  read_csv(file) %>%
    select(geoUnit, year, value) %>%
    rename(country = geoUnit) %>%
    mutate(field_of_study = field,
           sex = sex)
}

df_nsms_f <- tag("data_NSMS_f.csv", "NSMS", "Female")
df_ict_f  <- tag("data_ICT_f.csv",  "ICT",  "Female")
df_emc_f  <- tag("data_EMC_f.csv",  "ECM",  "Female")

df_nsms_m <- tag("data_NSMS_m.csv", "NSMS", "Male")
df_ict_m  <- tag("data_ICT_m.csv",  "ICT",  "Male")
df_emc_m  <- tag("data_EMC_m.csv",  "ECM",  "Male")

stem_sum <- bind_rows(
  df_nsms_f, df_ict_f, df_emc_f,
  df_nsms_m, df_ict_m, df_emc_m
)

stem_per_field <- stem_sum %>%
  pivot_wider(names_from = sex, values_from = value) %>%
  mutate(total = Female + Male,
         pct_female_per_field = (Female / total)*100)

stem_total <- stem_per_field %>%
  group_by(country, year) %>%
  summarise(
    female_total = sum(Female, na.rm = TRUE),
    male_total   = sum(Male, na.rm = TRUE),
    total        = female_total + male_total,
    pct_female_stem = (female_total / total)*100,
    .groups = "drop"
  )

stem_disaggregated <- stem_per_field %>% 
  select(country, year, field_of_study,pct_female_per_field) %>% 
  mutate(
    country_name = countrycode(country, origin = "iso3c", destination = "country.name.en")
  ) %>% write_csv("stem_disaggregated.csv")

View(stem_disaggregated)

stem_final <- stem_total %>%
  select(country, year, pct_female_stem) %>%
  mutate(
    country_name = countrycode(country, origin = "iso3c", destination = "country.name.en")
    ) %>% write_csv("stem_final.csv")



#Grid
all_countries <- unique(stem_final$country)
all_years <- unique(stem_final$year)

country_year_grid <- expand.grid(
  country = all_countries,
  year = all_years
)

stem_grid <- country_year_grid %>%
  left_join(stem_final, by = c("country", "year")) %>%  
  arrange(country, year) %>%
  write_csv("stem_grid.csv")



# Script: Most Recent

df <- read_csv("stem_grid_2.csv")

df_most_recent <- df %>%
  filter(!is.na(pct_female_stem)) %>%
  group_by(country) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(country, country_name, year, pct_female_stem)

write_csv(df_most_recent, "most_recent_pct_female_stem.csv")

View(df_most_recent)

# Script: Change 2015 - 2020

df <- read_csv("stem_grid_2.csv")

change_2015_2020 <- df %>%
  filter(year %in% c(2015, 2020)) %>%
  filter(!is.na(pct_female_stem)) %>%
  select(country, country_name, year, pct_female_stem) %>%
  pivot_wider(names_from = year, values_from = pct_female_stem, names_prefix = "y_") %>%
  filter(!is.na(y_2015) & !is.na(y_2020)) %>%
  mutate(
    change = y_2020 - y_2015,
    base_year = 2015,
    compare_year = 2020,
    y_start = y_2015,
    y_end = y_2020
  ) %>%
  select(country, country_name, base_year, compare_year, y_start, y_end, change)

write_csv(change_2015_2020, "change_2015_2020.csv")

View(change_2015_2020)

# Script: Change Combined
df <- read_csv("stem_grid_2.csv")

calculate_pair_change <- function(data, base_year, compare_year, already_included_countries) {
  base_col <- paste0("y_", base_year)
  compare_col <- paste0("y_", compare_year)
  
  data %>%
    filter(year %in% c(base_year, compare_year)) %>%
    filter(!is.na(pct_female_stem)) %>%
    filter(!country %in% already_included_countries) %>%
    select(country, country_name, year, pct_female_stem) %>%
    pivot_wider(names_from = year, values_from = pct_female_stem, names_prefix = "y_") %>%
    filter(!is.na(.data[[base_col]]) & !is.na(.data[[compare_col]])) %>%
    mutate(change = .data[[compare_col]] - .data[[base_col]],
           base_year = base_year,
           compare_year = compare_year,
           y_start = .data[[base_col]],
           y_end = .data[[compare_col]]) %>%
    select(country, country_name, base_year, compare_year, y_start, y_end, change)
}

year_pairs <- list(
  c(2015, 2020),
  c(2019, 2020),
  c(2020, 2021),
  c(2020, 2022),
  c(2018, 2019),
  c(2021, 2022)
)

included_countries <- c()
combined_changes <- list()

for (pair in year_pairs) {
  base <- pair[1]
  compare <- pair[2]
  
  new_data <- calculate_pair_change(df, base, compare, included_countries)
  
  included_countries <- c(included_countries, new_data$country)
  combined_changes[[paste0(base, "_", compare)]] <- new_data
}

change_combined <- bind_rows(combined_changes)

write_csv(change_combined, "change_combined_pct_female_stem.csv")


# Script: All changes available
library(tidyverse)

# Load data
df <- read_csv("stem_grid_2.csv")
most_recent <- read_csv("most_recent_pct_female_stem.csv")

# Clean input if needed
df <- df %>%
  filter(!is.na(pct_female_stem))

# Filter to only countries present in most_recent file
target_countries <- unique(most_recent$country)
df_filtered <- df %>%
  filter(country %in% target_countries)

# Function to calculate change for one country
calculate_country_change <- function(country_data) {
  country_data <- country_data %>%
    arrange(year)
  
  recent_year <- max(country_data$year)
  base_candidates <- country_data %>%
    filter(year <= (recent_year - 5))
  
  if (nrow(base_candidates) == 0) {
    return(NULL)  # no valid pair
  }
  
  base_year <- max(base_candidates$year)
  
  y_start <- country_data %>% filter(year == base_year) %>% pull(pct_female_stem)
  y_end <- country_data %>% filter(year == recent_year) %>% pull(pct_female_stem)
  
  tibble(
    country = unique(country_data$country),
    country_name = unique(country_data$country_name),
    base_year = base_year,
    compare_year = recent_year,
    y_start = y_start,
    y_end = y_end,
    change = y_end - y_start
  )
}

# Apply to each country
change_data <- df_filtered %>%
  group_by(country) %>%
  group_split() %>%
  map_dfr(calculate_country_change)

# Save result
write_csv(change_data, "change_pct_female_stem_recent_years.csv")

