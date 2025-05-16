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

stem_wide <- stem_sum %>%
  pivot_wider(names_from = sex, values_from = value) %>%
  mutate(
    Female = replace_na(Female, 0),
    Male   = replace_na(Male, 0),
    total  = Female + Male
  )

stem_totals <- stem_wide %>%
  group_by(country, year) %>%
  summarise(
    female_total = sum(Female, na.rm = TRUE),
    male_total   = sum(Male, na.rm = TRUE),
    total        = female_total + male_total,
    pct_female_stem = (female_total / total) * 100,
    pct_male_stem   = (male_total / total) * 100,
    .groups = "drop"
  )

female_dist <- stem_wide %>%
  group_by(country, year) %>%
  mutate(female_total = sum(Female, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_female_in_field = (Female / female_total) * 100) %>%
  select(country, year, field_of_study, pct_female_in_field) %>%
  pivot_wider(names_from = field_of_study, values_from = pct_female_in_field, names_prefix = "pct_female_in_")

male_dist <- stem_wide %>%
  group_by(country, year) %>%
  mutate(male_total = sum(Male, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_male_in_field = (Male / male_total) * 100) %>%
  select(country, year, field_of_study, pct_male_in_field) %>%
  pivot_wider(names_from = field_of_study, values_from = pct_male_in_field, names_prefix = "pct_male_in_")

stem_final_2 <- stem_totals %>%
  left_join(female_dist, by = c("country", "year")) %>%
  left_join(male_dist,   by = c("country", "year")) %>%
  mutate(
    country_name = countrycode(country, origin = "iso3c", destination = "country.name.en")
  )

stem_final_2 <- stem_final_2 %>%
  select(-female_total, -male_total, -total)


write_csv(stem_final_2, "stem_final_2.csv")

all_countries_2 <- unique(stem_final_2$country)
all_years_2 <- unique(stem_final_2$year)

country_year_grid_2 <- expand.grid(
  country = all_countries,
  year = all_years
)


stem_grid_2 <- country_year_grid_2 %>%
  left_join(stem_final_2, by = c("country", "year")) %>%
  mutate(
    country_name = countrycode(country, origin = "iso3c", destination = "country.name.en")
  ) %>%
  arrange(country, year)

# Exportieren
write_csv(stem_grid_2, "stem_grid_2.csv")



