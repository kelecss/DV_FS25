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



