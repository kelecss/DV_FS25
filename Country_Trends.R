library(dplyr)
library(readr)

stem_grid_2 <- read_csv("stem_grid_2.csv")


# Calculate each country's trend
country_trends <- stem_grid_2 %>%
  group_by(country_name) %>%
  summarise(
    start = first(pct_female_stem[!is.na(pct_female_stem)]),
    end = last(pct_female_stem[!is.na(pct_female_stem)]),
    pct_change = end - start
  )

country_trends <- country_trends %>%
  mutate(change_binned = case_when(
    pct_change > 20 ~ "8 > 20% increase",
    pct_change > 10 ~ "7 10–20% increase",
    pct_change > 5  ~ "6 5–10% increase",
    pct_change > 0  ~ "5 < 5% increase",
    pct_change == 0 ~ "4 No change",
    pct_change > -5 ~ "3 < 5% decrease",
    pct_change > -10 ~ "2 5–10% decrease",
    pct_change > -20 ~ "1 10–20% decrease",
    pct_change <= -20 ~ "0 > 20% decrease"
  ))





write.csv(country_trends, "country_trends.csv", row.names = FALSE)

