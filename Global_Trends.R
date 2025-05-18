library(readr)
library(dplyr)

stem_grid_2 <- read_csv("stem_grid_2.csv")

global_trend <- stem_grid_2 %>%
  group_by(year) %>%
  summarise(
    avg_pct_female_stem = mean(pct_female_stem, na.rm = TRUE)
  ) %>%
  arrange(year)

View(global_trend)

write.csv(global_trend, "global_trend.csv", row.names = FALSE)


