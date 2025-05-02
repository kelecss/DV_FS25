# DV_FS25: Final Project
Authors: Emmanuel & Kerim
Supervisor: Dr. Charlotte Cabane
Course: Data visualization Workshop (FS25)

## Project Description and Goal
TBD

## Conclusion
TBD

## Data Description
### stem_disaggregated.csv
This dataset contains the percentages of female graduates within each STEM field (Natural Sciences, ICT, Engineering) by country
and year.

Variables:
- country: ISO 3-letter country code (e.g., DEU for Germany)
- year: Year of observation
- field_of_study: One of "NSMS" (Natural Sciences, Mathematics and Statistics), "ICT" (Information and Communication Technology), or 
"ECM" (Engineering, Manufacturing and Construction)
pct_female_per_field: Percentage of graduates in the field who are female (0-100%)

### stem_final.csv
This dataset summarizes the overall percentage of women among all STEM graduates (combined across Natural Sciences, ICT, 
and Engineering) by country and year. It is calculated as the proportion of female STEM graduates relative to the total number of 
STEM graduates (female + male).

Variables:
- country: ISO 3-letter country code (e.g., DEU for Germany)
- year: Year of observation
- pct_female_stem: Percentage of all STEM graduates who are women (0–100%)

### Datasource
### stem_disaggregated.csv and stem_final.csv
UNESCO Institute for Statistics (2025), 
retrieved from http://data.uis.unesco.org/, based on the indicators for tertiary
education graduates by field of study and sex.
