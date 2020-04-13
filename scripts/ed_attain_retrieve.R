## Purpose: Retrieve Educational Attainment data from census API
## Author: Jason Jones

# Load packages ----
library(tidyverse)
library(tidycensus)
library(sf)

# Retrieve API key environment variable ----
source("api_key/api_key.R")

# Check available variables and load for string extraction ----
vars <- load_variables(year = 2018, dataset = "acs1")

# Store some constants for recycling ----
cen_var <- "B15002"

comp_counties <- c("45045", "37081", "37067", "37183",
                   "37063", "37119", "37129", "45083", "47065")

comp_places <- c("3712000", "3719000", "3728000", "3731400", "3755000",
                 "3774440", "3775000")

# Per Capita Income detailed table ----
## Grab vector of variable labels from Census object
acs_vars <- vars %>%
  filter(str_starts(name, cen_var)) %>%
  pull(name)

## Retrieve data for NC, SC, & TN
detailed_county <- map_df(.x = acs_vars,
                          .f = ~get_acs(geography = "county", variables = .,
                                        year = 2018, state = c(45, 37, 47), key = api_key,
                                        survey = "acs1"))

## Filter for only comparison counties stored in constant
detailed_county <- detailed_county %>%
  filter(GEOID %in% comp_counties)

## Retrieve data for CDP's
detailed_place <- map_df(.x = acs_vars,
                         .f = ~get_acs(geography = "place", variables = .,
                                       year = 2018, state = 37, key = api_key,
                                       survey = "acs1"))

## Filter for only comparison places stored in constant
detailed_place <- detailed_place %>%
  filter(GEOID %in% comp_places)

## Retrieve acs5 data for merging with census tracts
tracts <- get_acs(geography = "tract", table = cen_var, state = 37,
                  county = 81, survey = "acs5", key = api_key)

# Add detail from variables object and clean up labels ----
county_deatiled_clean <- detailed_county %>%
  left_join(rename(vars, variable = name)) %>%
  mutate(concept = str_extract(concept, pattern = "\\(([^)]+)\\)$")) %>%
  mutate(concept = str_remove_all(concept, "\\(|\\)")) %>%
  mutate(concept = str_to_title(concept)) %>%
  mutate(concept = case_when(variable == paste0(cen_var, "_001") ~ "All Population",
                             variable != paste0(cen_var, "_001") ~ concept)) %>%
  mutate(label = str_extract(label, "(?<=!!).+"))

places_detailed_clean <- detailed_place %>%
  left_join(rename(vars, variable = name)) %>%
  mutate(concept = str_extract(concept, pattern = "\\(([^)]+)\\)$")) %>%
  mutate(concept = str_remove_all(concept, "\\(|\\)")) %>%
  mutate(concept = str_to_title(concept)) %>%
  mutate(concept = case_when(variable == paste0(cen_var, "_001") ~ "All Population",
                             variable != paste0(cen_var, "_001") ~ concept)) %>%
  mutate(label = str_extract(label, "(?<=!!).+"))

tracts_clean <- tracts %>%
  left_join(rename(vars, variable = name)) %>%
  mutate(concept = str_extract(concept, pattern = "\\(([^)]+)\\)$")) %>%
  mutate(concept = str_remove_all(concept, "\\(|\\)")) %>%
  mutate(concept = str_to_title(concept)) %>%
  mutate(concept = case_when(variable == paste0(cen_var, "_001") ~ "All Population",
                             variable != paste0(cen_var, "_001") ~ concept)) %>%
  mutate(label = str_extract(label, "(?<=!!).+"))

# Write data from retrieval
write_rds(county_deatiled_clean, "data/educational_attainment/ed_attain_county.rds")
write_rds(places_detailed_clean, "data/educational_attainment/ed_attain_places.rds")
write_rds(tracts_clean, "data/educational_attainment/ed_attain_tracts.rds")

write_csv(county_deatiled_clean, "data/educational_attainment/ed_attain_county.csv")
write_csv(places_detailed_clean, "data/educational_attainment/ed_attain_places.csv")
write_csv(tracts_clean, "data/educational_attainment/ed_attain_tracts.csv")

