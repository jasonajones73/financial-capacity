## Purpose: Cleaning educational attainment data for proper use
## Author: Jason Jones
## Date: 2020-04-11

# Load packages ----
library(tidyverse)

# Load data ----
ed_dat <- read_rds("data/educational_attainment/ed_attain_county.rds")
ed_dat_pl <- read_rds("data/educational_attainment/ed_attain_places.rds")
ed_dat_tracts <- read_rds("data/educational_attainment/ed_attain_tracts.rds")

# Initial reformatting
ed_dat <- ed_dat %>%
  select(-moe) %>%
  pivot_wider(names_from = c(GEOID, NAME), values_from = estimate,) %>%
  fill(concept, .direction = "down")

ed_dat_pl <- ed_dat_pl %>%
  select(-moe) %>%
  pivot_wider(names_from = c(GEOID, NAME), values_from = estimate,) %>%
  fill(concept, .direction = "down")

ed_dat_tracts <- ed_dat_tracts %>%
  select(-moe) %>%
  pivot_wider(names_from = c(GEOID, NAME), values_from = estimate,) %>%
  fill(concept, .direction = "down")

# Final cleaning and reformatting to group and display neatly
ed_dat <- ed_dat %>%
  mutate(variable = str_remove_all(variable, "[:alpha:]|[:punct:]")) %>%
  mutate(label = str_extract(label, "(?<=!!).+")) %>%
  mutate(label = str_extract(label, "(?<=!!).+")) %>%
  mutate(label = case_when(variable == "15002001" ~ "Total Population",
                           variable != "15002001" ~ label)) %>%
  filter(!(is.na(label))) %>%
  select(-variable) %>%
  group_by(concept, label) %>%
  summarise_all(.funs = sum) %>%
  ungroup() %>%
  pivot_longer(cols = 3:11, names_to = c("GEOID", "county"), names_sep = "_", values_to = "estimate")

ed_dat <- ed_dat %>%
  filter(label == "Total Population") %>%
  rename(total_pop = estimate) %>%
  select(GEOID, concept, total_pop) %>%
  left_join(ed_dat)

ed_dat_pl <- ed_dat_pl %>%
  mutate(variable = str_remove_all(variable, "[:alpha:]|[:punct:]")) %>%
  mutate(label = str_extract(label, "(?<=!!).+")) %>%
  mutate(label = str_extract(label, "(?<=!!).+")) %>%
  mutate(label = case_when(variable == "15002001" ~ "Total Population",
                           variable != "15002001" ~ label)) %>%
  filter(!(is.na(label))) %>%
  select(-variable) %>%
  group_by(concept, label) %>%
  summarise_all(.funs = sum) %>%
  ungroup() %>%
  pivot_longer(cols = 3:9, names_to = c("GEOID", "place"), names_sep = "_", values_to = "estimate")

ed_dat_pl <- ed_dat_pl %>%
  filter(label == "Total Population") %>%
  rename(total_pop = estimate) %>%
  select(GEOID, concept, total_pop) %>%
  left_join(ed_dat_pl)

ed_dat_tracts <- ed_dat_tracts %>%
  mutate(variable = str_remove_all(variable, "[:alpha:]|[:punct:]")) %>%
  mutate(label = str_extract(label, "(?<=!!).+")) %>%
  mutate(label = str_extract(label, "(?<=!!).+")) %>%
  mutate(label = case_when(variable == "15002001" ~ "Total Population",
                           variable != "15002001" ~ label)) %>%
  filter(!(is.na(label))) %>%
  select(-variable) %>%
  group_by(concept, label) %>%
  summarise_all(.funs = sum) %>%
  ungroup() %>%
  pivot_longer(cols = 3:121, names_to = c("GEOID", "tract"), names_sep = "_", values_to = "estimate")

ed_dat_tracts <- ed_dat_tracts %>%
  filter(label == "Total Population") %>%
  rename(total_pop = estimate) %>%
  select(GEOID, concept, total_pop) %>%
  left_join(ed_dat_tracts)

# Compute percentages to standardize across jurisdictions
ed_dat <- ed_dat %>%
  mutate(pct = estimate / total_pop)

ed_dat_pl <- ed_dat_pl %>%
  mutate(pct = estimate / total_pop)

ed_dat_tracts <- ed_dat_tracts %>%
  mutate(pct = estimate / total_pop)

# Write out clean data objects
write_rds(ed_dat, "data/educational_attainment/ed_attain_county_cleaned.rds")
write_csv(ed_dat, "data/educational_attainment/ed_attain_county_cleaned.csv")

write_rds(ed_dat_tracts, "data/educational_attainment/ed_attain_tracts_cleaned.rds")
write_csv(ed_dat_tracts, "data/educational_attainment/ed_attain_tracts_cleaned.csv")

write_rds(ed_dat_pl, "data/educational_attainment/ed_attain_places_cleaned.rds")
write_csv(ed_dat_pl, "data/educational_attainment/ed_attain_places_cleaned.csv")
