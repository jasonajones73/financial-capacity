

library(tidyverse)
library(sf)
library(tigris)
library(extrafont)


guil_tracts <- tracts(state = 37, county = 81, year = 2017)

guil_tracts_sf <- guil_tracts %>%
  st_as_sf()

################################################################################
# Population

pop <- read_rds("data/population.rds")

pop_merge <- pop %>%
  select(GEOID, survey_year_2017) %>%
  rename("population" = survey_year_2017)

################################################################################
# Median Household Income

mhi <- read_rds("data/median_household_income.rds")

guil_tracts_sf %>%
  left_join(mhi) %>%
  ggplot() +
  geom_sf(aes(fill = survey_year_2017), color = "white", size = 0.3) +
  scale_fill_viridis_c("Estimate", option = "magma",
                       direction = -1,
                       labels = scales::dollar_format()) +
  labs(title = "Median Household Income by Census Tract",
       subtitle = "Guilford County, NC",
       caption = "\nSource: ACS 5 Year Estimates\nAuthor: Jason Jones") +
  cowplot::theme_map() +
  theme(text = element_text(family = "Roboto"))

ggsave(filename = "mhi.png", device = "png", path = "maps/",
       width = 10, height = 10, dpi = 320)

guil_tracts_sf %>%
  left_join(mhi) %>%
  mutate(distinction = ifelse(survey_year_2017 > 50000, "Above", "Below")) %>%
  filter(is.na(distinction) != TRUE) %>%
  ggplot() +
  geom_sf(aes(fill = distinction), color = "white", size = 0.3) +
  scale_fill_manual("Above or Below", values = c("black", "red")) +
  labs(title = "Median Household Income: Above $50,000",
       subtitle = "Guilford County, NC",
       caption = "\nSource: ACS 5 Year Estimates\nAuthor: Jason Jones") +
  cowplot::theme_map() +
  theme(text = element_text(family = "Roboto"))

ggsave(filename = "mhi_class.png", device = "png", path = "maps/",
       width = 10, height = 10, dpi = 320)

################################################################################
# Public Assistance

pa <- read_rds("data/public_assistance.rds")

guil_tracts_sf %>%
  left_join(pa) %>%
  left_join(pop_merge) %>%
  mutate(pct = round(survey_year_2017 / population, digits = 4)) %>%
  ggplot() +
  geom_sf(aes(fill = pct), color = "white", size = 0.3) +
  scale_fill_viridis_c("Percent of Tract", option = "magma",
                       direction = -1,
                       labels = scales::percent_format()) +
  labs(title = "Public Assistance by Census Tract",
       subtitle = "Guilford County, NC",
       caption = "\nSource: ACS 5 Year Estimates\nAuthor: Jason Jones") +
  cowplot::theme_map() +
  theme(text = element_text(family = "Roboto"))

ggsave(filename = "pa.png", device = "png", path = "maps/",
       width = 10, height = 10, dpi = 320)

################################################################################
# Poverty

pov <- read_rds("data/poverty.rds")

guil_tracts_sf %>%
  left_join(pov) %>%
  left_join(pop_merge) %>%
  mutate(pct = round(survey_year_2017 / population, digits = 4)) %>%
  ggplot() +
  geom_sf(aes(fill = pct), color = "white", size = 0.3) +
  scale_fill_viridis_c("Percent of Tract", option = "magma",
                       direction = -1,
                       labels = scales::percent_format()) +
  labs(title = "Poverty by Census Tract",
       subtitle = "Guilford County, NC",
       caption = "\nSource: ACS 5 Year Estimates\nAuthor: Jason Jones") +
  cowplot::theme_map() +
  theme(text = element_text(family = "Roboto"))

ggsave(filename = "pov.png", device = "png", path = "maps/",
       width = 10, height = 10, dpi = 320)

################################################################################
# Per Capita Income

pci <- read_rds("data/per_capita_income.rds")

guil_tracts_sf %>%
  left_join(pci) %>%
  ggplot() +
  geom_sf(aes(fill = survey_year_2017), color = "white", size = 0.3) +
  scale_fill_viridis_c("Estimate", option = "magma",
                       direction = -1,
                       labels = scales::dollar_format()) +
  labs(title = "Per Capita Income by Census Tract",
       subtitle = "Guilford County, NC",
       caption = "\nSource: ACS 5 Year Estimates\nAuthor: Jason Jones") +
  cowplot::theme_map() +
  theme(text = element_text(family = "Roboto"))

ggsave(filename = "pci.png", device = "png", path = "maps/",
       width = 10, height = 10, dpi = 320)

guil_tracts_sf %>%
  left_join(pci) %>%
  mutate(distinction = ifelse(survey_year_2017 > 35000, "Above", "Below")) %>%
  filter(is.na(distinction) != TRUE) %>%
  ggplot() +
  geom_sf(aes(fill = distinction), color = "white", size = 0.3) +
  scale_fill_manual("Above or Below", values = c("black", "red")) +
  labs(title = "Per Capita Income: Above $35,000",
       subtitle = "Guilford County, NC",
       caption = "\nSource: ACS 5 Year Estimates\nAuthor: Jason Jones") +
  cowplot::theme_map() +
  theme(text = element_text(family = "Roboto"))

ggsave(filename = "pci_class.png", device = "png", path = "maps/",
       width = 10, height = 10, dpi = 320)

################################################################################
# Medicaid

med <- read_rds("data/medicaid.rds")

guil_tracts_sf %>%
  left_join(med) %>%
  left_join(pop_merge) %>%
  mutate(pct = round(survey_year_2017 / population, digits = 4)) %>%
  ggplot() +
  geom_sf(aes(fill = pct), color = "white", size = 0.3) +
  scale_fill_viridis_c("Percent of Tract", option = "magma",
                       direction = -1,
                       labels = scales::percent_format()) +
  labs(title = "Medicaid by Census Tract",
       subtitle = "Guilford County, NC",
       caption = "\nSource: ACS 5 Year Estimates\nAuthor: Jason Jones") +
  cowplot::theme_map() +
  theme(text = element_text(family = "Roboto"))

ggsave(filename = "med.png", device = "png", path = "maps/",
       width = 10, height = 10, dpi = 320)
