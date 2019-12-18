

library(tidyverse)
library(lubridate)

inspect <- read_csv("O:/OpenGov/Accela Data/Inspections/fiveYearInspections.csv")

inspect_month <- read_csv("O:/OpenGov/Accela Data/Inspections/fourMonthInspections.csv")

inspect_all <- rbind(inspect, inspect_month)


all.inspect = inspect_all %>% mutate(trade = str_sub(INSPECTION_DESC, 5, 7))
all.inspect$trade[all.inspect$trade=="BLD"] <- "Building"
all.inspect$trade[all.inspect$trade=="ELE"] <- "Electrical"
all.inspect$trade[all.inspect$trade=="ENV"] <- "Other"
all.inspect$trade[all.inspect$trade=="FIR"] <- "Other"
all.inspect$trade[all.inspect$trade=="FUE"] <- "Plumbing/Mechanical/Gas"
all.inspect$trade[all.inspect$trade=="MEC"] <- "Plumbing/Mechanical/Gas"
all.inspect$trade[all.inspect$trade=="PLU"] <- "Plumbing/Mechanical/Gas"
all.inspect$trade[all.inspect$trade=="SOI"] <- "Other"
all.inspect$trade[all.inspect$trade=="WTR"] <- "Other"
all.inspect$trade[all.inspect$trade=="ial"] <- "Other"
all.inspect$trade[all.inspect$trade=="owu"] <- "Other"

inspect_count <- all.inspect %>%
  mutate(month = month(RESULTDATE, label = TRUE)) %>%
  mutate(year = year(RESULTDATE)) %>%
  mutate(fiscal_year = ifelse(month(RESULTDATE) > 5, year(RESULTDATE) + 1, year(RESULTDATE))) %>%
  group_by(fiscal_year, year, month, JURISDICTION, trade) %>%
  count(name = "inspection_count") %>%
  ungroup()

write_csv(inspect_count, "data/inspections_count.csv", na = "", append = FALSE)


recent.permits = read_csv("O:/OpenGov/Accela Data/Permitting/fourMonthsPermitting.csv", col_types = cols(BALANCE = col_number()))
historic.permits = read_csv("O:/OpenGov/Accela Data/Permitting/fiveYearPermitting.csv", col_types = cols(BALANCE = col_number()))

all.permit = rbind(historic.permits, recent.permits)

permit <- all.permit %>%
  mutate(month = month(PERMITISSUEDATE, label = TRUE)) %>%
  mutate(year = year(PERMITISSUEDATE)) %>%
  mutate(fiscal_year = ifelse(month(PERMITISSUEDATE) > 5, year(PERMITISSUEDATE) + 1, year(PERMITISSUEDATE))) %>%
  mutate(`C/R` = ifelse(`C/R` == "C", "Commercial", "Residential")) %>%
  group_by(fiscal_year, year, month, `C/R`) %>%
  summarise(permit_count = n(), permit_fees = sum(INVOICEDFEES, na.rm = TRUE)) %>%
  ungroup()

write_csv(permit, "data/permits.csv", na = "", append = FALSE)
