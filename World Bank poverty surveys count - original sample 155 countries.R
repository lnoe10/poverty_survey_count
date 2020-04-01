library(tidyverse)
library(xlsx)

setwd("C:/Users/lnoe/Documents/R")

# From here, as the country and lending page looked in October 2014 
# https://web.archive.org/web/20141019014228/http://data.worldbank.org/about/country-and-lending-groups
incgroups2014 <- read.xlsx("Data/Input Data/WB incomegroups Jul2014.xlsx", sheetIndex = 1, startRow = 5) %>%
  filter(Region!="x", !is.na(Region)) %>%
  select(wb_country = Economy, iso3c = Code, wbregion = Region, incgroup = Income.group, lendingcat = Lending.category) %>%
  mutate(lendingcat = case_when(lendingcat == ".." ~ NA_character_,
                                TRUE ~ as.character(lendingcat)))

incgroups2014 %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
  count() # 139 countries

# All developing countries plus those which are still up to blend
incgroups2014 %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat)) %>%
  count() # 149 countries

# All developing countries plus those which are still up to blend and
# countries where poverty is above 0: Czech Republic, Estonia, Latvia, Lithuania, Slovak Republic and Slovenia
# country list ^ From Serajuddin 2015 
incgroups2014 %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat) | iso3c %in% c("CZE", "EST", "LVA", "LTU", "SVK", "SVN")) %>%
  count() # 155 countries

# Export
incgroups2014 %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat) | iso3c %in% c("CZE", "EST", "LVA", "LTU", "SVK", "SVN")) %>%
  select(wb_country, iso3c) %>%
  mutate(iso3c = as.character(iso3c), iso3c = case_when(
    # Updating iso3, since WBG changed these
    iso3c == "KSV" ~ "XKX",
    iso3c == "ROM" ~ "ROU",
    iso3c == "TMP" ~ "TLS",
    iso3c == "WBG" ~ "PSE",
    iso3c == "ZAR" ~ "COD",
    TRUE ~ iso3c
  )) %>%
  write_csv("Data/Output Data/Original sample of 155 countries.csv")

marker2014 <- incgroups2014 %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat) | iso3c %in% c("CZE", "EST", "LVA", "LTU", "SVK", "SVN")) %>%
  mutate(universe = 1, iso3c = as.character(iso3c), iso3c = case_when(
    # Updating iso3, since WBG changed these
    iso3c == "KSV" ~ "XKX",
    iso3c == "ROM" ~ "ROU",
    iso3c == "TMP" ~ "TLS",
    iso3c == "WBG" ~ "PSE",
    iso3c == "ZAR" ~ "COD",
    TRUE ~ iso3c
  )) %>%
  select(iso3c, universe)

wdi2014 <- read.xlsx("Data/Input Data/WDI archives extract Nov2014.xlsx", sheetIndex = 1) %>%
  select(country = Country.Name, iso3c = Country.Code, year = Time, SI.POV.NAHC = Poverty.headcount.ratio.at.national.poverty.lines....of.population...SI.POV.NAHC.,
         SI.POV.DDAY = Poverty.headcount.ratio.at..1.90.a.day..2011.PPP.....of.population...SI.POV.DDAY.)

wdi2014 %>%
  filter(!is.na(SI.POV.DDAY)|!is.na(SI.POV.NAHC), year>=1976 & year <=2013) %>%
  count()

# Use marker to cut wdi dataset down and create binary for evidence of survey
working_data2014 <- wdi2014 %>%
  inner_join(marker2014) %>%
  mutate(survey_present = case_when(
    !is.na(SI.POV.DDAY) | !is.na(SI.POV.NAHC) ~ 1,
    TRUE ~ 0
  ))

# How many observations for poverty? Is it because something was updated
# in October 2014 and this is November 2014 data?
working_data2014 %>%
  filter(!is.na(SI.POV.DDAY)|!is.na(SI.POV.NAHC), year>=1976 & year <=2013) %>%
  count() # 1,119

# Latest 10 year period with complete data is likely 2006-2015
# How many countries either 0 or 1 survey in ten year span?
working_data2014 %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0,1,2)) %>%
  count() # 55 countries with 0 or 1 survey only in latest full ten year period

working_data2014 %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  mutate(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(2), survey_present == 1) %>%
  group_by(iso3c) %>%
  summarize(year2 = max(year, na.rm = TRUE), year1 = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff = year2 - year1, vuln = case_when(
    diff>5 ~ 1,
    TRUE ~ 0
  )) %>%
  count(vuln)

orig_countries_02_11 <- read_csv("Data/Input Data/Serajuddin et al original countries.csv") %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"))
