# Estimating number of countries with/without surveys for poverty estimation
# Based off Serajuddin et al 2015
# http://documents.worldbank.org/curated/en/700611468172787967/pdf/WPS7252.pdf


setwd("C:/Users/lnoe/Documents/R")

library(tidyverse)
library(xlsx)
library(WDI)

# "the 155 countries for which the World Bank monitors poverty data using the WDI database,
# 29 countries do not have any poverty data point and 28 countries have only one poverty 
# data point."
# "A poverty data point consists of a poverty estimate in a certain year at either the 
# international poverty line of $1.25 a day or the national poverty 
# line compiled from household consumption survey data."

wdi <- WDI(indicator = c("SI.POV.DDAY", "SI.POV.NAHC"), extra = TRUE) %>%
  # Fix region and iso3c for North Macedonia and North Korea
  mutate(region = as.character(region),
         region = case_when(
           country == "North Macedonia" ~ "Europe & Central Asia",
           str_detect(country, "Korea, Dem.") ~ "East Asia & Pacific",
           TRUE ~ region),
         iso3c = as.character(iso3c),
         iso3c = case_when(
           country == "North Macedonia" ~ "MKD",
           str_detect(country, "Korea, Dem.") ~ "PRK",
           TRUE ~ iso3c
           ),
         income = as.character(income),
         income = case_when(
           country == "North Macedonia" ~ "Upper middle income",
           str_detect(country, "Korea, Dem.") ~ "Low income",
           TRUE ~ income
         ),
         income = as.factor(income),
         income = fct_relevel(income, "High income", "Upper middle income", "Lower middle income", "Low income")) %>%
  filter(region != "Aggregates")

# "This database identifies 1,101 different poverty data points across developing countries
# between 1976 and 2013, for which poverty measures were computed at national or international poverty
# lines"
wdi %>%
  filter(!is.na(SI.POV.DDAY)|!is.na(SI.POV.NAHC), year>=1976 & year <=2013) %>%
  count()
# 1342 data points in same time frame now
wdi %>%
  filter(!is.na(SI.POV.DDAY)|!is.na(SI.POV.NAHC)) %>%
  count()
# 1582 data points overall

### Constructing sample of countries
# Adding income and lending groups of all WB classified countries (218)
incgroups <- read.xlsx("Data/Input Data/WB incomegroups Jun2019.xlsx", sheetIndex = 1, startRow = 5) %>%
  filter(Region!="x", !is.na(Region)) %>%
  select(wb_country = Economy, iso3c = Code, wbregion = Region, incgroup = Income.group, lendingcat = Lending.category) %>%
  mutate(lendingcat = case_when(lendingcat == ".." ~ NA_character_,
                                TRUE ~ as.character(lendingcat)))

# Add additional countries where poverty is not 0 but incomes are high
# Use Povcal API to download all surveys
# See user info http://iresearch.worldbank.org/PovcalNet/docs/PovcalNet%20API.pdf
povcal_df <- read.csv(url("http://iresearch.worldbank.org/povcalnet/povcalnetapi.ashx?PovertyLine=1.9&Countries=all&SurveyYears=all")) %>%
  group_by(CountryCode) %>%
  # Determining by latest survey in Povcal
  filter(RequestYear == max(RequestYear)) %>%
  ungroup() %>%
  filter(HeadCount>0) %>%
  # Delete doubles, either because of multiple surveys in same year, 
  # or urban/rural split, like CHina, India, Indonesia
  distinct(CountryCode) %>%
  mutate(poverty = 1) %>%
  select(CountryCode, poverty)
# save(povcal_df, file = "Data/Output Data/povcal_Sept2019_with_pov")

# Import total number of countries in povcal
povcal_full <- read.csv(url("http://iresearch.worldbank.org/povcalnet/povcalnetapi.ashx?PovertyLine=1.9&Countries=all&SurveyYears=all")) %>%
  group_by(CountryCode) %>%
  # Keep latest survey info
  filter(RequestYear == max(RequestYear)) %>%
  ungroup() %>%
  # Get rid of countries with split urban rural (CHN, IND, IDN), as well as different survey types
  distinct(CountryCode, .keep_all = TRUE)

## Replicate filter from Serajuddin et al. to create corpus of countries
# All developing countries
incgroups %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
  count() # 138 countries

# All developing countries plus those which are still up to blend
incgroups %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat)) %>%
  count() # 148 countries

# All developing countries plus those which are still up to blend and have poverty above 0
incgroups %>%
  left_join(povcal_df, by = c("iso3c" = "CountryCode")) %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat) | poverty == 1) %>%
  count() # 170 countries
# This higher count is because high income countries are now included in
# PovcalNet with poverty above 0, even if only 0.2 % or something like that

# Summary stats about sample
##### 
# By income group
incgroups %>%
  left_join(povcal_df, by = c("iso3c" = "CountryCode")) %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat) | poverty == 1) %>%
  count(incgroup)
# By lending status
incgroups %>%
  left_join(povcal_df, by = c("iso3c" = "CountryCode")) %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat) | poverty == 1) %>%
  count(lendingcat)
# By both
incgroups %>%
  left_join(povcal_df, by = c("iso3c" = "CountryCode")) %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat) | poverty == 1) %>%
  count(incgroup, lendingcat)


#####
# Create marker of country
marker <- incgroups %>%
  left_join(povcal_df, by = c("iso3c" = "CountryCode")) %>%
  filter(incgroup %in% c("Low income", "Lower middle income", "Upper middle income") | !is.na(lendingcat) | poverty == 1) %>%
  mutate(universe = 1) %>%
  select(iso3c, universe)

#####
### Checking how many countries are in povcal and not in my sample, and reverse
# How many countries in my final sample not in povcal?
(marker %>%
  anti_join(povcal_full, by = c("iso3c" = "CountryCode")) %>%
  left_join(incgroups) %>%
  select(wb_country, iso3c, incgroup) %>%
  # incgroup breakdown of these countries (remove pound sign)
  # count(incgroup)
  write_csv("Data/Output Data/countries not in povcal.csv"))

# Do these countries have any poverty data?
wdi %>%
  inner_join(marker %>%
               anti_join(povcal_full, by = c("iso3c" = "CountryCode")) %>%
               left_join(incgroups) %>%
               select(wb_country, iso3c, incgroup)) %>%
  mutate(survey_present = case_when(
    !is.na(SI.POV.DDAY) | !is.na(SI.POV.NAHC) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(iso3c, wb_country) %>%
  summarize(any_data = sum(survey_present, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(any_data > 0)

# How many countries in povcal but not in final sample?
(povcal_full %>%
  anti_join(marker, by = c("CountryCode" = "iso3c")) %>%
  left_join(incgroups, by = c("CountryCode" = "iso3c")) %>%
  select(wb_country, CountryCode, incgroup) %>%
  write_csv("Data/Output Data/Povcal countries not in sample.csv"))

#####

# Use marker to cut wdi dataset down and create binary for evidence of survey
working_data <- wdi %>%
  inner_join(marker) %>%
  mutate(survey_present = case_when(
    !is.na(SI.POV.DDAY) | !is.na(SI.POV.NAHC) ~ 1,
    TRUE ~ 0
  ))

# How many countries do we have now? Should be same as in marker
working_data %>% 
  distinct(iso3c) %>% 
  count() # 170 countries, good

working_data %>%
  filter(survey_present == 1, year>=1976 & year <=2013) %>%
  count() # 1227 obs with surveys

working_data %>%
  filter(survey_present == 1) %>%
  count() # 1451 obs with surveys

# Replicate Figure 1 in paper. Number of surveys per year per region
working_data %>%
  filter(year >= 1976) %>%
  group_by(year, region) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = sum_surveys, fill = region)) +
  geom_col() + 
  scale_x_continuous(breaks = c(1976, 1977, 1978, 1979, 1980, 1981, 
                                1982, 1983, 1984, 1985, 1986, 1987, 
                                1988, 1989, 1990, 1991, 1992, 1993, 
                                1994, 1995, 1996, 1997, 1998, 1999, 
                                2000, 2001, 2002, 2003, 2004, 2005, 
                                2006, 2007, 2008, 2009, 2010, 2011, 
                                2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
  labs(y= "Number of Poverty Datapoints", x = "", title = "Number of Poverty Datapoints since 1976") +
  theme(axis.text.x = element_text(angle = 90, size = 6.5, hjust = 1, vjust = 0.5),
        legend.title = element_blank(), legend.position = c(0.25, 0.7), 
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Graphs/Number of Poverty Datapoints.png", dpi = 600)

# Latest 10 year period with complete data is likely 2006-2015
# How many countries either 0 or 1 survey in ten year span?
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0,1)) %>%
  count() # 55 countries with 0 or 1 survey only in latest full ten year period

# How many 0 survey?
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0)) %>%
  count() # 20 countries with 0 survey in latest full ten year period

survey_0 <- working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0)) %>%
  mutate(category = "Extreme Data Deprivation (0 surveys)") %>%
  select(-sum_surveys)

# How many 1 survey?
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(1)) %>%
  count() # 35 countries with 1 survey in latest full ten year period

survey_1 <- working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(1)) %>%
  mutate(category = "Moderate Data Deprivation (1 survey)") %>%
  select(-sum_surveys)

# Latest 10 year period 2006-2015 0 or 1 survey region
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c, region) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0,1)) %>%
  count(region)

# How many countries with 2 surveys but more than 5 years apart
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(2)) %>%
  count() # 34 countries with 2 surveys in latest full ten year period

working_data %>%
  filter(year>=2006 & year<=2015) %>%
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
  count(vuln) # 12 countries vulnerable, 22 countries satisfy minimum req

survey_2 <- working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  mutate(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(2), survey_present == 1) %>%
  group_by(iso3c) %>%
  summarize(year2 = max(year, na.rm = TRUE), year1 = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff = year2 - year1, category = case_when(
    diff>5 ~ "Vulnerable to Data Deprivation, >5 yr btw surveys",
    TRUE ~ "Satisfy minimum requirement, <=5 yrs btw surveys"
  )) %>%
  select(iso3c, category)

# How many countries with satisfactory data needs?
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>% 
  mutate(satisfactory = case_when(
    sum_surveys >= 3 ~ 1,
    TRUE ~ 0
  )) %>%
  count(satisfactory)

survey_3 <- working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>% 
  mutate(category = case_when(
    sum_surveys >= 3 ~ "Satisfactory for data needs, >=3 data pts.",
    TRUE ~ "Unsatisfactory"
  )) %>%
  filter(sum_surveys >= 3) %>%
  select(iso3c, category)

countries_06_15 <- survey_0 %>%
  rbind(survey_1) %>%
  rbind(survey_2) %>%
  rbind(survey_3) %>%
  rename(cat_06_15 = category) %>%
  mutate(deprivation_06_15 = case_when(cat_06_15 %in% c(
    "Extreme Data Deprivation (0 surveys)", "Moderate Data Deprivation (1 survey)",
    "Vulnerable to Data Deprivation, >5 yr btw surveys") ~ 1,
    TRUE ~ 0
  ))

# For the period of 2006-2015, the five categories according to Serajuddin et al look as follows
# Extreme data deprivation (0 surveys): 20
# Moderate data deprivation (1 survey): 35
# Vulnerable to data deprivation (2 surveys more than 5 years apart: 12
# Satisfy minimum requirement (2 surveys less than 5 years apart): 22
# Satisfactory for data needs (3 surveys or more per 10 years): 81


#### Replicate for period 2002-2011 as in original paper
# How many countries either 0 or 1 survey in ten year span?
working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0,1)) %>%
  count() # 53 countries with 0 or 1 survey only

# How many 0 survey?
working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0)) %>%
  count() # 21 countries with 0 surveys

survey_0 <- working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0)) %>%
  mutate(category = "Extreme Data Deprivation (0 surveys)") %>%
  select(-sum_surveys)

# How many 1 survey?
working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(1)) %>%
  count() # 32 countries with 1 survey

survey_1 <- working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(1)) %>%
  mutate(category = "Moderate Data Deprivation (1 survey)") %>%
  select(-sum_surveys)

# Latest 10 year period 2006-2015 0 or 1 survey region
working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c, region) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0,1)) %>%
  count(region)

# How many countries with 2 surveys but more than 5 years apart
working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(2)) %>%
  count() # 36 countries with 2 surveys

working_data %>%
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
  count(vuln) # 17 countries vulnerable, 19 countries satisfy minimum req

survey_2 <- working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  mutate(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(2), survey_present == 1) %>%
  group_by(iso3c) %>%
  summarize(year2 = max(year, na.rm = TRUE), year1 = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff = year2 - year1, category = case_when(
    diff>5 ~ "Vulnerable to Data Deprivation, >5 yr btw surveys",
    TRUE ~ "Satisfy minimum requirement, <=5 yrs btw surveys"
  )) %>%
  select(iso3c, category)

# How many countries with satisfactory data needs?
working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>% 
  mutate(satisfactory = case_when(
    sum_surveys >= 3 ~ 1,
    TRUE ~ 0
  )) %>%
  count(satisfactory)

survey_3 <- working_data %>%
  filter(year>=2002 & year<=2011) %>%
  group_by(iso3c) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>% 
  mutate(category = case_when(
    sum_surveys >= 3 ~ "Satisfactory for data needs, >=3 data pts.",
    TRUE ~ "Unsatisfactory"
  )) %>%
  filter(sum_surveys >= 3) %>%
  select(iso3c, category)

countries_02_11 <- survey_0 %>%
  rbind(survey_1) %>%
  rbind(survey_2) %>%
  rbind(survey_3) %>%
  rename(cat_02_11 = category) %>%
  mutate(deprivation_02_11 = case_when(cat_02_11 %in% c(
    "Extreme Data Deprivation (0 surveys)", "Moderate Data Deprivation (1 survey)",
    "Vulnerable to Data Deprivation, >5 yr btw surveys") ~ 1,
    TRUE ~ 0
  ))

# For the period of 2002-2011, the five categories according to Serajuddin et al look as follows
# Extreme data deprivation (0 surveys): 21
# Moderate data deprivation (1 survey): 32
# Vulnerable to data deprivation (2 surveys more than 5 years apart: 17
# Satisfy minimum requirement (2 surveys less than 5 years apart): 19
# Satisfactory for data needs (3 surveys or more per 10 years): 81

#####
# Churn between time periods
both_periods <- countries_06_15 %>%
  left_join(countries_02_11)

# Which countries are now data deprived and weren't before?
both_periods %>%
  filter(deprivation_02_11 == 0 & deprivation_06_15 == 1) %>%
  select(iso3c, cat_02_11, cat_06_15)

# Which countries were data deprived and now aren't?  
both_periods %>%
  filter(deprivation_02_11 == 1 & deprivation_06_15 == 0) %>%
  select(iso3c, cat_02_11, cat_06_15)

# Which countries didn't move categories
both_periods %>%
  filter(cat_02_11 == cat_06_15) %>%
  count() # 131 didn't move

# Which countries did move categories
both_periods %>%
  filter(cat_02_11 != cat_06_15) %>%
  select(iso3c, cat_02_11, cat_06_15) %>%
  write_csv("Data/Output Data/Changed categories.csv")

# From footnote 15 and 16 for extremely and moderately data deprived countries from original study
orig_countries_02_11 <- read_csv("Data/Input Data/Serajuddin et al original countries.csv") %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(
           country == "Korea, Dem. Rep." ~ "PRK",
           TRUE ~ iso3c
         ),
         orig_deprivation_02_11 = 1)

compare_old_study <- both_periods %>%
  left_join(orig_countries_02_11) %>%
  mutate(cat_02_11 = str_trim(cat_02_11),
         cat_06_15 = str_trim(cat_06_15),
         num_cat_02_11 = case_when(
           cat_02_11 == "Extreme Data Deprivation (0 surveys)" ~ 1,
           cat_02_11 == "Moderate Data Deprivation (1 survey)" ~ 2,
           cat_02_11 == "Vulnerable to Data Deprivation, >5 yr btw surveys" ~ 3,
           cat_02_11 == "Satisfy minimum requirement, <=5 yrs btw surveys" ~ 4,
           cat_02_11 == "Satisfactory for data needs, >=3 data pts." ~ 5,
           TRUE ~ NA_real_
         ),
         num_cat_06_15 = case_when(
           cat_06_15 == "Extreme Data Deprivation (0 surveys)" ~ 1,
           cat_06_15 == "Moderate Data Deprivation (1 survey)" ~ 2,
           cat_06_15 == "Vulnerable to Data Deprivation, >5 yr btw surveys" ~ 3,
           cat_06_15 == "Satisfy minimum requirement, <=5 yrs btw surveys" ~ 4,
           cat_06_15 == "Satisfactory for data needs, >=3 data pts." ~ 5,
           TRUE ~ NA_real_
         ))

# Which countries were data deprived and are still for same period?
compare_old_study %>%
  filter(orig_deprivation_02_11 == 1 & deprivation_02_11 == 1) %>%
  select(iso3c, orig_cat_02_11, cat_02_11, cat_06_15)
# 52 countries

# Which countries were data deprived and are no longer for that period
compare_old_study %>%
  filter(orig_deprivation_02_11 == 1 & deprivation_02_11 == 0) %>%
  select(iso3c, orig_cat_02_11, cat_02_11, cat_06_15) %>%
  write_csv("Data/Output Data/no longer data deprived.csv")
# 5 countries

# Which countries were data deprived for 2002-2011 no matter the data
# and are no longer in most recent period (2006-2015)
compare_old_study %>%
  filter(orig_deprivation_02_11 == 1 & deprivation_02_11 == 1 & deprivation_06_15 == 0) %>%
  select(iso3c, orig_cat_02_11, cat_02_11, cat_06_15) %>%
  write_csv("Data/Output Data/no longer data deprived0615.csv")

# Which countries were data deprived and are still for the most recent period?
compare_old_study %>%
  filter(orig_deprivation_02_11 == 1 & deprivation_06_15 == 1) %>%
  select(iso3c, orig_cat_02_11, cat_02_11, cat_06_15) %>%
  count() # 47 countries

# Which (57) countries switched categories?
compare_old_study %>%
  filter(num_orig_cat_02_11 != num_cat_02_11) %>%
  count()
# 15 countries whose category changed for same period with updated data

compare_old_study %>%
  filter(num_orig_cat_02_11 != num_cat_06_15) %>%
  count()
# 24 countries whose category changed for updated data between 02-11 and 06-15
# Some of these countries were already covered by looking at data deprivation

compare_old_study %>%
  filter((num_orig_cat_02_11 != num_cat_02_11) & (orig_deprivation_02_11 == deprivation_02_11)) %>%
  count()
# 10 of the above 15 who changed category but didn't change data deprivation status
compare_old_study %>%
  filter((num_orig_cat_02_11 != num_cat_02_11) & (orig_deprivation_02_11 == deprivation_02_11)) %>%
  select(iso3c, orig_cat_02_11, cat_02_11) %>%
  write_csv("Data/Output Data/countries 02-11 change in status but still data deprived.csv")

compare_old_study %>%
  filter((num_orig_cat_02_11 != num_cat_06_15) & (orig_deprivation_02_11 == deprivation_06_15)) %>%
  select(iso3c, orig_cat_02_11, cat_06_15)

compare_old_study %>%
  filter((num_orig_cat_02_11 != num_cat_06_15) & (orig_deprivation_02_11 != deprivation_06_15)) %>%
  select(iso3c, orig_cat_02_11, cat_06_15)

#####
library(viridis)
library(networkD3)
library(hrbrthemes)
library(circlize)
### Sankey chart!
# From first example here https://www.data-to-viz.com/graph/sankey.html
sankey <- both_periods %>% 
  count(cat_02_11, cat_06_15) %>%
  rename(source = cat_02_11, target = cat_06_15, value = n) %>%
  mutate(value = as.double(value), short_source = case_when(
    source == "Extreme Data Deprivation (0 surveys)" ~ "Extreme: 0 surveys",
    source == "Moderate Data Deprivation (1 survey)" ~ "Moderate: 1 survey",
    source == "Vulnerable to Data Deprivation, >5 yr btw surveys" ~ "Vulnerable: 2 surveys, >5yr apart",
    source == "Satisfy minimum requirement, <=5 yrs btw surveys" ~ "Minimum: 2 surveys, <=5yr apart",
    source == "Satisfactory for data needs, >=3 data pts." ~ "Satisfactory: 3 surveys or more",
    TRUE ~ NA_character_), short_target = case_when(
      target == "Extreme Data Deprivation (0 surveys)" ~ "Extreme: 0 surveys",
      target == "Moderate Data Deprivation (1 survey)" ~ "Moderate: 1 survey",
      target == "Vulnerable to Data Deprivation, >5 yr btw surveys" ~ "Vulnerable: 2 surveys, >5yr apart",
      target == "Satisfy minimum requirement, <=5 yrs btw surveys" ~ "Minimum: 2 surveys, <=5yr apart",
      target == "Satisfactory for data needs, >=3 data pts." ~ "Satisfactory: 3 surveys or more",
      TRUE ~ NA_character_),
    target = str_c(target, " "), short_target = str_c(short_target, " "))

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes_cat <- data.frame(name=c(as.character(sankey$short_source), as.character(sankey$short_target)) %>% unique()) %>%
  mutate(name = fct_relevel(name, "Satisfactory: 3 surveys or more",
                            "Minimum: 2 surveys, <=5yr apart",
                            "Vulnerable: 2 surveys, >5yr apart",
                            "Moderate: 1 survey",
                            "Extreme: 0 surveys",
                            "Satisfactory: 3 surveys or more ",
                            "Minimum: 2 surveys, <=5yr apart ",
                            "Vulnerable: 2 surveys, >5yr apart ",
                            "Moderate: 1 survey ",
                            "Extreme: 0 surveys ")) %>%
  arrange(factor(name))

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
sankey$IDsource=match(sankey$short_source, nodes_cat$name)-1 
sankey$IDtarget=match(sankey$short_target, nodes_cat$name)-1

# prepare colour scale: Goes from red (bad) #1a9641 to green (good) #d7191c
ColourScal ='d3.scaleOrdinal().range(["#1A9641","#a6d96a","#ffffbf","#fdae61","#d7191c"])'

# Make the Network
sankeyNetwork(Links = sankey, Nodes = nodes_cat,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", iterations = 0,
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, 
              fontSize=13, nodePadding=20)


#####
# Comparing 2002-2011 to 2006-2015 timeframe bars
compare <- data.frame("category" = c("No Data Point (Extreme Data Deprivation)", 
                                     "1 Data Point (Moderate Data Deprivation)", 
                                     "2 Data Points (Interval of 6 years or Longer,\n    vulnerable to Data Deprivation)", 
                                     "2 Data Points (Interval of 5 years or shorter,\n    satisfies a minimum requirement for Data Needs", 
                                     "3 or More Data Points (Satisfactory for Data Needs)"),
                      "yr2002_2011" = c(21, 32, 17, 19, 81),
                      "yr2006_2015" = c(20, 35, 12, 22, 81)) %>%
  # write_csv("Data/Output Data/Serajuddin comparisons.csv")
  gather(period, num_countries, yr2002_2011:yr2006_2015) %>%
  mutate(period = str_remove(period, "yr"),
         period = str_replace(period, "_", "-"),
         category = fct_relevel(category, "3 or More Data Points (Satisfactory for Data Needs)",
                                "2 Data Points (Interval of 5 years or shorter,\n    satisfies a minimum requirement for Data Needs",
                                "2 Data Points (Interval of 6 years or Longer,\n    vulnerable to Data Deprivation)", 
                                "1 Data Point (Moderate Data Deprivation)", 
                                "No Data Point (Extreme Data Deprivation)"))

compare %>%
  ggplot(aes(x = period, y = num_countries, fill = category, label = num_countries)) +
  geom_col() +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(y = "Number of countries", x = "", title = "Original study timeframe versus update") +
  scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
  theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.8, "cm"))
ggsave("Graphs/Serajuddin charts comparison.png", dpi = 600)  


#### Regional breakdown for 2006-2015
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c, region) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0)) %>%
  count(region)

working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c, region) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(1)) %>%
  count(region)

working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  mutate(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(2), survey_present == 1) %>%
  group_by(iso3c, region) %>%
  summarize(year2 = max(year, na.rm = TRUE), year1 = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff = year2 - year1, vuln = case_when(
    diff>5 ~ 1,
    TRUE ~ 0
  )) %>%
  count(region, vuln) # 12 countries vulnerable, 22 countries satisfy minimum req

# How many countries with satisfactory data needs?
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c, region) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>% 
  mutate(satisfactory = case_when(
    sum_surveys >= 3 ~ 1,
    TRUE ~ 0
  )) %>%
  count(region, satisfactory)


# Number of datapoints per income group
working_data %>%
  filter(year >= 1976) %>%
  group_by(year, income) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = sum_surveys, fill = income)) +
  geom_col() + 
  scale_x_continuous(breaks = c(1976, 1977, 1978, 1979, 1980, 1981, 
                                1982, 1983, 1984, 1985, 1986, 1987, 
                                1988, 1989, 1990, 1991, 1992, 1993, 
                                1994, 1995, 1996, 1997, 1998, 1999, 
                                2000, 2001, 2002, 2003, 2004, 2005, 
                                2006, 2007, 2008, 2009, 2010, 2011, 
                                2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
  labs(y= "Number of Poverty Datapoints", x = "", title = "Number of Poverty Datapoints since 1976") +
  theme(axis.text.x = element_text(angle = 90, size = 6.5, hjust = 1, vjust = 0.5),
        legend.title = element_blank(), legend.position = c(0.25, 0.7), 
        legend.text = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Graphs/Number of Poverty Datapoints by income group.png", dpi = 600)


#### Income group breakdown for 2006-2015
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c, income) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(0)) %>%
  count(income)

working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c, income) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(1)) %>%
  count(income)

working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c) %>%
  mutate(sum_surveys = sum(survey_present)) %>%
  ungroup() %>%
  filter(sum_surveys %in% c(2), survey_present == 1) %>%
  group_by(iso3c, income) %>%
  summarize(year2 = max(year, na.rm = TRUE), year1 = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff = year2 - year1, vuln = case_when(
    diff>5 ~ 1,
    TRUE ~ 0
  )) %>%
  count(income, vuln) # 12 countries vulnerable, 22 countries satisfy minimum req

# How many countries with satisfactory data needs?
working_data %>%
  filter(year>=2006 & year<=2015) %>%
  group_by(iso3c, income) %>%
  summarize(sum_surveys = sum(survey_present)) %>%
  ungroup() %>% 
  mutate(satisfactory = case_when(
    sum_surveys >= 3 ~ 1,
    TRUE ~ 0
  )) %>%
  count(income, satisfactory)
