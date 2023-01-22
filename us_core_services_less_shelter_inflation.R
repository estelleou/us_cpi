#script calculates core Services less shelter CPI 
library(tidyverse)
library(lubridate)
library(scales)

#pull relative importance data and cpi time series ---------------------------

source("clean_historical_cpi_relative_importance.R")

#cpi time series 
raw_time_series<-
  #all items
  read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.1.allitems', col_type = 'c') %>% 
  #"us food and beverages",
  full_join(read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.11.USFoodBeverage',  col_type = 'c')) %>%
  #us commodities services special aggregate,
  full_join( read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.20.USCommoditiesServicesSpecial',  col_type = 'c')) %>%
  #"us transportation",
  full_join( read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation',   col_type = 'c')) %>%
  #"us housing,
  full_join( read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.12.USHousing',  col_type = 'c') )%>%
  #more items that were dropped",
  full_join(  read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.0.Current',  col_type = 'c')) %>%
  #"us medical",
  full_join( read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.15.USMedical',  col_type = 'c'))

#sector category codes
services_code_mapping <- 
  tribble(~id, ~items,
  "CUSR0000SASLE",	"services less energy services",
  "CUSR0000SAH1","shelter",
  "CUSR0000SEHC", "owners' equivalent rent of residences",
  "CUSR0000SEHC01","rent of primary residence",
  "CUSR0000SEHB", "lodging away from home",
  "CUSR0000SEHG","water and sewer and trash collection services",
  "CUUR0000SEHP","household operations",
  "CUSR0000SAS4","transportation services",
  "CUSR0000SETG01","airline fares",
  "CUSR0000SAM2", "medical care services",
  "CUSR0000SARS",  "recreation services",
  "CUSR0000SAES","education and communication services",
  "CUUR0000SAGS", "other personal services")

category_list <- 
  unique(services_code_mapping$id)

cleaned_core_services_cpi_data<- 
  #clean data
  raw_time_series%>% 
  #reformat and clean data
  rename(id = `series_id        `, 
         year = `year`,
         month = `period`, 
         value = `       value`) %>% 
  select(id, year, month, value) %>% 
  mutate(id = str_trim(id),
         value = str_trim(value), 
         value = as.double(value), 
         month = str_sub(month, 2, -1), 
         date = str_c(year, "-", month), 
         date = ym(date)) %>% 
  select(date, id, value) %>%
  filter(id %in% category_list) %>% 
  #merging mapping
  left_join(services_code_mapping) %>% 
  # select(date, value, series_title) %>%
  #calculating change per month
  group_by(id) %>% 
  mutate(monthly_chg = round(((value - lag(value))/lag(value))*100, 1)) %>% 
  ungroup()

#merging the two datasets together
core_services_cpi_and_weights_full_dataset <- 
  cleaned_core_services_cpi_data %>% 
  #creating year variables so that it can be merged with the weights dataset
  mutate(year = year(date)) %>% 
  #merging relative weights data from clean_historical_cpi_relative_importance.R
  full_join(cleaned_cpi_relative_importance_historical_data) %>% 
  #don't need these two variables anymore for merging with the weights dataset
  select(-indent_level)


#creating time-series of core services inflation less shelter data--------------
core_services_less_shelter_categories <- 
  c("education and communication services", "water and sewer and trash collection services",
    "recreation services", "transportation services")

core_services_less_shelter_cpi_data <- 
 core_services_cpi_and_weights_full_dataset %>% 
  filter(date > as.Date("2010-01-01")) %>% 
  filter(items %in% core_services_less_shelter_categories) %>% 
  # calculate weights by year
  group_by(date) %>% 
  mutate(denominator = sum(weight_c),
         core_services_weights = weight_c/denominator,
         adjusted_contribution = monthly_chg*core_services_weights) %>% 
  summarize(core_services_less_shelter_inflation = sum(adjusted_contribution)) %>% 
   ungroup()

#plotting core services less shelter inflation through time ------------------
  core_services_less_shelter_cpi_data %>% 
  ggplot() +
  geom_line(aes(x = date, y = core_services_less_shelter_inflation)) +
  # geom_bar(aes(x = date, y = core_services_less_shelter_inflation), stat = "identity") +
  geom_hline(yintercept = 0)+
  labs(x = " ", y = " ", 
       title = "U.S. Core Services Less Shelter Inflation (2010-Present)", 
       subtitle = "% m/m",
       caption = "Series includes: Education and Communication Services, Water and Sewer and Trash Collection Services, Recreation Services, \nTransportation Services or historical equivalents") + 
  scale_x_date(lim = c(as.Date("2010-01-01"), as.Date(today())),
               date_label = "%Y",
               date_breaks = "1 year") +
  #my own personal theme setup
  estelle_theme() +
  theme(plot.caption= element_text(hjust = 0,
                                   margin = margin(-10,0,0,0)))

ggsave("us_core_services_less_shelter_inflation.png")

#following the sanity check, realized the weights change every month, not just
#year so need to go back and adjust that, but we're close
#should find out how often these weights actuall change

  




