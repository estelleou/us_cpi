library(tidyverse)
library(lubridate)
library(viridis)

#pull in public data from BLS website
raw_data <- 
  #Total nonfarm"
  read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems', col_type = 'c') %>% 
  #"Total private",
  full_join(read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.11.USFoodBeverage', col_type = 'c')) %>%
  #"Mining and logging",
  full_join( read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.20.USCommoditiesServicesSpecial', col_type = 'c')) %>%
  #"Construction",
  full_join( read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation', col_type = 'c')) %>%
  #"Manufacturing",
  full_join( read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.12.USHousing', col_type = 'c') )%>%
  #Durable Goods",
  full_join(  read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.0.Current', col_type = 'c')) %>%
  #"Nondurable Goods",
  full_join( read_delim('https://download.bls.gov/pub/time.series/cu/cu.data.15.USMedical', col_type = 'c'))

#sector category codes
code_mapping <- 
  tribble(~id, ~series_title,
          "CUSR0000SA0",	"All items___________________________________",
          "CUSR0000SAF",	"Food_________________________________",
          "CUSR0000SAF11",	"Food at home_____________________",
          "CUUR0000SEFV",	"Food away from home (1)__________",
          "CUSR0000SA0E",	"Energy_______________________________",
          "CUSR0000SACE",	"Energy commodities______________",
          "CUSR0000SETB01",	"Gasoline (all types)_________",
          "CUUR0000SEHE01",	"Fuel oil (1)_________________",
          "CUSR0000SEHF", "Energy services__________________",
          "CUSR0000SEHF01",	"Electricity__________________", 
          "CUSR0000SEHF02",	"Utility (piped) gas service____",
          "CUSR0000SA0L1E",	"All items less food and energy__________",
          "CUSR0000SACL1E",	" Commodities less food and energy_",
          "CUSR0000SETA01",	"New vehicles_______________",
          "CUSR0000SETA02",	"Used cars and trucks________",
          "CUSR0000SAA",	"Apparel___________________ ",
          "CUUR0000SAM1",	"Medical care commodities (1)",
          "CUSR0000SASLE",	"Services less energy services____",
          "CUSR0000SAH1",	"Shelter____________________",
          "CUSR0000SAS4", "Transportation services______",
          "CUSR0000SAM2",	"Medical care services_______")

category_list <- 
  unique(code_mapping$id)

cleaned_data_since_2018 <- 
  #clean data
  raw_data %>% 
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
  left_join(code_mapping) %>% 
  # select(date, value, series_title) %>%
  #calculating change per month
  group_by(id) %>% 
  mutate(monthly_chg = round(((value - lag(value))/lag(value))*100, 1)) %>% 
  ungroup() 

#generate dataset since 2021
cleaned_data_since_2021 <-
  cleaned_data_since_2018 %>% 
  filter(date >= "2021-01-01")

factor_list <- 
  unique(code_mapping$series_title)

cleaned_data_since_2021 %>% 
  mutate(series_title = fct_rev(factor(series_title, levels = factor_list)))  %>% 
  ggplot(aes(x = date, series_title)) +
  geom_tile(aes(fill = monthly_chg)) +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(monthly_chg>=8 | monthly_chg <= -2),
            aes(label =  monthly_chg), color = "white") +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(monthly_chg<8 & monthly_chg > -2 ),
            aes(label =  monthly_chg), color = "black") +
  scale_x_date(date_breaks = "1 month", date_label = "%b", 
               expand = c(0, 0)) +
  scale_fill_viridis(option="H")+
  labs(x = "2021", y = "", title = "Percent changes in CPI for All Urban Consumers",
       fill = "Thousands",
       caption = "1 Not seasonally adjusted.")+
  theme_bw()+
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border  =element_blank())

ggsave("D:/Estelle/Rscripts/us_cpi/latest_cpi_heatmap.png", w = 11, h = 7)
