library(tidyverse)
library(lubridate)
library(viridis)
library(gridExtra)
library(grid)

#pull in public data from BLS website
raw_data <- 
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
code_mapping <- 
  tribble(~id, ~series_title, ~series_name,
          "CUSR0000SA0",	"(Headline CPI) All items___________________________________","All items",
          "CUSR0000SAF",	"Food___________________________________________","Food",
          "CUSR0000SAF11",	"Food at home_______________________________","Food at home",
          "CUUR0000SEFV",	"Food away from home (1)____________________","Food away from home (1)",
          "CUSR0000SA0E",	"Energy_________________________________________","Energy",
          "CUSR0000SACE",	"Energy commodities________________________","Energy commodities",
          "CUSR0000SETB01",	"Gasoline (all types)____________________","Gasoline (all types)",
          "CUUR0000SEHE01",	"Fuel oil (1)___________________________ ","Fuel oil (1)",
          "CUSR0000SEHF", "Energy services____________________________","Energy services",
          "CUSR0000SEHF01",	"Electricity____________________________", "Electricity",
          "CUSR0000SEHF02",	"Utility (piped) gas service______________ ","Utility (piped) gas service",
          "CUSR0000SA0L1E",	"(Core CPI) All items less food and energy___________________","All items less food and energy",
          # "CUSR0000SACL1E",	" Commodities less food and energy___________"," Commodities less food and energy",
          "CUSR0000SAH31", "Household Furnishings and Supplies___________ ","Household Furnishings and Supplies",
          "CUSR0000SETA", "New and used motor vehicles__________________", "New and used motor vehicles",
          "CUSR0000SETA01",	"New vehicles__________________________","New vehicles",
          "CUSR0000SETA02",	"Used cars and trucks___________________ ","Used cars and trucks",
          "CUSR0000SAA",	"Apparel_____________________________________ ","Apparel",
          "CUUR0000SAM1",	"Medical care commodities (1)__________________","Medical care commodities (1) ",
          "CUSR0000SARC",  "Recreation Commodities______________________ ","Recreation Commodities",
          "CUSR0000SAES",  "Education and Communication_________________"  ,"Education and Communication",
          "CUSR0000SAF116","Alcoholic Beverages__________________________"  ,"Alcoholic Beverages",
          "CUSR0000SAGC",  "Other Goods_________________________________ "  ,"Other Goods",
          # "CUSR0000SASLE",	"Services less energy services____________________","Services less energy services",
          "CUSR0000SAH1",	"Shelter______________________________________ ","Shelter",
          "CUSR0000SEHC",  "Owners' Equivalent Rent of Residences__"  ,"Owners' Equivalent Rent of Residences",
          "CUSR0000SEHC01","Rent of Primary Residence_____________"  ,"Rent of Primary Residence",
          "CUSR0000SEHB",  "Lodging Away From Home_____________"  ,"Lodging Away From Home",
          "CUSR0000SEHG",  "Water and sewer and trash collection services___"  ,"Water and sewer and trash collection services",
          "CUUR0000SEHP",  "Household operations (1)_______________________"  ,"Household operations (1)",
          "CUSR0000SAS4",  "Transportation services________________________","Transportation services",
          "CUSR0000SETG01","Airline fares_________________________ "  ,"Airline fares",
          "CUSR0000SAM2",	"Medical care services_________________________ ", "Medical care services",
          "CUSR0000SARS",  "Recreation Services___________________________"  ,"Recreation Services",
          "CUSR0000SAES",  "Education and Communication services_________"  ,"Education and Communication services",
          "CUUR0000SAGS",  "Other Personal services (1)_____________________"  ,"Other Personal services (1)")


headline_code_list <- 
  c("CUSR0000SA0",	
    "CUSR0000SAF",	
    "CUSR0000SAF11",
    "CUUR0000SEFV",	
    "CUSR0000SA0E",	
    "CUSR0000SACE",	
    "CUSR0000SETB01",
    "CUUR0000SEHE01",
    "CUSR0000SEHF", 
    "CUSR0000SEHF01",
    "CUSR0000SEHF02")

core_code_list <- 
          c("CUSR0000SA0L1E",
          "CUSR0000SACL1E",
          "CUSR0000SAH31", 
          "CUSR0000SETA01",
          "CUSR0000SETA02",
          "CUSR0000SAA",	
          "CUUR0000SAM1",	
          "CUSR0000SARC",  
          "CUSR0000SAES",  
          "CUSR0000SAF116",
          "CUSR0000SAGC",  
          "CUSR0000SASLE",
          "CUSR0000SAH1",	
          "CUSR0000SEHC",  
          "CUSR0000SEHC01",
          "CUSR0000SEHB",  
          "CUSR0000SEHG",  
          "CUUR0000SEHP",  
          "CUSR0000SAS4", 
          "CUSR0000SETG01",
          "CUSR0000SAM2",
          "CUSR0000SARS",  
          "CUSR0000SAES",  
          "CUUR0000SAGS") 

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

category_names <- 
  unique(code_mapping$series_name)

#heatmap scaled by ALL categories
cleaned_data_since_2021 %>% 
  mutate(series_title = fct_rev(factor(series_title, levels = factor_list)))  %>% 
  ggplot(aes(x = date, series_title)) +
  geom_tile(aes(fill = monthly_chg)) +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(monthly_chg>=8 | monthly_chg <= -4),
            aes(label =  monthly_chg), color = "white") +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(monthly_chg<8 & monthly_chg > -4 ),
            aes(label =  monthly_chg), color = "black") +
  scale_x_date(date_breaks = "1 month", date_label = "%b", 
               expand = c(0, 0)) +
  scale_fill_viridis(option="H")+
  labs(x = "2021", y = "", title = "Percent changes in CPI for All Urban Consumers",
       fill = "Percent",
       caption = "1 Not seasonally adjusted.")+
  theme_bw()+
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),

                panel.border  =element_blank())
ggsave("D:/Estelle/Rscripts/us_cpi/latest_cpi_heatmap.png", w = 11, h = 7)

#heatmap with components scaled within categories
rescaled_data <- 
  cleaned_data_since_2021 %>% 
  group_by(series_title) %>%
  mutate(rescaled = scales::rescale(monthly_chg, to = c(-10, 10))) %>%
  ungroup() 

rescaled_data %>% 
  mutate(series_title = fct_rev(factor(series_title, levels = factor_list))) %>% 
  ggplot(aes(x = date, y = series_title)) +
  geom_tile(aes( fill = rescaled)) +
  geom_text(data = cleaned_data_since_2021,
            aes(label =  monthly_chg),
            color = ifelse(rescaled_data$rescaled >5 |rescaled_data$rescaled <=-5, "white", "black")) +
  scale_x_date(date_breaks = "1 month", date_label = "%b-%y", 
               expand = c(0, 0), position = "top") +
  scale_fill_gradient2(low="darkslateblue", high="dark red")+
  # scale_fill_viridis( option="C")+
  labs(x = "", y = "", title = "Percent changes in CPI for All Urban Consumers (m/m)",
       fill = "Price acceleration relative to component history 
                (Slower <----> Faster)",
       caption = "1 Not seasonally adjusted.")+
  theme_bw()+
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_blank(),
        legend.position = "top",
        legend.margin= margin(0,0,-15,-1),
        panel.border  =element_blank()) 
ggsave("D:/Estelle/Rscripts/us_cpi/latest_cpi_within_category_heatmap.png", w = 11, h = 7)


#creating heatmap of just headline components for the last 4 months

cleaned_data_since_2021 %>% 
  filter(id %in% headline_code_list) %>% 
  filter(date > last(date)-120) %>%
  mutate(series_title = fct_rev(factor(series_title, levels = factor_list)))  %>% 
  ggplot(aes(x = date, series_title)) +
  geom_tile(aes(fill = monthly_chg)) +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(id %in% headline_code_list) %>% 
              filter(date > last(date)-120) %>%
              filter(monthly_chg>=8 | monthly_chg <= 1),
            aes(label =  monthly_chg), color = "white") +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(id %in% headline_code_list) %>% 
              filter(date > last(date)-120) %>%
              filter(monthly_chg<8 & monthly_chg > 1 ),
            aes(label =  monthly_chg), color = "black") +
  scale_x_date(date_breaks = "1 month", date_label = "%b", 
               expand = c(0, 0)) +
  scale_fill_viridis(option="H")+
  labs(x = "2021", y = "", title = "Headline CPI in the Last 4 Month (% change)",
       fill = "Percent",
       caption = "1 Not seasonally adjusted.")+
  theme_bw()+
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        
        panel.border  =element_blank())

ggsave("D:/Estelle/Rscripts/us_cpi/latest_headline_cpi_heatmap.png", w = 11, h = 7)

#creating heatmap of just core
cleaned_data_since_2021 %>% 
  filter(id %in% core_code_list) %>% 
  filter(date > last(date)-120) %>% 
  mutate(series_title = fct_rev(factor(series_title, levels = factor_list)))  %>% 
  ggplot(aes(x = date, series_title)) +
  geom_tile(aes(fill = monthly_chg)) +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(id %in% core_code_list) %>% 
              filter(date > last(date)-120) %>% 
              filter(monthly_chg>=2 | monthly_chg <= -5),
            aes(label =  monthly_chg), color = "white") +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(id %in% core_code_list) %>% 
              filter(date > last(date)-120) %>% 
              filter(monthly_chg<2 & monthly_chg > -5 ),
            aes(label =  monthly_chg), color = "black") +
  scale_x_date(date_breaks = "1 month", date_label = "%b", 
               expand = c(0, 0)) +
  scale_fill_viridis(option="H")+
  labs(x = "2021", y = "", title = "Core CPI in Last 4 months (% change)",
       fill = "Percent",
       caption = "1 Not seasonally adjusted.")+
  theme_bw()+
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        
        panel.border  =element_blank())

ggsave("D:/Estelle/Rscripts/us_cpi/latest_core_cpi_heatmap.png", w = 11, h = 7)

#heatmap scaled WITHIN categories

categorical_historic_intensity <- function(category) {
  ggplot()+
  geom_bar(data = cleaned_data_since_2018 %>% 
             mutate(series_title = fct_rev(factor(series_title, levels = factor_list)))  %>% 
             filter(series_title == factor_list[category]) %>% 
             filter(date >= "2019-10-30"), 
           aes(x = date, y = monthly_chg, fill = monthly_chg), stat = "identity") +
  geom_text(data= cleaned_data_since_2018 %>% 
              mutate(series_title = fct_rev(factor(series_title, levels = factor_list)))  %>% 
              filter(series_title == factor_list[category]) %>% 
              filter(date >= "2019-10-30") %>% 
              filter(monthly_chg >=0),
              aes(y = monthly_chg + 0.09, x = , date, label =  monthly_chg), color = "black", size = 3) +
    geom_text(data= cleaned_data_since_2018 %>% 
                mutate(series_title = fct_rev(factor(series_title, levels = factor_list)))  %>% 
                filter(series_title == factor_list[category]) %>% 
                filter(date >= "2019-10-30") %>% 
                filter(monthly_chg <0),
              aes(y = monthly_chg - 0.09, x = , date, label =  monthly_chg), color = "black",  size = 3)+
  geom_hline(yintercept = 0)+
  scale_fill_viridis(option="H") +
  labs(title = paste0(category_names[category]), x = "", y = "", fill = "Historical Intensity (%)") +
  scale_x_date(date_breaks = "1 month", date_label = "%b-%y", 
               expand = c(0, 0)) +
  theme_bw()+
  theme(axis.text = element_text(face = "bold", angle = 90),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"),
        panel.border  =element_blank())
}
  

pdf("categorical_heatmap.pdf", h = 8, w = 10.5)

for (i in seq(1, length(category_list), by = 3)) {
  
  grid.arrange(categorical_historic_intensity(i),
               categorical_historic_intensity(i+1),
               categorical_historic_intensity(i+2), 
                layout_matrix = rbind(c(1, 1),
                                      c(2, 2),
                                      c(3, 3)))
}

dev.off()

