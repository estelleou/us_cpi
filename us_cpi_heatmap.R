library(tidyverse)
library(lubridate)
library(viridis)
library(gridExtra)
library(grid)

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
  tribble(~id, ~series_title, ~series_name,
          "CUSR0000SA0",	"All items___________________________________","All items",
          "CUSR0000SAF",	"Food_________________________________","Food",
          "CUSR0000SAF11",	"Food at home_____________________","Food at home",
          "CUUR0000SEFV",	"Food away from home (1)__________","Food away from home (1)",
          "CUSR0000SA0E",	"Energy_______________________________","Energy",
          "CUSR0000SACE",	"Energy commodities______________","Energy commodities",
          "CUSR0000SETB01",	"Gasoline (all types)__________","Gasoline (all types)",
          "CUUR0000SEHE01",	"Fuel oil (1)_________________ ","Fuel oil (1)",
          "CUSR0000SEHF", "Energy services__________________","Energy services",
          "CUSR0000SEHF01",	"Electricity__________________", "Electricity",
          "CUSR0000SEHF02",	"Utility (piped) gas service____ ","Utility (piped) gas service",
          "CUSR0000SA0L1E",	"All items less food and energy__________","All items less food and energy",
          "CUSR0000SACL1E",	" Commodities less food and energy_"," Commodities less food and energy",
          "CUSR0000SETA01",	"New vehicles_______________","New vehicles",
          "CUSR0000SETA02",	"Used cars and trucks________","Used cars and trucks",
          "CUSR0000SAA",	"Apparel___________________ ","Apparel",
          "CUUR0000SAM1",	"Medical care commodities (1) ","Medical care commodities (1) ",
          "CUSR0000SASLE",	"Services less energy services____","Services less energy services",
          "CUSR0000SAH1",	"Shelter____________________","Shelter",
          "CUSR0000SAS4", "Transportation services______","Transportation services",
          "CUSR0000SAM2",	"Medical care services_______ ", "Medical care services")

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
              filter(monthly_chg>=8 | monthly_chg <= -2),
            aes(label =  monthly_chg), color = "white") +
  geom_text(data = cleaned_data_since_2021 %>% 
              filter(monthly_chg<8 & monthly_chg > -2 ),
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


#contributions chart 
#add more category 


ggsave("D:/Estelle/Rscripts/us_cpi/latest_cpi_heatmap.png", w = 11, h = 7)
