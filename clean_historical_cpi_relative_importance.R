#script cleans the relative importance of CPI items throughout history and combines
#it into one dataset 
library(tidyverse)
library(lubridate)

#latest data
#manually download the relative importance data from the BLS's website
#download Table 2 from https://www.bls.gov/web/cpi.supp.toc.htm 
raw_data <- 
  readxl::read_xlsx("latest_cpi_data.xlsx", skip = 2)

cleaned_cpi_relative_importance_and_indent_data  <-
  raw_data %>% 
  select(`...1`, `...2`, `...3`) %>% 
  rename(indent_level = `...1`,
         items= `...2`,
         dec_weight = `...3`) %>% 
  mutate(dec_weight = as.double(dec_weight)) %>% 
  #generating year so that dataset can be merged with other years
  mutate(date = as.Date("2022", "%Y"),
         year = year(date)) %>% 
  mutate(items = gsub("\\(\\d+\\)", "", items),
         items = gsub("\\d+", "", items),
         items = str_to_lower(items)) %>% 
  select(-date) %>% 
  slice(-(2:3)) %>% 
  slice(-(1:1))
  
#pull in all others through text files downloaded from the BLS's website
#2000-2009
cleaned_cpi_relative_importance_2000_to_2009 <- list()
for (i in seq(2000,2009, by = 1) ){

  file_name <-  paste0("cpi_relative_importance_2000-2009/", i,".txt")
  
  raw_data <-
  read_csv(file_name) %>% 
  slice(-(1:6)) 
  
cleaned_file <-
  raw_data %>% 
  separate(names(raw_data), into = c("items", "dec_weight", "weight_w"), sep = "    ") %>%
  select(-weight_w) %>% 
  mutate(dec_weight = as.double(dec_weight)) %>% 
  mutate(items = gsub("\\.+$", "", items)) %>% 
  mutate(items = gsub("\\(\\d+\\)", "", items),
         items = gsub("\\d+", "", items)) %>% 
  mutate(items = str_trim(items)) %>% 
  #remove the first row that shows "All items with a weight of 100" 
  #because don't need it and formatting is weird
  filter(!is.na(dec_weight)) %>% 
  #slapping on the date for the dataset for merging later on
  mutate(date = as.Date(as.character(i), "%Y"),
         year = year(date)) %>% 
  select(-date)

  cleaned_cpi_relative_importance_2000_to_2009 <- bind_rows(cleaned_cpi_relative_importance_2000_to_2009, cleaned_file)

}


#2010-2019
cleaned_cpi_relative_importance_2010_to_2019 <- list()
for (i in seq(2010,2019, by = 1) ){
  
  file_name <-  paste0("cpi_relative_importance_2010-2019/", i,".txt")
  
  raw_data <-
    read_csv(file_name) %>% 
    slice(-(1:6)) 
  
  cleaned_file <-
    raw_data %>% 
    separate(names(raw_data), into = c("items", "dec_weight", "weight_w"), sep = "    ") %>%
    select(-weight_w) %>% 
    mutate(dec_weight = as.double(dec_weight)) %>% 
    mutate(items = gsub("\\.+$", "", items)) %>% 
    mutate(items = gsub("\\(\\d+\\)", "", items),
           items = gsub("\\d+", "", items)) %>% 
    mutate(items = str_trim(items)) %>% 
    #remove the first row that shows "All items with a weight of 100" 
    #because don't need it and formatting is weird
    filter(!is.na(dec_weight)) %>% 
    #slapping on the date for the dataset for merging later on
    mutate(date = as.Date(as.character(i), "%Y"),
           year = year(date)) %>% 
    select(-date)
  
  cleaned_cpi_relative_importance_2010_to_2019 <- bind_rows(cleaned_cpi_relative_importance_2010_to_2019, cleaned_file)
  
}

#cleaning the latest two years (2020-2021)
cleaned_cpi_relative_importance_2020 <- 
  readxl::read_xlsx("cpi_relative_importance_2020.xlsx", skip = 8) %>% 
  rename(items = `All items`,
       dec_weight = `100...3`) %>% 
  select(items, dec_weight) %>% 
  filter(!is.na(items)) %>% 
  #slapping on the date for the dataset for merging later on
  mutate(date = as.Date("2020", "%Y"),
       year = year(date)) %>% 
  select(-date)

cleaned_cpi_relative_importance_2021 <- 
  readxl::read_xlsx("cpi_relative_importance_2021.xlsx", skip = 8) %>% 
  rename(items = `All items`,
         dec_weight = `100...3`) %>% 
  select(items, dec_weight) %>% 
  filter(!is.na(items)) %>% 
  #slapping on the date for the dataset for merging later on
  mutate(date = as.Date("2021", "%Y"),
         year = year(date)) %>% 
  select(-date)
  
#combine all the cleaned datasets
cleaned_cpi_relative_importance_historical_data <-
  cleaned_cpi_relative_importance_and_indent_data %>% 
  full_join(cleaned_cpi_relative_importance_2021) %>% 
  full_join(cleaned_cpi_relative_importance_2020) %>% 
  full_join(cleaned_cpi_relative_importance_2010_to_2019) %>% 
  full_join(cleaned_cpi_relative_importance_2000_to_2009) %>% 
  arrange(desc(items)) %>% 
  fill(indent_level, .direction = c("down")) %>% 
  # last bit of cleaning to make merging easier 
  mutate(items = str_to_lower(items)) %>% 
  mutate(items = ifelse(items == "recreation", "recreation services", items),
         items = ifelse(items == "education and communication", "education and communication services", items))

  

  