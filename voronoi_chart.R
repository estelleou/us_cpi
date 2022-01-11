# voronoi chart attempt


#file used for weights and checks
download.file(url ="https://www.bls.gov/web/cpi/cpipress2.xlsx", "D:/Estelle/Rscripts/us_cpi/cpi.xlsx", mode = "wb")

weights_raw_data <-
  readxl::read_xlsx("cpi.xlsx", skip = 3, col_names = TRUE)

#grabbing weights
# grabbing_last_headline_cpi_value_to_scale_by <- 
pdf("test.pdf", h = 11, w = 10)
test <- 
  weights_raw_data %>% 
  select(`Expenditure category`, contains("Relative\nimportance"), `Indent Level`) %>% 
  rename(series_name = `Expenditure category`,
         weights = names(.[,2]),
         categorical_level = `Indent Level`) %>% 
  # filter(categorical_level > 0) %>%
  right_join(cleaned_data_since_2021) %>% 
  filter(!is.na(weights)) %>%
  select(date, series_name, series_title, weights, categorical_level, monthly_chg) %>% 
  filter(date == last(date)) %>% 
  select(-date) %>% 
  mutate(series_title = fct_rev(factor(series_title, levels = factor_list))) %>% 
  select(-series_title)
grid.table(test)
dev.off()
