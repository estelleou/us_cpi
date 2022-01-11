# voronoi chart attempt


#file used for weights and checks
download.file(url ="https://www.bls.gov/web/cpi/cpipress2.xlsx", "D:/Estelle/Rscripts/us_cpi/cpi.xlsx", mode = "wb")

weights_raw_data <-
  readxl::read_xlsx("cpi.xlsx", skip = 3, col_names = TRUE)


