# voronoi chart attempt
library(tidyverse)
library(lubridate)
library(ggvoronoi)
library(gpclib)
library(deldir)

#file used for weights and checks
download.file(url ="https://www.bls.gov/web/cpi/cpipress2.xlsx", "D:/Rscripts/us_cpi/cpi.xlsx", mode = "wb")

weights_raw_data <-
  readxl::read_xlsx("cpi.xlsx", skip = 3, col_names = TRUE) %>% 
  select(`Indent Level`, contains("Expenditure") ,contains("Relative\n"), `...8`) %>% 
  rename(categorical_level = `Indent Level`,
         series_name = `Expenditure category`,
         weights = names(.[,3]),
         monthly_chg = `...8`) %>% 
  filter(categorical_level == 2)


allocate
function (names, s, w, outer, target, maxIteration = 200, debug = FALSE, 
          dplot = FALSE, debugCell = FALSE) 
  {
  count <- 1
  debugPlot <<- debugPlotGen()
  repeat {
    k <- awv(s, w, outer, debug, debugCell)
    areas <- lapply(k, area.poly)
    if (debug) {
      drawRegions(list(names = names, k = k, s = s, w = w, 
                       a = areas, t = target), debug)
      info <- rbind(area = round(unlist(areas)/sum(unlist(areas)), 
                                 4), target = round(target, 4), weight = round(w, 
                                                                               1))
      colnames(info) <- names
      print(info)
    }
    if (count > maxIteration || breaking(unlist(areas), target, 
                                         debug = debug, dplot = dplot)) {
      return(list(names = names, k = k, s = s, w = w, a = areas, 
                  t = target))
    }
    else {
      w <- adjustWeights(w, unlist(areas), target)
      s <- shiftSites(s, k)
      w <- shiftWeights(s, w)
    }
    count <- count + 1
  }
}

x <- runif(6,1,100)
y <- runif(6,1,100)
ggplot()+
ggvoronoi(aes(x=x, y=y, fill = weights_raw_data$weights))
w
unitSquare <- as(list(x=c(0,0,1000,1000,0),
                      y=c(0,1000,1000,0,0)),
                 "gpc.poly")
treemap <- allocate(weights_raw_data$series_name, list(x=x,y=y),
         w, unitSquare, weights_raw_data$weights)
drawRegions(treemap, label= TRUE)
