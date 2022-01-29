  # voronoi chart attempt
  library(tidyverse)
  library(lubridate)
  library(ggvoronoi)
  library(viridis)
  library(deldir)

  
  #file used for weights and checks
  download.file(url ="https://www.bls.gov/web/cpi/cpipress2.xlsx", "D:/Rscripts/us_cpi/cpi.xlsx", mode = "wb")
  
  weights_raw_data <-
    readxl::read_xlsx("cpi.xlsx", skip = 3, col_names = TRUE) %>% 
    select(`Indent Level`, contains("Expenditure") ,contains("Relative"), `...8`) %>% 
    rename(categorical_level = `Indent Level`,
           series_name = `Expenditure category`,
           weights = names(.[,3]),
           monthly_chg = `...8`) %>% 
    filter(!is.na(categorical_level)) %>% 
    filter(categorical_level != 0) %>% 
    filter(!is.na(weights)) %>% 
    #try simple category level 1 first
    filter(categorical_level == 5)
  
  obs <- count(weights_raw_data) %>% 
    as.double()
  
  s <- seq(0, 2 * pi, length.out = obs)
  
  sample <- 
  tibble(
    x = runif(obs),
         y = runif(obs),
         weights_raw_data) %>% 
    mutate(circle_x = weights*x,
           circle_y = weights*y,
           monthly_chg = as.double(monthly_chg))
  #first layer of voronoi diagram-- labels are now gone?
  sample %>% 
  ggplot(aes(x=x,y=y)) +
    geom_point() +
    geom_voronoi(aes(x=circle_x,y=circle_y, fill = monthly_chg), color = "black") +
    scale_fill_viridis(option="E")
    geom_text(aes(x=x,y=y,label = series_name),size = 3)
    # theme_void()

   
  
  

  
  
  weights_raw_data %>% 
  ggplot() +
    geom_voronoi(aes(x = x, y= y, fill = categorical_level), outline = circle, size = 0.1)
  
  x <- weights_raw_data$monthly_chg
  
  set.seed(1)
  x <- sample( weights_raw_data$monthly_chg, size = 10)
  y <- sample( weights_raw_data$monthly_chg, size = 10)
  dist <- sqrt((x - 20) ^ 2 + (y - 20) ^ 2)
  
  df <- data.frame(x, y, dist = dist)
  
  s <- seq(0, 2 * pi, length.out = 30)
  circle <- data.frame(x = 1.2 * (1 + cos(s)),
                       y = 1.2 * (1 + sin(s)),
                       group = rep(1, 30))
  
  ggplot(df, aes(x, y, fill = dist)) +
    geom_voronoi(outline = circle,
                 color = 1, size = 0.1) +
    scale_fill_gradient(low = "#B9DDF1",
                        high = "#2A5783",
                        guide = "none") +
    theme_void() +
    coord_fixed()
  
  
  
  ggplot(iris, aes(Sepal.Length, Sepal.Width, group = -1L)) +
    geom_voronoi(aes(fill = Species)) +
    geom_voronoi() 
  
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
