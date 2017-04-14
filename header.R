# Sys.setenv("plotly_username"="hxfan1227")
# Sys.setenv("plotly_username"="hxfan1227")
# Sys.setenv("plotly_api_key"="nsiwqkxzg6")
# rm(list=ls())
# library(lubridate)
# library(reshape2)
# library(ggplot2)
# library(ggthemes)
# library(ggstance)
# library(SPEI)
# library(plyr)
# library(dplyr)
# library(stringr)
# library(MSBVAR)
# library(GGally)
# library(Deriv)
# library(zoo)
# library(reshape2)
# library(R.matlab)
# library(data.table)
# library(forecast)
# library(readr)
# library(scales)
# library(devtools)
# library(lattice)
# library(plotrix)
# library(cowplot)
# library(sp)
# library(rgdal)
# library(kriging)
# library(gstat)
# library(raster)
# library(rasterVis)
# library(maptools)
# library(ggsci)
# library(ggstance)
# library(gganimate)
# library(ggradar)
# library(tidyr)
# library(mice)



libraries <- function(pkgs, date, check = F){
  if(missing(date)) date <- as.Date(Sys.time())-1
  if (check) checkpoint::setSnapshot(date)
  install.fun <- function(pkg){
    if(!require(pkg, character.only = T)) install.packages(pkg)
  }
  # update.packages()
  plyr::a_ply(pkgs, 1, install.fun)
}


pkgs <- c("data.table", "tidyverse", "lubridate", "reshape2", "ggthemes", "ggstance", "SPEI", "plyr", "stringr", 
          "ggsci", "ggstance", "ggthemes", "gstat", "gWidgetsRGtk2", "kriging", "lattice", "lubridate", 
          "maptools", "mice", "MODISTools", "MSBVAR", "plotrix", "plyr", "R.matlab", "raster", 
          "rasterVis", "reshape2", "rgdal", "Rmisc", "scales", "sp", "SPEI", "stringr", "tidyverse", "zoo")
libraries(pkgs)

plyr::a_ply(pkgs, 1, library, character.only = T)



# source("trendMK.R")
# source("sen_slope.R")
# source("Mann_Kendall.R")
# source("createCluster.R")
# source("PM_FAO.R")
# source("voronoipolygons.R")


