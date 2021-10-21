

require(pacman)
pacman::p_load(raster, rgdal, ggrepel, rgeos, stringr, sf, tidyverse, gtools, 
               ggspatial, terra, ggspatial, gridExtra, ggpubr, ggrepel, ggthemes, hrbrthemes,
               RColorBrewer, Boruta, mlbench, randomForest, caret, fasterize, fs, gridExtra, 
               colorspace, glue, dismo, usdm, classInt, ENMeval)

g <- gc(reset = TRUE)
options(scipen = 999)
rm(list = ls())

