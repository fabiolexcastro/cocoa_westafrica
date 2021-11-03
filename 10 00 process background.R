

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, dismo, tidyverse, gtools, 
               fasterize, fs, colorspace, gridExtra, ggpubr, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
make_bias <- function(shp){
  
  # shp <- prds[[1]]
  
  cat('Start\n')
  pnt <- st_intersection(x = pnts, y = shp)
  pnt <- pnt %>% dplyr::select(starts_with('NAME'))
  frq <- pnt %>% as_tibble() %>% dplyr::select(-geometry) %>% group_by(across(everything(), as.character)) %>% summarise(count = n()) %>% ungroup()
  pnt <- st_as_sf(x = pnt, coords = c('X', 'Y'), crs = st_crs(4326))
  shp <- inner_join(shp, frq)
  shp <- mutate(shp, prp_vle = value / sum(value, na.rm = TRUE), prp_frq = count / sum(count, na.rm = TRUE))
  shp <- mutate(shp, prp_vle = round(prp_vle, digits = 5), prp_frq = round(prp_frq, digits = 5))
  shp <- mutate(shp, prp_glb = prp_frq / prp_vle, prp_nrm = prp_glb / sum(prp_glb, na.rm = TRUE))
  shp <- mutate(shp, prp_nrm = round(prp_nrm, digits = 4))
  cat('Done!\n')
  return(list(shp, pnt))
  
}
make_maps <- function(shp, pnt, cnt){
  
  # shp <- rslt[[5]][[1]]
  # pnt <- rslt[[5]][[2]]
  # cnt <- 'NGA'
  
  cat('Start', cnt, '\n')
  lim <- raster::getData(name = 'GADM', country = cnt, level = 0)
  msk <- raster::crop(mask, lim) %>% raster::mask(., lim)
  
  cat('Map value\n')
  ggv <- ggplot() + 
    geom_sf(data = shp, aes(fill = prp_vle), col = 'white') +
    scale_fill_continuous_sequential(palette = 'Heat', na.value = 'grey') + 
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(2, 'line')) + 
    labs(fill = 'Proportion\nHouseholds')
  
  cat('Map frequency\n')
  ggf <- ggplot() + 
    geom_sf(data = shp, aes(fill = prp_frq), col = 'white') + 
    scale_fill_continuous_sequential(palette = 'Heat', na.value = 'grey') + 
    theme_void() +
    theme(legend.position = 'bottom', 
          legend.key.width = unit(2, 'line')) + 
    labs(fill = 'Percentage\nPresences')
  
  cat('Generate points\n')
  cll <- raster::extract(msk, st_coordinates(pnt)[,1:2], cellnumbers = TRUE)
  msk[cll[,1]] <- NA
  rst <- fasterize(sf = shp, raster = msk, field = 'prp_nrm')
  
  # shp[is.infinite(shp$prp_glb),'prp_glb'] <- NA
  # shp <- mutate(shp, prp_nrm = prp_glb / sum(prp_glb, na.rm = TRUE), prp_nrm = round(prp_nrm, digits = 4))
  
  bck <- randomPoints(mask = rst, n = nrow(pnt), prob = TRUE)
  bck <- as_tibble(bck)
  
  write.csv(bck, glue('../data/tbl/{run}/00 bck.csv'), row.names = FALSE)
  
  cat('Map background\n')
  ggb <- ggplot() + 
    geom_tile(data = as_tibble(rasterToPoints(rst)), aes(x = x, y = y, fill = layer)) +
    scale_fill_continuous_sequential(palette = 'Heat', na.value = 'grey') + 
    geom_sf(data = shp, fill = NA, col = 'white') +
    geom_point(data = bck, aes(x = x, y = y), col = 'red') + 
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(2, 'line')) + 
    labs(fill = 'Probability')
  
  cat('Map all\n')
  fg1 <- annotate_figure(ggv, top = text_grob('')) 
  fg2 <- annotate_figure(ggf, top = text_grob(''))
  fg3 <- annotate_figure(ggb, top = text_grob(''))
  
  lyt <- matrix(c(1, 1, 2, 2, 1, 1, 2, 2, NA, 3, 3, NA, NA, 3, 3, NA), nrow = 4, ncol = 4, byrow = TRUE)
  all <- grid.arrange(fg1, fg2, fg3, layout_matrix = lyt)
  
  cat('To save the png\n')
  ggsave(plot = all,
         filename = glue('../png/maps/bck/{run}/{cnt}.png'), units = 'in', 
         width = 12, height = 7, dpi = 300)
  
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------
pnts <- read_csv('../data/tbl/run_1/01 points 10 km.csv')
fles <- dir_ls('../data/shp/prod', regexp = '.shp$')
prds <- map(.x = fles, .f = st_read)
mask <- raster('../data/raster/climate/worldclim/2_5min/v1_4/bio_1.tif') * 0 + 1
run <- 'run_1'

# Table to shapefile ------------------------------------------------------
pnts <- st_as_sf(pnts, coords = c('x', 'y'), crs = st_crs(4326))

# Clean shapefile ---------------------------------------------------------
prds <- map(.x = 1:length(prds), .f = function(k){prds[[k]] %>% dplyr::select(-c(Freq, bias_num, bias_dnm, bias, bias_norm))})

# Make bias ---------------------------------------------------------------
rslt <- map(.x = prds, .f = make_bias)

# Make maps ---------------------------------------------------------------
cntr <- c('CIV', 'CMR', 'GHA', 'LBR', 'NGA')

map(.x = 1:length(rslt), .f = function(k){
  
  cat(cntr[k], '\n')
  make_maps(shp = rslt[[k]][[1]], pnt = rslt[[k]][[2]], cnt = cntr[k])
  
})


