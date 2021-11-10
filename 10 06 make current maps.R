
# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Function ----------------------------------------------------------------
make_map <- function(rst, clr, nms, tpe){

  # rst <- rf20
  # clr <- cl20
  # nms <- tp20
  # tpe <- 'wc 20'
  
  tbl <- rasterToPoints(x = rst, spatial = FALSE)
  tbl <- as_tibble(tbl)
  colnames(tbl) <- c('lon', 'lat', 'value')
  tbl <- mutate(tbl, value = factor(value, levels = 1:9))
  
  lbl <- data.frame(value = 1:9, 
                    name = c('Unsuitable', 'Unsuitable', nms, 'Limitations', 'Mixed'),
                    color = c('#FFFFFF', '#FFFFFF', clr, '#7E7E7E', '#FFFFA8'))
  
  lbl <- mutate(lbl, value = factor(value, levels = value), name = factor(name, levels = c('Unsuitable', nms, 'Limitations', 'Mixed')))
  
  tbl <- inner_join(tbl, lbl, by = 'value')
  
  ggm <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = name)) + 
    geom_sf(data = limt, fill = NA) + 
    scale_fill_manual(values = unique(lbl$color)) +
    coord_sf() + 
    theme_ipsum_es() + 
    theme(legend.position = 'bottom') + 
    labs(x = 'Longitude', y = 'Latitude', fill = 'AEZ')
  
  ggsave(plot = ggm, 
         filename = glue('../png/maps/rf/{run}/rf_current_{tpe}.png'), 
         units = 'in', width = 12, height = 8, dpi = 300)
  
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------
rf14 <- raster('../rf/output/run_2/wc 14/results/process/RF_clust_unc.tif')
rf20 <- raster('../rf/output/run_2/wc 20/results/process/RF_clust_unc.tif')
limt <- sf::st_read('../data/shp/base/countries_target_4.shp')
wrld <- sf::st_read('../data/shp/base/all_countries.shp')
wrld <- filter(wrld, CONTINENT == 'Africa')

# Make map ----------------------------------------------------------------
cl14 <- c('#CFCF49', '#5662C5', '#D68910', '#154360', '#E4641F')
cl20 <- c('#4CDA52', '#0E7F86', '#EC7063', '#566573', '#154360')

tp14 <- c('Hot - Moderate', 'Hot - Wet', 'Moderate - Very Wet', 'Cold - Wet', 'Cold - Dry')
tp20 <- c('Hot - Very Wet', 'Very Hot - Very Wet', 'Moderate - Very Dry', 'Very cold - Dry', 'Cold - Wet')

# Apply functions ---------------------------------------------------------
make_map(rst = rf14, clr = cl14, nms = tp14, tpe = 'wc 14')
make_map(rst = rf20, clr = cl20, nms = tp20, tpe = 'wc 20')

