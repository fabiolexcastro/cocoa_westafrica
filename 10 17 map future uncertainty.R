
# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Function ----------------------------------------------------------------
make_map <- function(rst, clr, nms, tpe){
  
  rst <- rst4
  clr <- clrs
  nms <- tpes
  tpe <- 'BCC-CSM2-MR'
  
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
  
  plot(rst)
  ggm <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = name)) + 
    geom_sf(data = limt, fill = NA) + 
    scale_fill_manual(values = tbl %>% distinct(value, name, color) %>% arrange(value) %>% pull(color) %>% unique()) +
    coord_sf() + 
    theme_ipsum_es() + 
    theme(legend.position = 'bottom') + 
    labs(x = 'Longitude', y = 'Latitude', fill = 'AEZ')
  
  ggsave(plot = ggm, 
         filename = glue('../png/maps/rf/{run}/rf_future_{tpe}.png'), 
         units = 'in', width = 12, height = 8, dpi = 300)
  
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------
# rst1 <- raster('../rf/output/run_4/wc 14/results/process/mixed/2040_2069/rf_5_mixed_mohc_hadgem2_es.tif')
# rst2 <- raster('../rf/output/run_4/wc 14/results/process/mixed/2040_2069/rf_5_mixed_miroc_miroc5.tif')
rst3 <- raster('../rf/output/run_4/wc 20/results/process/mixed/2040_2069/rf_5_mixed_CanESM5.tif')
rst4 <- raster('../rf/output/run_4/wc 20/results/process/mixed/2040_2069/rf_5_mixed_BCC-CSM2-MR.tif')
limt <- sf::st_read('../data/shp/base/countries_target_4.shp')
wrld <- sf::st_read('../data/shp/base/all_countries.shp')
wrld <- filter(wrld, CONTINENT == 'Africa')
run <- 'run_4'

# Make map ----------------------------------------------------------------
# clrs <- c('#943126', '#138D75', '#461904', '#42607B', '#154360')
clrs <- c('#557E4F', '#0E7F86', '#C0A918', '#42607B', '#73B26D')
# tpes <- c('Hot - Moderate', 'Hot - Very wet', 'Very hot - Dry', 'Very cold - Dry', 'Cold - Wet')
tpes <- c('Very hot - Very Wet', 'Hot - Very Wet', 'Very hot - Very Dry', 'Very cold - Dry', 'Hot - Wet')

# Apply functions ---------------------------------------------------------
make_map(rst = rst1, clr = clrs, nms = tpes, tpe = 'wc 14')
make_map(rst = rst2, clr = clrs, nms = tpes, tpe = 'wc 20')

