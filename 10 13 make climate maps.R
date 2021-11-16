


# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Load data ---------------------------------------------------------------
root <- '../data/raster/climate'
dir_ls(root)

# CMIP5 -------------------------------------------------------------------
fls.cmp5 <- dir_ls(glue('{root}/cmip5/rcp60/2040_2069'))
fles <- fls.cm5
fles <- grep('.tif$', fles, value = TRUE)
fles <- mixedsort(fles)
fles <- grep('bio', fles, value = TRUE)

rslt <- map(.x = 1:34, .f = function(i){
  grep(glue('bio_{i}.tif'), fles, value = TRUE) %>% 
    as.character() %>% 
    stack() %>% 
    mean()
})

Map('writeRaster', x = rslt, filename = glue('../data/raster/climate/average/cmip5/bio_{1:34}.tif'))


# CMIP6 -------------------------------------------------------------------
fles <- dir_ls(glue('{root}/cmip6/rcp60/2040_2069'))
fles <- grep('.tif$', fles, value = TRUE)
fles <- mixedsort(fles)
fles <- grep('bio', fles, value = TRUE)
fles <- as.character(fles)

rslt <- map(.x = 1:34, .f = function(i){
  cat(i, '\n')
  grep(glue('_{i}.tif'), fles, value = TRUE) %>% 
    as.character %>% 
    stack() %>% 
    mean()
})

dir_create('../data/raster/climate/average/cmip6')
Map('writeRaster', x = rslt, filename = glue('../data/raster/climate/average/cmip6/bio_{1:34}.tif'))



# Load average ------------------------------------------------------------
fls.cm5 <- dir_ls('../data/raster/climate/average/cmip5') %>% 
  mixedsort %>% 
  as.character()
stk.cm5 <- map(fls.cm5, raster)

fls.cm6 <- dir_ls('../data/raster/climate/average/cmip6') %>% 
  mixedsort %>% 
  as.character()
stk.cm6 <- map(fls.cm6, raster)

lim <- st_read('../data/shp/base/countries_target_4.shp')
wrl <- st_read('../data/shp/base/all_countries.shp') %>% filter(CONTINENT == 'Africa') 

# Make map ----------------------------------------------------------------

# Function ----------------------------------------------------------------
make_map <- function(var, tpe){
  
  # var <- 3
  # tpe <- 'tmp'
  
  cat(var, '/n')
  # Read as a raster
  r14 <- stk.cm5[[var]]
  r20 <- stk.cm6[[var]]
  
  # Condition for the temperature (v1.4)
  cat('Conditional 1/n')
  if(tpe == 'tmp'){
    r14 <- r14 / 10
  }

  # Condition for the color of the legend / units
  cat('Conditional 2/n')
  if (tpe == 'tmp') {
    clr <- 'Heat'
    unt <- 'Temperature (°C)'
  } else if(tpe == 'ppt'){
    clr <- 'Emrld'
    unt <- 'Precipitation (mm)'
  } else {
    clr <- 'TealGrn'
    unt <- 'Evapotranspiration\nPotential (mm)'
  }
  
  # Stack and conversion to table
  stk <- raster::stack(r14, r20)
  tbl <- rasterToPoints(stk, spatial = FALSE)
  tbl <- as_tibble(tbl)
  names(tbl) <- c('lon', 'lat', 'CMIP5', 'CMIP6')
  tbl <- gather(tbl, source, value, -lon, -lat)
  
  # To make the map
  gmp <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = value)) + 
    scale_fill_continuous_sequential(palette = clr, rev = TRUE) +
    geom_sf(data = wrl, fill = NA, lty = 3, col = 'grey') + 
    geom_sf(data = lim, fill = NA) + 
    coord_sf(xlim = extent(lim)[1:2], ylim = extent(lim)[3:4]) +
    ggtitle(label = paste0('Bio ', var)) +
    facet_wrap(.~source) + 
    theme_ipsum_es() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(1.8, 'line'), 
          plot.title = element_text(size = 12, face = 'bold', hjust = 0.5), 
          axis.text.x = element_text(size = 6), 
          axis.text.y = element_text(size = 6), 
          axis.title.x = element_text(size = 8), 
          axis.title.y = element_text(size = 8), 
          legend.title = element_text(size = 8, face = 'bold')) + 
    labs(x = 'Longitude', y = 'Latitude') + 
    north(data = lim, symbol = 12) + 
    labs(fill = unt)
  
  out <- glue('../png/maps/climate/future/bio_{var}.png')
  ggsave(plot = gmp, filename = out, units = 'in', width = 9, height = 3.85, dpi = 300)
  cat('Done\n')
  
}


map(.x = 1:11, .f = function(k) make_map(var = k, tpe = 'tmp'))
map(.x = 12:20, .f = function(k) make_map(var = k, tpe = 'ppt'))
map(.x = 21:34, .f = function(k) make_map(var = k, tpe = 'etp'))

