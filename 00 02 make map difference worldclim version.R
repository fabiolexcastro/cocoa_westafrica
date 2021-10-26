
# Load --------------------------------------------------------------------
source('00 general function.R')
source('01 functions bioclimatic.R')

# Load --------------------------------------------------------------------
lim <- st_read('../data/shp/base/countries_target_4.shp')
wrl <- st_read('../data/shp/base/all_countries.shp') %>% filter(CONTINENT == 'Africa') 
pth.v14 <- '../data/raster/climate/worldclim/2_5min/v1_4'
pth.v20 <- '../data/raster/climate/worldclim/2_5min/v2_0'

fls.v14 <- dir_ls(pth.v14, regexp = '.tif$') 
fls.v14 <- grep('bio_', fls.v14, value = TRUE)
fls.v14 <- mixedsort(fls.v14)

fls.v20 <- dir_ls(pth.v20, regexp = '.tif$')
fls.v20 <- grep('bio_', fls.v20, value = TRUE)
fls.v20 <- mixedsort(fls.v20)

# Function ----------------------------------------------------------------
make_map <- function(var, tpe){
  
  # var <- 'bio_21.tif'
  # tpe <- 'etp'
  
  cat(var, '/n')
  # Read as a raster
  r14 <- raster(grep(var, fls.v14, value = TRUE))
  r20 <- raster(grep(var, fls.v20, value = TRUE))
  
  # Condition for the temperature (v1.4)
  cat('Conditional 1/n')
  if(tpe == 'tmp'){
    r14 <- r14 / 10
  }
  
  # Title
  ttl <- gsub('.tif', '', var)
  ttl <- gsub('_', ' ', ttl)
  ttl <- str_to_title(ttl)
  
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
    unt <- 'Evapotranspiration/nPotential (mm)'
  }

  # Stack and conversion to table
  stk <- raster::stack(r14, r20)
  tbl <- rasterToPoints(stk, spatial = FALSE)
  tbl <- as_tibble(tbl)
  names(tbl) <- c('lon', 'lat', 'Worldclim v1.4', 'Worldclim v2.0')
  tbl <- gather(tbl, source, value, -lon, -lat)
  
  # To make the map
  gmp <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = value)) + 
    scale_fill_continuous_sequential(palette = clr, rev = FALSE) +
    geom_sf(data = wrl, fill = NA, lty = 3, col = 'grey') + 
    geom_sf(data = lim, fill = NA) + 
    coord_sf(xlim = extent(lim)[1:2], ylim = extent(lim)[3:4]) +
    ggtitle(label = ttl) +
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
   
  out <- glue('../png/maps/climate/current/{gsub(".tif", ".jpg", var)}')
  ggsave(plot = gmp, filename = out, units = 'in', width = 9, height = 3.85, dpi = 300)
  cat('Done\n')
  
}


# Make map ----------------------------------------------------------------

# Temperature 
map(.x = 1:11, .f = function(k){
  make_map(var = glue('bio_{k}.tif'), tpe = 'tmp')
})

# Precipitation 
map(.x = 12:19, .f = function(k){
  make_map(var = glue('bio_{k}.tif'), tpe = 'ppt')
})

# Evapotranspiration 
map(.x = 21:29, .f = function(k){
  make_map(var = glue('bio_{k}.tif'), tpe = 'etp')
})

map(.x = 30:33, .f = function(k){
  make_map(var = glue('bio_{k}.tif'), tpe = 'etp')
})


shapefile(ecd, '//catalogue/workspace-cluster9/CLIMA_LOCA/7. Workspace/cadmium/shp/ecu_adm1.shp')
