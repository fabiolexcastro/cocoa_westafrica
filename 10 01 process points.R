

# Load --------------------------------------------------------------------
source('00 general function.R')

# Functions to use --------------------------------------------------------
read_climate <- function(fls){
  cat('Start\n')
  fls <- mixedsort(fls)
  fls <- grep('bio', fls, value = TRUE)
  stk <- map(.x = fls, .f = raster)
  ext <- extent(stk[[34]])
  stk <- map(.x = stk, .f = function(k) raster::crop(k, ext))
  stk <- raster::stack(stk)
  names(stk) <- glue('bio_{1:34}')
  cat('Done!\n')
  return(stk)
}
rmve_otliers <- function(dfm){
  norm <- scores(dfm[,3:ncol(dfm)], 'z')
  norm_na <- norm
  norm_na[abs(norm_na) > 3.5] <- NA
  normpoints <- cbind(dfm[,c('x', 'y')], norm_na)
  
  post <- which(is.na(normpoints), arr.ind=TRUE)[,1]
  post <- as_tibble(normpoints[unique(post),1:2])
  
  norm_naomt <- na.omit(normpoints) 
  norm_naomt <- as_tibble(norm_naomt)
  norm_naomt <- norm_naomt[,c('x', 'y')]
  print('Done...!')
  return(list(norm_naomt, post))
}

# Load data ---------------------------------------------------------------
pnts <- read_csv('../data/tbl/run_1/01 points 10 km.csv')
shpf <- shapefile('../data/shp/base/countries_target_4.shp')
shpf <- st_as_sf(shpf)

# Climate data 
f1_4 <- dir_ls('../data/raster/climate/worldclim/2_5min/v1_4') 
f2_0 <- dir_ls('../data/raster/climate/worldclim/2_5min/v2_0')
s1_4 <- read_climate(f1_4)
s2_0 <- read_climate(f2_0)

# get values --------------------------------------------------------------
v1_4 <- drop_na(as_tibble(cbind(pnts[,1:2], raster::extract(s1_4, pnts[,1:2]))))
v2_0 <- drop_na(as_tibble(cbind(pnts[,1:2], raster::extract(s2_0, pnts[,1:2])))) 

# Remove possible outliers ------------------------------------------------
r1_4 <- rmve_otliers(v1_4)
r2_0 <- rmve_otliers(v2_0)

# Map of the outliers -----------------------------------------------------
r1_4 <- rbind(r1_4[[1]] %>% mutate(type = 'Clean'), r1_4[[2]] %>% mutate(type = 'Removed'))
r1_4 <- mutate(r1_4, source = 'Worldclim v1.4')
r2_0 <- rbind(r2_0[[1]] %>% mutate(type = 'Clean'), r2_0[[2]] %>% mutate(type = 'Removed'))
r2_0 <- mutate(r2_0, source = 'Worldclim v2.0')
tall <- rbind(r1_4, r2_0)
tall <- st_as_sf(tall, coords = c('x', 'y'), crs = st_crs(4326))
tall <- mutate(tall, type = factor(type, levels = c('Clean', 'Removed')))

tall %>% as_tibble %>% group_by(type, source) %>% summarise(count = n())

gall <- ggplot() + 
  geom_sf(data = tall, aes(col = type), size = 0.3) + 
  scale_color_manual(values = c('#BA7F0F', '#ED0808')) +
  facet_wrap(.~ source ) + 
  geom_sf(data = shpf, fill = NA) + 
  coord_sf() + 
  theme_ipsum_es() + 
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7), 
        legend.text = element_text(size = 11), 
        strip.text = element_text(size = 12, face = 'bold')) +
  labs(x = 'Longitude', y = 'Latitude', col = '') +
  guides(color = guide_legend(override.aes = list(size = 5)))

dir_create('../png/maps/points')

ggsave(plot = gall, 
       filename = '../png/maps/points/removed_outliers_run1.png', 
       units = 'in', width = 9, height = 4, dpi = 300)

# Write the results -----------------------------------------------------
tall

tble <- st_coordinates(tall) %>% 
  as_tibble %>% 
  mutate(type = pull(tall, type), source = pull(tall, source))

wcl1 <- tble %>% filter(source == 'Worldclim v1.4')
wcl2 <- tble %>% filter(source == 'Worldclim v2.0')

write.csv(wcl1, '../data/tbl/run_1/02 points wcl 1 4.csv', row.names = FALSE)
write.csv(wcl2, '../data/tbl/run_1/02 points wcl 2 0.csv', row.names = FALSE)



