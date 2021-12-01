
# Load libraries ----------------------------------------------------------
source(file = './00 general function.R')

# Add limitations / mixed -------------------------------------------------
get_zns <- function(prc.prb, prc.unc){
  
  # prc <- 1
  # prc.prb <- 10
  # prc.unc <- 25
  
  # Filtering the threshold
  thr.prb <- filter(thrs, percentage == prc.prb) %>% pull(2)
  thr.unc <- filter(qntl, percentage == prc.unc) %>% pull(2)
  
  # Matrix
  mcl.prb <- matrix(c(0, thr.prb, 0, thr.prb, 1, 1), ncol = 3, nrow = 2, byrow = TRUE)
  mcl.cls <- matrix(data = c(0.5, 2.5, 0, 2.5, 7.5, 1), ncol = 3, byrow = TRUE)
  
  # Bianary raster (prob and cluster)
  bcl <- raster::reclassify(x = mdal, rcl = mcl.cls)
  bpr <- raster::reclassify(x = prob, rcl = c(0, thr.prb, 0, thr.prb, 1, 2))
  
  # To generate the limitations raster
  rsl <- mdal
  dff <- bpr - bcl
  rsl[which(dff[] == -1)] <- 8
  rsl[which(dff[] ==  2)] <- 8
  
  # To generate the mixed raster
  rsl[which(uncr[] < thr.unc  & prob[] > thr.prb)] <- 9
  brs <- raster::reclassify(x = rsl, rcl = c(0, 2.5, 0, 2.5, 7.5, 1, 7.5, 8.5, 2, 8.5, 9.5, 3))
  plot(brs)
  
  # To make the map
  lbl <- data.frame(x = c(0, 1, 2, 3), y = c('Unsuitable', 'Suitable', 'Limitations', 'Mixed'))
  tbl <- raster::rasterToPoints(x = brs, spatial = FALSE) %>% as_tibble()
  colnames(tbl) <- c('x', 'y', 'value')
  tbl <- tbl %>% mutate(class = ifelse(value == 0, 'Unsuitable', ifelse(value == 1, 'Suitable', ifelse(value == 2, 'Limitations', 'Mixed'))))
  tbl <- tbl %>% mutate(class = factor(class, levels = c('Unsuitable', 'Suitable', 'Limitations', 'Mixed')))
  
  # Ggplot
  gmp <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = class)) + 
    scale_fill_manual(values = c('white', 'darkgreen', 'darkgrey', '#ffffc6')) +
    geom_sf(data = wrld, fill = NA) + 
    ggtitle(label = glue('Percentile: {prc.prb}%-{prc.unc}% / Thr prob: {round(thr.prb, 2)} / Thr uncr: {round(thr.unc, 2)}')) +
    coord_sf(xlim = extent(brs)[1:2], ylim = extent(brs)[3:4]) + 
    theme_void() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 14, hjust = 0.5)) +
    labs(x = 'Longitude', y = 'Latitude', fill = '')
  
  cat('Done\n')
  return(gmp)
  
}


# Load data ---------------------------------------------------------------
pnts <- read_csv(glue('../data/tbl/run_4/03 points RF wcl 2 0.csv'))[,1:2]
# wrld <- st_read('../data/shp/base/all_countries.shp') %>% filter(CONTINENT == 'Africa')
wrld <- st_read('../data/shp/base/countries_target_4.shp')

# Clustering
fles <- dir_ls('../rf/output/run_4/wc 20/results/raw/future', regexp = '.tif$')
fles <- grep('clust', fles, value = T)
fles <- as.character(fles)

# To calculate the modal raster -------------------------------------------
stck <- raster::stack(fles)
mdal <- raster::modal(stck)

# Probability -------------------------------------------------------------
fles <- dir_ls('../rf/output/run_4/wc 20/results/raw/future', regexp = '.tif$')
fles <- grep('rob', fles, value = T)
fles <- as.character(fles)

prob <- raster::stack(fles)
prob <- raster::mean(prob)

# To calculate the average uncertainty ------------------------------------
fles <- dir_ls('../rf/output/run_4/wc 20/results/raw/future')
fles <- grep('uncrt', fles, value = T)
fles <- as.character(fles)

# To calculate the average ------------------------------------------------
stck <- raster::stack(fles)
uncr <- raster::mean(stck)

# Different thresholds ----------------------------------------------------

# Probability
thrs <- read_csv('../data/tbl/run_4/06 thr prob 2 0.csv')

# Uncertainty
pnts <- mutate(pnts, uncr = raster::extract(uncr, pnts[,1:2]))
qntl <- rownames_to_column(as.data.frame(quantile(pnts$uncr, seq(0, 1, 0.01))))
colnames(qntl) <- c('percentage', 'uncr')
qntl <- as_tibble(qntl)
qntl <- mutate(qntl, percentage = parse_number(percentage))

# Create rasters and map --------------------------------------------------

# Combinations 
cmbn <- expand.grid(seq(5, 25, 5), c(5, 25, 5))
plts <- map2(.x = c(5, 10, 15, 20, 25, 10), .y = c(15, 15, 15, 15, 20, 20), .f = get_zns) # plts <- map(.x = 1:12, .f = get_zns)
plts <- map2(.x = cmbn$Var1, .y = cmbn$Var2, .f = get_zns)
gall <- ggarrange(plotlist = plts, ncol = 4, nrow = 4)
ggsave(plot = gall, filename = '../png/maps/thr/limitations_mixed_future_modal_2_0_v3.jpg', units = 'in', width = 20, height = 20, dpi = 300)


