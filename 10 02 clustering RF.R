
# Load --------------------------------------------------------------------
source('00 general function.R') 

# Functions ---------------------------------------------------------------
source('10 02 random forest functions.R')
read_climate <- function(v){
  cat('Start\n')
  clmt <- glue('{root}/{v}')
  clmt <- dir_ls(clmt)
  clmt <- grep('bio', clmt, value = TRUE)
  clmt <- mixedsort(clmt)
  clmt <- as.character(clmt)
  clmt <- map(.x = clmt, .f = rast)
  clmt <- map(.x = clmt, .f = raster)
  extn <- extent(clmt[[34]])
  clmt <- map(.x = clmt, .f = function(k) raster::crop(k, extn))
  clmt <- raster::stack(clmt)
  cat('Done!\n')
  return(clmt)
}
make_rf <- function(tble){
  
  # tble <- p1_4
  
  cat('Start\n')
  
  # Prepare data
  envv <- as.matrix(tble[,3:ncol(tble)])
  dtRF <- as.data.frame(tble[,3:ncol(tble)])  
  d <- dist(dtRF, method = 'euclidean')
  
  # Tunning RF
  nfrs <- 25
  ntrs <- 100
  nvrs <- 4
  ncls <- 5
  
  # RF Distancies
  dsRF <- RFdist(dtRF, mtry = nvrs, ntrs, nfrs, addcl1 = TRUE, addcl2 = FALSE, imp = T, oob.prox1 = T)
  nopr <- ncls
  lbRF <- pamNew(dsRF$cl1, nopr)
  clDt <- hclust(as.dist(dsRF$cl1), method = 'ward.D2')
  
  cat('Done!\n')
  return(list(lbRF, clDt))
  
}

# Load data ---------------------------------------------------------------

# Run 
run <- 'run_4'

# Study area
limt <- shapefile('../data/shp/base/countries_target_4.shp')

# Points
p1_4 <- read_csv(glue('../data/tbl/{run}/02 points wcl 1 4.csv'))
p2_0 <- read_csv(glue('../data/tbl/{run}/02 points wcl 2 0.csv'))
p1_4 <- filter(p1_4, type == 'Clean')
p2_0 <- filter(p2_0, type == 'Clean')

# Climate
root <- '../data/raster/climate/worldclim/2_5min'
c1_4 <- read_climate(v = 'v1_4')
c2_0 <- read_climate(v = 'v2_0')

names(c1_4) <- gsub('_v2', '', names(c1_4))
names(c2_0) <- gsub('_v2', '', names(c2_0))

# Get values for the points -----------------------------------------------
p1_4 <- as_tibble(cbind(p1_4[,1:2], raster::extract(c1_4, p1_4[,1:2])))
p2_0 <- as_tibble(cbind(p2_0[,1:2], raster::extract(c2_0, p2_0[,1:2])))

# Cleaning variables ------------------------------------------------------
vif_14 <- vifstep(x = as.data.frame(p1_4[,3:ncol(p1_4)]), th = 5)
vif_20 <- vifstep(x = as.data.frame(p2_0[,3:ncol(p2_0)]), th = 5)
vif_14 <- as.character(vif_14@results$Variables)
vif_20 <- as.character(vif_20@results$Variables)
vif_14 <- ifelse(vif_14 == 'bio_20', 'bio_21', vif_14)
vif_20 <- ifelse(vif_20 == 'bio_20', 'bio_21', vif_20)

# Another way
vars <- colnames(read_csv('../data/tbl/run_2/04 all cls 20.csv'))[-1]

p1_4 <- drop_na(dplyr::select(p1_4, X, Y, vif_14))
p2_0 <- drop_na(dplyr::select(p2_0, X, Y, vif_20))

p1_4 <- drop_na(dplyr::select(p1_4, X, Y, vars))
p2_0 <- drop_na(dplyr::select(p2_0, X, Y, vars))

# Clustering using random forest ------------------------------------------
rf14 <- make_rf(tble = p1_4)
lb14 <- rf14[[1]]
p1_4 <- mutate(p1_4, class = lb14)

rf20 <- make_rf(tble = p2_0)
lb20 <- rf20[[1]]
p2_0 <- mutate(p2_0, class = lb20)

dir.create('../png/dendogram/run_4')

# Dendogram graph ---------------------------------------------------------
dd14 <- fviz_dend(rf14[[2]], k = 5, 
                  k_color = brewer.pal(n = 5, name = 'Dark2'), 
                  cex = 0.5, main = 'Dendogram', xlab = 'Objects', ylab = 'Distance')
ggsave(plot = dd14, filename = glue('../png/dendogram/{run}/dd_wc14.png'), 
       units = 'in', width = 9, height = 7, dpi = 300)

dd20 <- fviz_dend(rf20[[2]], k = 5, 
                  k_color = brewer.pal(n = 5, name = 'Dark2'), 
                  cex = 0.5, main = 'Dendogram', xlab = 'Objects', ylab = 'Distance')
ggsave(plot = dd20, filename = glue('../png/dendogram/{run}/dd_wc20.png'), 
       units = 'in', width = 9, height = 7, dpi = 300)

# Class data --------------------------------------------------------------
cd14 <- as_tibble(cbind(pb = as.factor(lb14), p1_4[,3:ncol(p1_4)]))
cd20 <- as_tibble(cbind(pb = as.factor(lb20), p2_0[,3:ncol(p2_0)]))

dir.create('../data/rds/run_2')
saveRDS(object = cd14, file = glue('../data/rds/{run}/cd14.rds'))
saveRDS(object = cd20, file = glue('../data/rds/{run}/cd20.rds'))

# Write these files -------------------------------------------------------

# Worldclim 1.4
write.csv(p1_4, glue('../data/tbl/{run}/03 points RF wcl 1 4.csv'), row.names = FALSE)
cl14 <- rf14[[2]]
saveRDS(object = cl14, file = glue('../data/rds/{run}/cl14.rds'))

# Worldclim 2.0
write.csv(p2_0, glue('../data/tbl/{run}/03 points RF wcl 2 0.csv'), row.names = FALSE)
cl20 <- rf20[[2]]
saveRDS(object = cl20, file = glue('../data/rds/{run}/cl20.rds'))
