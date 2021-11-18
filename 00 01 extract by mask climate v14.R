

# Load --------------------------------------------------------------------
source('00 general function.R')
source('01 functions bioclimatic.R')

# Load --------------------------------------------------------------------
path <- '//alliancedfs.alliance.cgiar.org/data_cluster_4/observed/gridded_products/worldclim/Global_2_5min'
dout <- '../data/raster/climate/worldclim/2_5min/v1_4/'
fles <- dir_ls(path)
limt <- shapefile('../data/shp/base/countries_target_4.shp')

# Extract by mask variables  ----------------------------------------------
prec <- grep('prec', fles, value = TRUE) %>% 
  mixedsort() %>% 
  as.character() %>% 
  grep(paste0(paste0('prec_', 1:12, '$'), collapse = '|'), ., value = TRUE) %>% 
  stack() %>% 
  raster::crop(., limt) %>% 
  raster::mask(., limt)
tmax <- grep('tmax', fles, value = TRUE) %>% 
  mixedsort() %>% 
  as.character() %>% 
  grep(paste0(paste0('tmax_', 1:12, '$'), collapse = '|'), ., value = TRUE) %>% 
  stack() %>% 
  raster::crop(., limt) %>% 
  raster::mask(., limt) 
tmin <- grep('tmin', fles, value = TRUE) %>% 
  mixedsort() %>% 
  as.character() %>% 
  grep(paste0(paste0('tmin_', 1:12, '$'), collapse = '|'), ., value = TRUE) %>% 
  raster::stack() %>% 
  raster::crop(., limt) %>% 
  raster::mask(., limt)
tavg <- grep('tmean', fles, value = TRUE) %>% 
  mixedsort() %>%
  as.character() %>% 
  grep(paste0(paste0('tmean_', 1:12, '$'), collapse = '|'), ., value = TRUE) %>% 
  raster::stack() %>% 
  raster::crop(., limt) %>% 
  raster::mask(., limt)

# Output directory -------------------------------------------------------
dout <- '../data/raster/climate/worldclim/2_5min/v1_4'

# Bioclimatic variables 1 - 19 --------------------------------------------
bios <- dismo::biovars(prec = stack(prec), tmax = stack(tmax), tmin = stack(tmin))
Map('writeRaster', x = unstack(bios), filename = glue('{dout}/bio_{1:19}.tif'), overwrite = TRUE)

# Bioclimatic 20 ----------------------------------------------------------
precbin <- reclassify(stack(prec), c(-Inf, 100, 1, 100, Inf, NA))
prectwo <- addLayer(precbin, precbin)
allp <- stack()
for(i in 1:12){
  oney <- prectwo[[i:(i + 11)]]
  drym <- cumsum(oney)
  maxn <- max(drym, na.rm = TRUE)
  allp <- addLayer(allp, maxn)
}

bio_20 <- max(allp, na.rm = TRUE)
writeRaster(bio_20, glue('{dout}/bio_20.tif'))

# ETP Bioclimatic ---------------------------------------------------------

# Normal solar radiation
srad <- list.files('//alliancedfs.alliance.cgiar.org/data_cluster_4/observed/gridded_products/worldclim/Global_2_5min_v2', full.names = TRUE, pattern = '.tif$') %>% 
  grep('srad', ., value = TRUE) %>% 
  mixedsort %>% 
  stack() %>% 
  raster::crop(., limt) %>% 
  raster::mask(., limt)

# Conversion Solar Radiation - Normal solar radiation
srad <- srad * 0.408/1000
Map('writeRaster', x = unstack(srad), filename = glue('../data/raster/srad/xtt_srad_{1:12}.tif'))

# Extratterrestre solar radiation
srad <- list.files('//catalogue/workspace-cluster9/DATA/1.Data/SolarRadiationExtraterrestre/ET_SolRad', full.names = T) %>% 
  grep(paste0(paste0('et_solrad_', 1:12, '$'), collapse = '|'), ., value = TRUE) %>% 
  mixedsort() %>% 
  stack() %>% 
  raster::crop(., limt) %>% 
  raster::mask(., limt)

# Resampling SRAD -----------------------------------------------------------
srad <- raster::resample(srad, tmax, method = 'bilinear')
srad <- srad * c(31,29,31,30,31,30,31,31,30,31,30,31)

tmax <- tmax/10
tmin <- tmin/10
tavg <- tavg/10

# ETP way one ---------------------------------------------------------------
etps <- 0.0023 * srad * sqrt(reclassify(tmax - tmin, c(-Inf, 0, 0))) * (tavg + 17.8)
names(etps) <- paste0('etp_', 1:12)
etps <- round(etps, 0)
etps <- etps * c(31,29,31,30,31,30,31,31,30,31,30,31)

# ETP way two -------------------------------------------------------------
etps <- 0.0013 * 0.408 * srad * (tavg + 17) * (tmax - tmin - 0.0123 * prec) ^ 0.76
Map('writeRaster', x = unstack(etps), filename = glue('{dout}/etp_{1:12}_v2.tif'), overwrite = TRUE)

# Map('writeRaster', x = unstack(etps), filename = glue('{dout}/etp_{1:12}.tif'), overwrite = TRUE)
Map('writeRaster', x = unstack(tmax), filename = glue('{dout}/tmax_{1:12}.tif'), overwrite = TRUE)
Map('writeRaster', x = unstack(tmin), filename = glue('{dout}/tmin_{1:12}.tif'), overwrite = TRUE)
Map('writeRaster', x = unstack(prec), filename = glue('{dout}/prec_{1:12}.tif'), overwrite = TRUE)
Map('writeRaster', x = unstack(tavg), filename = glue('{dout}/tmean_{1:12}.tif'), overwrite = TRUE)

# ETP bioclimatics --------------------------------------------------------
nrow(rasterToPoints(etps))
nrow(rasterToPoints(prec))

etpr <- cbind(as.matrix(etps),as.matrix(prec),as.matrix(tavg))
etbi <- t(apply(etpr, 1, etpvars))
nmes <- paste0('bio_', 21:29)
zero <- prec[[1]]
zero <- zero * 0 + 1
names(zero) <- 'zero'

map(.x = 1:ncol(etbi), .f = function(k){
  print(k)
  lyer <- zero
  values(lyer) <- etbi[,k]
  plot(lyer)
  writeRaster(lyer, filename = glue('{dout}/{nmes[k]}_v2.tif'), overwrite = TRUE)
})

# Biovariables 30 to 33 ---------------------------------------------------
dfct <- prec - etps
dftm <- cbind(as.matrix(dfct), as.matrix(tmin), as.matrix(tmax))
bios <- t(apply(dftm, 1, cumTemp))
nmes <- paste0('bio_', 30:33)

map(.x = 1:ncol(bios), .f = function(k){
  print(k)
  lyer <- zero
  values(lyer) <- bios[,k]
  writeRaster(lyer, filename = glue('{dout}/{nmes[k]}_v2.tif'), overwrite = TRUE)
})

# Bioclimatic 34 ----------------------------------------------------------
ppt <- prec
names(ppt) <- glue('prec_{1:12}')
ssn <- list(1:6, 2:7, 3:8, 4:9, 5:10, 6:11, 7:12, 8:13, 9:14, 10:15, 11:16, 12:17)
ptw <- addLayer(ppt, ppt)
ptb <- ptw %>% rasterToPoints() %>% as_tibble()
ptb <- ptb %>% mutate(gid = 1:nrow(.)) %>% dplyr::select(gid, x, y, everything())
ptb <- ptb %>% dplyr::select(gid:prec_5.2)
coords <- ptb %>% dplyr::select(1:3)

get_max <- function(pix){
  
  vls <- ptb %>% slice(pix) %>% .[4:ncol(.)] %>% as.numeric()
  max <- map(.x = 1:length(ssn), .f = function(k){sum(vls[ssn[[k]]])}) %>% unlist() %>% .[which.max(.)]
  return(max)
  
}

library(future); library(furrr)
plan(multisession, workers = 4)
rslt <- coords %>% mutate(max_prec = furrr::future_map(.x = gid, .f = get_max))
future:::ClusterRegistry("stop"); gc(reset = T)
rslt <- rslt %>% unnest(max_prec) 
rstr <- rslt %>% dplyr::select(2:4) %>% rasterFromXYZ()

cat('To write the final raster')
writeRaster(x = rstr, 
            filename = glue('{dout}/bio_34.tif'), overwrite = TRUE)

plot(rstr)
plot(raster(glue('{dout}/bio_12.tif')))
max(rstr[], na.rm = TRUE)
max(raster(glue('{dout}/bio_12.tif'))[], na.rm = TRUE)
