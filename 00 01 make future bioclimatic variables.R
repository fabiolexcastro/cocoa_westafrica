

# Load --------------------------------------------------------------------
source('00 general function.R')
source('01 functions bioclimatic.R')

# Functions to use --------------------------------------------------------
get_etp_cm5 <- function(gcm){
  
  cat('Start ', gcm, '\n')
  # Filtering the GCM
  ppt <- grep(glue('{gcm}_prec'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  tmx <- grep(glue('{gcm}_tmax'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  tmn <- grep(glue('{gcm}_tmin'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  
  # Read as a raster
  ppt <- raster::stack(ppt)
  tmx <- raster::stack(tmx)
  tmn <- raster::stack(tmn)
  
  # Change units temperature
  tmx <- tmx / 10
  tmn <- tmn / 10
  
  # Average temperature
  tav <- (tmx + tmn) / 2
  
  # Change the names
  names(ppt) <- glue('prec_{1:12}')
  names(tmx) <- glue('tmax_{1:12}')
  names(tmn) <- glue('tmin_{1:12}')
  names(tav) <- glue('tavg_{1:12}')
  
  # Resampling srad # Source: https://cgiarcsi.community/data/global-aridity-and-pet-database/ 
  srad <- raster::resample(srad, ppt, method = 'bilinear')
  
  # To calculte the ETP  Hargreaves Modified:
  # Source: https://link.springer.com/content/pdf/10.1023/A:1015508322413.pdf
  etp <- 0.0013 * 0.408 * srad * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76 
  names(etp) <- glue('etp_{1:12}') 
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  etp <- round(etp, 0)
  
  # Chech Nans
  for(i in 1:12){
    print(i)
    etp[[i]][which(is.nan(etp[[i]][]))] <- 0
  }
  
  out <- glue('../data/raster/climate/cmip5/rcp60/2040_2069/{gcm}_etp_{1:12}.tif')
  Map('writeRaster', x = unstack(etp), filename = out, overwrite = TRUE)
  cat('Done\n')
  
}

get_bio_cm5 <- function(gcm){
  
  cat('Start ', gcm, '\n')
  
  # Filtering the GCM
  ppt <- grep(glue('{gcm}_prec'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  tmx <- grep(glue('{gcm}_tmax'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  tmn <- grep(glue('{gcm}_tmin'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  etp <- grep(glue('{gcm}_etp'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  
  # Read as a raster
  ppt <- raster::stack(ppt)
  tmx <- raster::stack(tmx)
  tmn <- raster::stack(tmn)
  etp <- raster::stack(etp)
  
  # Change units temperature
  tmx <- tmx / 10
  tmn <- tmn / 10
  
  # Average temperature
  tav <- (tmx + tmn) / 2
  
  # To calculate the bioclimatic variables
  mtx <- cbind(as.matrix(etp), as.matrix(ppt), as.matrix(tav))
  etb <- t(apply(mtx, 1, etpvars))
  dot <- glue('../data/raster/climate/cmip5/rcp60/2040_2069/{gcm}_bio_{21:29}.tif')
  zro <- ppt[[1]]
  
  map(1:ncol(etb), function(i){
    cat(i, '\n')
    lyr <- zro
    values(lyr) <- etb[,i]
    writeRaster(lyr, filename = dot[i], overwrite = TRUE)
  })
  
  # To calculate other bioclimatic variables
  dfc <- ppt - etp
  dft <- cbind(as.matrix(dfc), as.matrix(tmn), as.matrix(tmx))
  bio <- t(apply(dft, 1, cumTemp))
  dot <- glue('../data/raster/climate/cmip5/rcp60/2040_2069/{gcm}_bio_{30:33}.tif')
  zro <- ppt[[1]]
  
  map(1:ncol(bio), function(i){
    cat(i, '\n')
    lyr <- zro
    values(lyr) <- bio[,i]
    writeRaster(lyr, filename = dot[i], overwrite = TRUE)
  })
  
  cat('Done\n')
}
get_etp_cm6 <- function(gcm){
  
  cat('Start ', gcm, '\n')
  # Filtering the GCM
  fls <- grep(gcm, fles, value = TRUE)
  ppt <- grep(glue('prec'), mixedsort(as.character(grep(gcm, fls, value = TRUE))), value = TRUE)
  tmx <- grep(glue('tmax'), mixedsort(as.character(grep(gcm, fls, value = TRUE))), value = TRUE)
  tmn <- grep(glue('tmin'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  
  # Read as a raster
  ppt <- raster::stack(ppt)
  tmx <- raster::stack(tmx)
  tmn <- raster::stack(tmn)
  
  # Average temperature
  tav <- (tmx + tmn) / 2
  
  # Change the names
  names(ppt) <- glue('prec_{1:12}')
  names(tmx) <- glue('tmax_{1:12}')
  names(tmn) <- glue('tmin_{1:12}')
  
  # Resampling srad
  srad <- raster::resample(srad, ppt, method = 'bilinear')
  
  # To calculte the ETP 
  etp <- 0.0013 * 0.408 * srad * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76 
  names(etp) <- glue('etp_{1:12}')
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  etp <- round(etp, 0)
  
  # Chech Nans
  for(i in 1:12){
    print(i)
    etp[[i]][which(is.nan(etp[[i]][]))] <- 0
  }
  
  out <- glue('../data/raster/climate/cmip6/rcp60/2040_2069/etp_{gcm}_ssp370_2041-2060_{1:12}.tif')
  Map('writeRaster', x = unstack(etp), filename = out, overwrite = TRUE)
  cat('Done\n')
  
}
get_bio_cm6 <- function(gcm){
  
  cat('Start ', gcm, '\n')
  
  # Filtering the GCM
  fls <- grep(gcm, fles, value = TRUE)
  ppt <- grep(glue('prec'), mixedsort(as.character(grep(gcm, fls, value = TRUE))), value = TRUE)
  tmx <- grep(glue('tmax'), mixedsort(as.character(grep(gcm, fls, value = TRUE))), value = TRUE)
  tmn <- grep(glue('tmin'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  etp <- grep(glue('etp'), mixedsort(as.character(grep(gcm, fles, value = TRUE))), value = TRUE)
  
  # Read as a raster
  ppt <- raster::stack(ppt)
  tmx <- raster::stack(tmx)
  tmn <- raster::stack(tmn)
  etp <- raster::stack(etp)
  
  # Average temperature
  tav <- (tmx + tmn) / 2
  
  # To calculate the bioclimatic variables
  mtx <- cbind(as.matrix(etp), as.matrix(ppt), as.matrix(tav))
  etb <- t(apply(mtx, 1, etpvars))
  dot <- glue('../data/raster/climate/cmip6/rcp60/2040_2069/bio_{gcm}_ssp370_2041-2060_{21:29}.tif')
  zro <- ppt[[1]]
  
  map(1:ncol(etb), function(i){
    cat(i, '\n')
    lyr <- zro
    values(lyr) <- etb[,i]
    writeRaster(lyr, filename = dot[i], overwrite = TRUE)
  })
  
  # To calculate other bioclimatic variables
  dfc <- ppt - etp
  dft <- cbind(as.matrix(dfc), as.matrix(tmn), as.matrix(tmx))
  bio <- t(apply(dft, 1, cumTemp))
  dot <- glue('../data/raster/climate/cmip6/rcp60/2040_2069/bio_{gcm}_ssp370_2041-2060_{30:33}.tif')
  zro <- ppt[[1]]
  
  map(1:ncol(bio), function(i){
    cat(i, '\n')
    lyr <- zro
    values(lyr) <- bio[,i]
    writeRaster(lyr, filename = dot[i], overwrite = TRUE)
  })
  
  cat('Done\n')
}

# -------------------------------------------------------------------------
# Srad general 
# -------------------------------------------------------------------------
srad <- dir_ls('../data/raster/srad', regexp = '.xtt')
srad <- mixedsort(srad)
srad <- raster::stack(srad)
names(srad) <- glue('srad_{1:12}')

# -------------------------------------------------------------------------
# CMIP5 -------------------------------------------------------------------
# -------------------------------------------------------------------------
fles <- dir_ls('../data/raster/climate/cmip5/rcp60/2040_2069', regexp = '.tif$')

# Get each GCM
gcms <- gsub('_prec_1.tif', '', basename(grep('prec_1.tif$', fles, value = TRUE)))

# To calculate the ETPs ---------------------------------------------------
map(.x = gcms, .f = get_etp_cm5)

# To calculate the bioclimatic variables ----------------------------------
map(.x = gcms, .f = get_bio_cm5)

# -------------------------------------------------------------------------
# CMIP6 -------------------------------------------------------------------
# -------------------------------------------------------------------------
fles <- dir_ls('../data/raster/climate/cmip6/rcp60/2040_2069', regexp = '.tif$')

# Get the name of each GCM
gcms <- grep('prec', fles, value = TRUE)
gcms <- as.character(gcms)
gcms <- gsub('prec_', '', basename(grep('_1.tif$', gcms, value = TRUE)))
gcms <- gsub('_ssp370_2041-2060_1.tif', '', gcms)

# To calculate the ETPs ---------------------------------------------------
map(.x = gcms, .f = get_etp_cm6)

# To calculate the bioclimatic variables ----------------------------------
fles <- dir_ls('../data/raster/climate/cmip6/rcp60/2040_2069', regexp = '.tif$')
map(.x = gcms, .f = get_bio_cm6)
