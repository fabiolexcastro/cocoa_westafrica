

# Load libraries ----------------------------------------------------------
source('00 general function.R')


# Functions ---------------------------------------------------------------
make_modal <- function(tp, rn){

  # tp <- '14'
  # rn <- 'run_2'
  
  cat(rn, '\n')
  fl <- dir_ls(glue('../rf/output/{rn}/wc {tp}/results/process/mixed/2040_2069/'))
  st <- raster::stack(fl)
  md <- raster::modal(st)
  cat('To write\n')
  writeRaster(x = md, file = glue('../rf/output/{rn}/wc {tp}/results/process/mixed/modal_future.tif'), overwrite = TRUE)
  cat('Done!\n')
  
}


# Load data ---------------------------------------------------------------
run <- 'run_2'

# Apply function ----------------------------------------------------------
make_modal(tp = 14, rn = 'run_2')
make_modal(tp = 20, rn = 'run_2')