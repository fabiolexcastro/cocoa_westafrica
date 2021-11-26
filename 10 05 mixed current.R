


# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Functions ---------------------------------------------------------------
make_mixed <- function(tpe, pnt){
  
  # tpe <- tpes[1]
  # pnt <- pn14
  
  cat('Start ', '\n')
  
  # Read the data (Probability raster)
  fls <- glue('{root}/{tpe}/results/raw') %>% dir_ls() 
  cls <- glue('{root}/{tpe}/results/process') %>% dir_ls() %>% grep('lim', ., value = TRUE) %>% raster()
  prb <- grep('Prob', fls, value = TRUE) %>% as.character %>% raster()
  unc <- grep('Unc', fls, value = TRUE) %>% as.character %>% raster()
  
  # Extract the values
  thr.unc <- raster::extract(unc, pnt[,1:2])
  thr.unc <- thr.unc[!is.na(thr.unc)]
  thr.unc <- as.numeric(quantile(thr.unc, 0.1))
  save(thr.unc, file = glue('../rData/{run}/{tpe}/threshold_unc.rData'))
  
  # Probability threshold
  load(glue('../rData/{run}/{tpe}/threshold_prob.rData'))
  
  # Reclassify the values - Mixed
  rsl <- cls
  rsl[which(unc[] < thr.unc & prb[] > thr)] <- max(unique(cls[]), na.rm = TRUE) + 1
  writeRaster(x = rsl, glue('../rf/output/{run}/{tpe}/results/process/RF_clust_unc.tif'), overwrite = TRUE)
  cat('Done\n')
  return(rsl)
  
}

# Load data ---------------------------------------------------------------
run <- 'run_4'
pn14 <- read_csv(glue('../data/tbl/{run}/03 points RF wcl 1 4.csv'))
pn20 <- read_csv(glue('../data/tbl/{run}/03 points RF wcl 2 0.csv'))
root <- glue('../rf/output/{run}/')
tpes <- basename(dir_ls(root))

# Mixed function ----------------------------------------------------------
lm14 <- make_mixed(tpe = 'wc 14', pnt = pn14)
lm20 <- make_mixed(tpe = 'wc 20', pnt = pn20)

par(mfrow = c(1, 2))
plot(lm14)
plot(lm20)
par(mfrow = c(1, 1))





