

# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Functions ---------------------------------------------------------------
make_limitations <- function(tpe, pnt){
  
  # tpe <- tpes[1]
  # pnt <- pn14
  
  cat('Start ', '\n')
  
  # Read the data (Probability raster)
  fls <- glue('{root}/{tpe}/results/raw') %>% dir_ls() 
  rst <- grep('Prob', fls, value = TRUE) %>% 
    as.character %>% 
    raster()
  
  # Extract the values
  vls <- raster::extract(rst, pnt[,1:2])
  vls <- vls[!is.na(vls)]
  qnt_05 <- quantile(vls, seq(0, 1, 0.01))
  dfm <- as.data.frame(qnt_05)
  thr <- as.numeric(subset(dfm, rownames(dfm) == '5%'))
  save(thr, file = glue('../rData/{run}/{tpe}/threshold_prob.rData'))
  cat('Done threshold\n')
  
  # Make limitations
  cls <- grep('Clust', fls, value = TRUE) %>% raster()
  mtx_prb <- matrix(c(0, thr, 0, thr, 1, 2), ncol = 3, byrow = TRUE)
  no.absenceclasses <- 2
  no.clusters <- 5
  mtx_cls <- matrix(c(0.5, no.absenceclasses + 0.5, 0, no.absenceclasses + 0.5, no.absenceclasses + no.clusters + 0.5, 1), nrow = 2, byrow = T)
  
  # Reclassification
  prb_rcl <- raster::reclassify(x = rst, rcl = mtx_prb)
  cls_rcl <- raster::reclassify(x = cls, rcl = mtx_cls)
  
  # To make the difference
  dff <- prb_rcl - cls_rcl
  rsl <- cls
  rsl[which(dff[] == -1)] <- no.absenceclasses + no.clusters + 1
  rsl[which(dff[] ==  2)] <- no.absenceclasses + no.clusters + 1
  
  writeRaster(x = rsl, glue('../rf/output/{run}/{tpe}/results/process/RF_clust_lim.tif'), overwrite = TRUE)
  cat('Done\n')
  return(rsl)
  
}

# Load data ---------------------------------------------------------------
run <- 'run_2'
pn14 <- read_csv('../data/tbl/run_2/03 points RF wcl 1 4.csv')
pn20 <- read_csv('../data/tbl/run_2/03 points RF wcl 2 0.csv')
root <- '../rf/output/run_2/'
tpes <- basename(dir_ls(root))

# Limitations -------------------------------------------------------------
lm14 <- make_limitations(tpe = 'wc 14', pnt = pn14)
lm20 <- make_limitations(tpe = 'wc 20', pnt = pn20)

par(mfrow = c(1, 2))
plot(lm14)
plot(lm20)
par(mfrow = c(1, 1))



