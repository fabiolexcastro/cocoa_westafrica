
# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Load data ---------------------------------------------------------------
run <- 'run_2'

# Limitations future ------------------------------------------------------
get_limitations <- function(tp, gc, cl, pr, th){
  
  cat('Start\n')
  pr <- raster(pr)
  cl <- raster(cl)
  
  cat('Reclassify rasters\n')
  mt.pr <- matrix(c(0, th, 0, th, 1, 2), ncol = 3, byrow = T)
  mt.cl <- matrix(c(0.5, no.absenceclasses + 0.5, 0, no.absenceclasses + 0.5, no.absenceclasses + no.clusters + 0.5, 1), nrow = 2, byrow = T)
  
  pr.rc <- raster::reclassify(pr, mt.pr)
  cl.rc <- raster::reclassify(cl, mt.cl)
  
  cat('Make the difference\n')
  df <- pr.rc - cl.rc
  rs <- cl
  rs[which(df[] == -1)] <- no.absenceclasses + no.clusters + 1
  rs[which(df[] == 2)]  <- no.absenceclasses + no.clusters + 1
  
  cat('To write the raster')
  ou <- glue('../rf/output/{run}/wc {tp}/results/process/limitations/2040_2069')
  writeRaster(x = rs, glue('{ou}/rf_5_limitations_{gc}.tif'), overwrite = TRUE)
  
  
}
make_limitations <- function(run, tpe, clm){
  
  run <- run
  tpe <- '14'
  clm <- 'cmip5'
  
  # Cluster and absences
  no.absenceclasses <- 2
  no.clusters <- 5
  
  # GCM 
  if(clm == 'cmip5'){
    
    cat('CMIP5 \n')
    fls <- glue('../data/raster/climate/cmip5/rcp60/2040_2069') %>% 
      dir_ls(regexp = '.tif$') %>% 
      grep('bio', ., value = TRUE) %>% 
      as.character() %>% 
      mixedsort()
    
    gcm <- basename(fls) %>% 
      grep('bio_1.tif', ., value = TRUE) %>% 
      mixedsort() %>% 
      gsub('_bio_1.tif', '', .)
    
    prb <- glue('../rf/output/run_2/wc 14/results/raw/future') %>% 
      dir_ls() %>% 
      grep('probl', ., value = TRUE) %>% 
      as.character()
  
    cls <- glue('../rf/output/run_2/wc 14/results/raw/future') %>% 
      dir_ls() %>% 
      grep('clust', ., value = TRUE) %>% 
      as.character()
    
    load(glue('../rData/run_2/wc 14/threshold_prob.rData'))
    
    map(.x = 1:length(gcm), .f = function(k) get_limitations(tp = 14, gc = gcm[k], pr = prb[k], cl = cls[k], th = thr))
    cat('Done\n')
      
  } else {
    
    cat('CMIP6 \n')
    fls <- glue('../data/raster/climate/cmip6/rcp60/2040_2069') %>% 
      dir_ls() %>% 
      grep('bio', ., value = TRUE) %>% 
      as.character() %>% 
      mixedsort()
    
    gcm <- basename(fls) %>% 
      grep('_1.tif', ., value = TRUE) %>% 
      gsub('bio_', '', .) %>% 
      gsub('_1.tif', '', .) %>% 
      gsub('_spp370_2041-2060', '', .)
    
    prb <- glue('../rf/output/run_2/wc 20/results/raw/future') %>% 
      dir_ls() %>% 
      grep('probl', ., value = TRUE) %>% 
      as.character()
    
    cls <- glue('../rf/output/run_2/wc 20/results/raw/future') %>% 
      dir_ls() %>% 
      grep('clust', ., value = TRUE) %>% 
      as.character()
    
    load(glue('../rData/run_2/wc 20/threshold_prob.rData'))
    map(.x = 1:length(gcm), .f = function(k) get_limitations(tp = 20, gc = gcm[k], pr = prb[k], cl = cls[k], th = thr))
    cat('Done\n')
    
  }
  
}

# Apply the functions  ----------------------------------------------------
make_limitations(run = 'run_2', tpe = 14, clm = 'cmip5')
make_limitations(run = 'run_2', tpe = 20, clm = 'cmip6')