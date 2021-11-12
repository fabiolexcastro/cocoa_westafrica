


# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Load data ---------------------------------------------------------------
run <- 'run_2'

# Functions ---------------------------------------------------------------
get_mixed <- function(tp, gc, cl, pr, un, th.pr, th.un){

  cl <- raster(cl)
  pr <- raster(pr)
  un <- raster(un)
  
  cat('Start\n')
  rs <- cl
  rs[which(un[] < th.un & pr[] > th.pr)] <- max(unique(cl[]), na.rm = T) + 1
  
  cat('To write the raster\n')
  ou <- glue('../rf/output/{run}/wc {tp}/results/process/mixed/2040_2069')
  writeRaster(x = rs, glue('{ou}/rf_5_mixed_{gc}.tif'), overwrite = TRUE)
  
  
}
make_mixed <- function(run, tpe, clm){
  
  # run <- 'run_2'
  # tpe <- '20'
  # clm <- 'cmip6'
  
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
    
    cls <- glue('../rf/output/run_2/wc 14/results/process/limitations/2040_2069') %>% 
      dir_ls() %>% 
      as.character()
    
    unc <- glue('../rf/output/run_2/wc 14/results/raw/future') %>% 
      dir_ls() %>% 
      grep('unc', ., value = TRUE) %>% 
      as.character()
    
    # Load threshold
    load(glue( '../rData/{run}/wc {tpe}/threshold_prob.rData'))
    load(glue( '../rData/{run}/wc {tpe}/threshold_unc.rData'))
    
    map(.x = 1:length(gcm), .f = function(k) get_mixed(tp = 14, gc = gcm[k], cl = cls[k], pr = prb[k], un = unc[k], th.pr = thr, th.un = thr.unc))
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
    
    unc <- glue('../rf/output/run_2/wc 20/results/raw/future') %>% 
      dir_ls() %>% 
      grep('unc', ., value = TRUE) %>% 
      as.character()
    
    load(glue( '../rData/{run}/wc {tpe}/threshold_prob.rData'))
    load(glue( '../rData/{run}/wc {tpe}/threshold_unc.rData'))
    map(.x = 1:length(gcm), .f = function(k) get_mixed(tp = 20, gc = gcm[k], cl = cls[k], pr = prb[k], un = unc[k], th.pr = thr, th.un = thr.unc))
    cat('Done\n')
    
  }
  
}

# Apply the function ------------------------------------------------------
make_mixed(run = 'run_2', tpe = 14, clm = 'cmip5')
make_mixed(run = 'run_2', tpe = 20, clm = 'cmip6')


