
# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Load data ---------------------------------------------------------------
run <- 'run_2'

# load(glue('../rData/{run}/wc 20/rflist_5.rData'))
clst <- 5
root <- '../data/raster/climate'

make_predict <- function(tpe, clm){
  
  clm <- 'cmip5'
  tpe <- '14'
  
  # Model
  load(glue('../rData/{run}/wc {tpe}/rflist_5.rdata'))
  rff <- do.call(randomForest::combine, rflist)
  
  fls <- glue('{root}/{clm}/rcp60/2040_2069') %>% 
    dir_ls(regexp = '.tif$') %>% 
    grep('bio', ., value = TRUE) %>% 
    as.character() %>% 
    mixedsort()
  
  gcm <- basename(fls) %>% 
    grep('_1.tif', ., value = TRUE) %>% 
    str_sub(., start = 1, end = ) %>% 
    gsub('_bio_1.tif', '', .)
  
  # gcm <- basename(fls) %>% 
  #   grep('_1.tif', ., value = TRUE) %>% 
  #   gsub('bio_', '', .) %>% 
  #   gsub('_1.tif', '', .) %>% 
  #   str_split(., pattern = '_') %>% 
  #   sapply(X = ., FUN = function(k)k[1]) 
  
  vrs <- rownames(rff$importance)
  
  map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    stk <- grep(glue('{gcm[k]}_b'), fls, value = TRUE) 
    # stk <- grep(glue('{gcm[k]}'), fls, value = TRUE) 
    stk <- grep(paste0(paste0('_', parse_number(vrs), '.tif'), collapse = '|'), stk, value = TRUE)
    stk <- raster::stack(stk)
    names(stk) <- vrs
    vls <- data.frame(getValues(stk))
    rpr <- predict(rff, vls, type = 'prob', progress = 'text')
    rrf <- rowSums(rpr[,c(3:(5+2))])
    unc <- apply(rpr, 1, max)
    
    rst.prb <- stk[[1]]
    values(rst.prb) <- rrf
    
    rst.unc <- stk[[1]]
    values(rst.unc) <- unc
    
    rfr <- max.col(rpr, 'first')  
    rst.rfr <- stk[[1]]
    values(rst.rfr) <- rfr
    
    dir <- glue('../rf/output/{run}/wc {tpe}/results/raw/future')
    cat('To write the raster\n')
    writeRaster(rst.rfr, glue('{dir}/rfr_clust_{gcm[k]}.tif'), overwrite = TRUE)
    writeRaster(rst.prb, glue('{dir}/rfr_probl_{gcm[k]}.tif'), overwrite = TRUE)
    writeRaster(rst.unc, glue('{dir}/rfr_uncrt_{gcm[k]}.tif'), overwrite = TRUE)
    cat('Done!\n')
    
  })
  
  
}