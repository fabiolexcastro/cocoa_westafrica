
# Load libraries ----------------------------------------------------------
source('00 general function.R')
source('10 02 random forest functions.R')

# Function make random forest ---------------------------------------------
make_rf <- function(all, ssz, run, tpe, mdl, stk){
  
  # all <- al14
  # ssz <- ss14
  # run <- 'run_1'
  # tpe <- 'wc 14'
  # mdl <- md14
  # stk <- st14
  
  all = al20; ssz = ss20; run = 'run_1'; tpe = 'wc 20'; mdl = md20; stk = st20
  
  cat('Start ...!', '\n')
  library(pROC)
  
  rflist <- vector('list', 50) 
  auc <- vector('list', 50)
  
  for(repe in 1:50){ # 50 bosques
    
    print(repe)
    pressample <- list()
    NumberOfClusters <- 5
    no.absenceclasses <- 2
    
    for(i in 1:(NumberOfClusters+no.absenceclasses)){
      
      if(any(i==c(1:no.absenceclasses))) { 
        
        print('Absence')
        # rows <- sample(rownames(all[all$pb==i,]), size = ssz*NumberOfClusters/2/no.absenceclasses)
        rows <- sample_n(tbl = all[all$pb == i,], size = ssz*NumberOfClusters/2/no.absenceclasses)
        
      } else {
        print('Presence')
        # rows <- sample(rownames(all[all$pb==i,]), size = ssz)
        rows <- sample_n(tbl = all[all$pb == i,], size = ssz)
        
      }
      
      # pressample[[i]] <- all[rows,] 
      pressample[[i]] <- rows 
      
    }
    
    species <- na.omit(do.call(rbind, pressample)) 
    head(species)
    unique(species$pb)
    Samplesplit <- sample(rownames(species)) 
    
    envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
    envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
    
    rfmodel <- randomForest(mdl, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
    
    dout <- glue('../rf/output/{run}/{tpe}/models')
    ifelse(!file.exists(dout), dir_create(dout), print('Directorio existe'))
    save(rfmodel, file = paste(dout, '/' , NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
    rflist[[repe]] <- rfmodel
    
    # AUC 
    predicted <- as.numeric(predict(rfmodel, envtest))
    observed <- as.vector(envtest[,'pb'])
    observed <- as.numeric(observed$pb)
    auc[[repe]] <- auc(observed, predicted) 
    rm(rfmodel)
    
    cat(auc[[repe]] ,'\n')
    
  }
  
  auc <- unlist(auc)
  rff <- do.call(randomForest::combine, rflist)
  importance <- as.data.frame(rff$importance)
  
  dout <- glue('../rData/{run}/{tpe}/')
  ifelse(!file.exists(dout), dir_create(dout), print('Directorio existe'))
  
  save(rflist, file = paste(dout, '/rflist_', NumberOfClusters, '.rdata', sep = ''))
  save(importance, file = paste0(dout, '/importanceRF.rData'))
  save(auc, file = paste0(dout, '/aucRF_dist.rData'))
  save(rff, file = paste0(dout, '/rff_dist.rData'))
  
  # Predict model
  climatevalues <- data.frame(getValues(stk))
  rasterProbs <- predict(rff, climatevalues, type = 'prob')
  rasterProbs_na <- na.omit(rasterProbs)
  sum_rasterProbs_na <- apply(rasterProbs_na, 1, sum)
  
  rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
  uncertainty <- apply(rasterProbs, 1, max)  
  
  rasterRFprob <- stk[[1]]
  values(rasterRFprob) <- rasterRF 
  
  rasterRFuncertainty <- stk[[1]]
  values(rasterRFuncertainty) <- uncertainty 
  
  rasterRF <- max.col(rasterProbs, 'first')
  rasterRFclass <- stk[[1]]
  values(rasterRFclass) <- rasterRF
  
  dout
  dout <- glue('../rf/output/{run}/{tpe}/results/raw')
  ifelse(!file.exists(dout), dir.create(dout, recursive = TRUE), print('Directorio existe'))
  
  plot(rasterRFclass)
  writeRaster(rasterRFclass, paste0(dout, '/RF_5Clust_current.asc'), format = 'ascii', overwrite = T)
  writeRaster(rasterRFprob, paste0(dout, '/RF_5Prob_current.asc'), format = 'ascii', overwrite = T)
  writeRaster(rasterRFuncertainty, paste0(dout, '/RF_5Unc_current.asc'), format = 'ascii', overwrite = T)
  
  cat('--------------------Done---------------------------!\n')
  return(rasterRFclass)
  
}


# Load data ---------------------------------------------------------------
run <- 'run_1'

# Climate 
st14 <- list.files('../data/raster/climate/worldclim/2_5min/v1_4', full.names = TRUE) %>% 
  grep('bio', ., value = TRUE) %>% 
  mixedsort()
st20 <- list.files('../data/raster/climate/worldclim/2_5min/v2_0', full.names = TRUE) %>% 
  grep('bio', ., value = TRUE) %>% 
  mixedsort()

mk14 <- raster('../data/raster/climate/worldclim/2_5min/v1_4/bio_1.tif') * 0 + 1
mk20 <- raster('../data/raster/climate/worldclim/2_5min/v2_0/bio_1.tif') * 0 + 1
vr14 <- readRDS(file = glue('../data/rds/{run}/cd14.rds'))
vr20 <- readRDS(file = glue('../data/rds/{run}/cd20.rds'))

pn14 <- read_csv('../data/tbl/run_1/03 points RF wcl 1 4.csv')
pn20 <- read_csv('../data/tbl/run_1/03 points RF wcl 2 0.csv')

vr14 <- colnames(vr14[,c(1:(ncol(vr14)))-1])
vr14 <- vr14[2:length(vr14)]

vr20 <- colnames(vr20[,c(1:(ncol(vr20)))-1])
vr20 <- vr20[2:length(vr20)]

# Read climate
st14 <- stack(grep(paste0(paste0(vr14, '.tif'), collapse = '|'), st14, value = TRUE))
st20 <- stack(grep(paste0(paste0(vr20, '.tif'), collapse = '|'), st20, value = TRUE))

# Background --------------------------------------------------------------
bc14 <- read_csv('//catalogue/workspace-cluster9/COCOA_SOILS/project_v1/1.Data/tbl/points/presences/run_6/bckgrn_10km_vif.csv')[,1:2]
bc20 <- bc14

# Sampling
bc14 <- sample_n(tbl = bc14, size = nrow(pn14), replace = F)
bc20 <- sample_n(tbl = bc20, size = nrow(pn20), replace = F)

# Extract values for the background
bc14 <- as_tibble(cbind(bc14, raster::extract(st14, bc14)))
bc20 <- as_tibble(cbind(bc20, raster::extract(st20, bc20)))

nrow(pn14)
nrow(pn20)
nrow(bc14)
nrow(bc20)

# Cluster analysis to pseudo-absences -------------------------------------

# Worldclim 1.4 and worldclim 2.0
bc14_distRF <- RFdist(as.data.frame(bc14)[,3:ncol(bc14)], mtry1 = 8, 500, 50, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)# mtry1 = 4 raw  # es la cantidad de variables a utilizar en cada no
bc20_distRF <- RFdist(as.data.frame(bc20)[,3:ncol(bc20)], mtry1 = 8, 500, 50, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)# mtry1 = 4 raw  # es la cantidad de variables a utilizar en cada no
no.absencesclasses <- 2

lb14 <- pamNew(bc14_distRF$cl1, no.absencesclasses)
lb20 <- pamNew(bc20_distRF$cl1, no.absencesclasses)

cld1 <- cbind(pb = as.factor(lb14), bc14[,3:ncol(bc14)])
cld2 <- cbind(pb = as.factor(lb20), bc20[,3:ncol(bc20)])

pw14 <- pn14[,3:ncol(pn14)] %>% mutate(pb = class + 2) %>% dplyr::select(-class) %>% dplyr::select(pb, everything())
al14 <- rbind(pw14, cld1)

pw20 <- pn20[,3:ncol(pn20)] %>% mutate(pb = class + 2) %>% dplyr::select(-class) %>% dplyr::select(pb, everything())
al20 <- rbind(pw20, cld2)

write.csv(al14, '../data/tbl/run_1/04 all cls 14.csv', row.names = FALSE)
write.csv(al20, '../data/tbl/run_1/04 all cls 20.csv', row.names = FALSE)

# To make the random forest analysis --------------------------------------
md14 <- as.formula(paste('factor(pb) ~', paste(paste(vr14), collapse = '+', sep =' ')))
md20 <- as.formula(paste('factor(pb) ~', paste(paste(vr20), collapse = '+', sep =' ')))

ss14 <- round(min(summary(as.factor(pw14$pb))) / 2, 0) 
ss20 <- round(min(summary(as.factor(pw20$pb))) / 2, 0) 

# Make random forest model ------------------------------------------------
rf14 <- make_rf(all = al14, ssz = ss14, run = 'run_1', tpe = 'wc 14', mdl = md14, stk = st14)
rf20 <- make_rf(all = al20, ssz = ss20, run = 'run_1', tpe = 'wc 20', mdl = md20, stk = st20)


