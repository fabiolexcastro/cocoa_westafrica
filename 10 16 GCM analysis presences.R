

cat('Set up\n')
source(file = './00 general function.R')

cat('Load data')
run  <- 'run_4'

# Points ------------------------------------------------------------------
pnts <- read.csv('../data/tbl/run_4/03 points RF wcl 1 4.csv')
pnts <- as_tibble(pnts)


# -------------------------------------------------------------------------
# CMIP5 -------------------------------------------------------------------
# -------------------------------------------------------------------------
fles <- '../data/raster/climate/cmip5/rcp60/2040_2069' %>% 
  dir_ls() %>% 
  grep(paste0(c('bio_1.tif', 'bio_12.tif'), collapse = '|'), ., value = TRUE) %>% 
  as.character()

gcms <- basename(fles) %>% grep('bio_1.tif', ., value = TRUE) %>% gsub('_bio_1.tif', '', .)
vls5 <- map(.x = 1:length(gcms), .f = function(k){
  grep(paste0(gcms[k], '_bio'), fles, value = TRUE) %>% 
    raster::stack() %>% 
    raster::extract(., pnts[,1:2]) %>% 
    apply(., 2, mean) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('var', 'value')) %>% 
    mutate(gcm = gcms[k], variable = c('bio_1', 'bio_12')) %>% 
    dplyr::select(variable, gcm, value)
}) %>% bind_rows()
vls5

vls5 <- vls5 %>% spread(variable, value) %>% mutate(bio_1 = round(bio_1 / 10, 2), bio_12 = round(bio_12, 0))

# Current 
crn4 <- dir_ls('../data/raster/climate/worldclim/2_5min/v1_4') %>% 
  grep(paste0(c('bio_1.tif$', 'bio_12.tif$'), collapse = '|'), ., value = TRUE) %>% 
  as.character() %>% 
  raster::stack() %>% 
  raster::extract(., pnts[,1:2])%>% 
  apply(., 2, mean) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('var', 'value')) %>% 
  spread(var, value) %>% 
  setNames(c('bio_1_crn', 'bio_12_crn')) %>% 
  mutate(bio_1_crn = round(bio_1_crn / 10, 2), bio_12_crn = round(bio_12_crn, 0))

# Join both ---------------------------------------------------------------
vls5 <- vls5 %>% mutate(bio_1_crn = pull(crn4, 1), bio_12_crn = pull(crn4, 2))
vls5 <- as_tibble(vls5)
vls5 <- vls5 %>% mutate(dfr_bio_1 = bio_1 - bio_1_crn, dfr_bio_12 = bio_12 - bio_12_crn)

ensm <- data.frame(current_x = mean(vls5$dfr_bio_1), current_y = mean(vls5$dfr_bio_1))

# Make the graph ----------------------------------------------------------

grph <- ggplot(data = vls5, aes(x = dfr_bio_1, y = dfr_bio_12)) + 
  geom_point() + 
  geom_text_repel(aes(label = gcm)) +
  geom_point(data = ensm, aes(x = current_x, y = current_y), col = 'red') + 
  theme_ipsum_es() + 
  labs(x = 'Temperature (°C)', y = 'Precipitation (mm)')
ggsave(plot = grph, filename = '../png/graphs/unc_cm4.png', units = 'in', width = 9, height = 7, dpi = 300)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Points ------------------------------------------------------------------
pnts <- read.csv('../data/tbl/run_4/03 points RF wcl 2 0.csv')
pnts <- as_tibble(pnts)

# -------------------------------------------------------------------------
# CMIP6 -------------------------------------------------------------------
# -------------------------------------------------------------------------
fles <- '../data/raster/climate/cmip6/rcp60/2040_2069' %>% 
  dir_ls() %>% 
  grep('bio', ., value = TRUE) %>% 
  grep(paste0(c('_1.tif', '_12.tif'), collapse = '|'), ., value = TRUE) %>% 
  as.character()

gcms <- basename(fles) %>% grep('_1.tif', ., value = TRUE) %>% gsub('_1.tif', '', .)
gcms <- gsub('bio_', '', gcms) %>% gsub('_spp370_2041-2060', '', .)
vls6 <- map(.x = 1:length(gcms), .f = function(k){
  grep(paste0(gcms[k]), fles, value = TRUE) %>% 
    raster::stack() %>% 
    raster::extract(., pnts[,1:2]) %>% 
    apply(., 2, mean) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('var', 'value')) %>% 
    mutate(gcm = gcms[k], variable = c('bio_1', 'bio_12')) %>% 
    dplyr::select(variable, gcm, value)
}) %>% bind_rows()
vls6

vls6 <- vls6 %>% spread(variable, value) %>% mutate(bio_1 = round(bio_1, 2), bio_12 = round(bio_12, 0))

# Current 
crn6 <- dir_ls('../data/raster/climate/worldclim/2_5min/v2_0') %>% 
  grep(paste0(c('bio_1.tif$', 'bio_12.tif$'), collapse = '|'), ., value = TRUE) %>% 
  as.character() %>% 
  raster::stack() %>% 
  raster::extract(., pnts[,1:2])%>% 
  apply(., 2, mean) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('var', 'value')) %>% 
  spread(var, value) %>% 
  setNames(c('bio_1_crn', 'bio_12_crn')) %>% 
  mutate(bio_1_crn = round(bio_1_crn, 2), bio_12_crn = round(bio_12_crn, 0))

# Join both ---------------------------------------------------------------
vls6 <- vls6 %>% mutate(bio_1_crn = pull(crn6, 1), bio_12_crn = pull(crn6, 2))
vls6 <- as_tibble(vls6)
vls6 <- vls6 %>% mutate(dfr_bio_1 = bio_1 - bio_1_crn, dfr_bio_12 = bio_12 - bio_12_crn)

ensm <- data.frame(current_x = mean(vls6$dfr_bio_1), current_y = mean(vls6$dfr_bio_1))

# Make the graph ----------------------------------------------------------

grph <- ggplot(data = vls6, aes(x = dfr_bio_1, y = dfr_bio_12)) + 
  geom_point() + 
  geom_text_repel(aes(label = gcm)) +
  geom_point(data = ensm, aes(x = current_x, y = current_y), col = 'red') + 
  theme_ipsum_es() + 
  labs(x = 'Temperature (°C)', y = 'Precipiation (mm)')
ggsave(plot = grph, filename = '../png/graphs/unc_cm6.png', units = 'in', width = 9, height = 7, dpi = 300)


