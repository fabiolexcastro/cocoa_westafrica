

# Load libraries ----------------------------------------------------------
source('00 general function.R')

# Functions ---------------------------------------------------------------
read_climate <- function(tpe){
  clmt <- grep(tpe, fles, value = TRUE) %>% 
    dir_ls(., regexp = '.tif$') %>% 
    mixedsort() %>% 
    grep('bio', ., value = TRUE) %>% 
    mixedsort() %>% 
    as.character() %>% 
    map(.x = ., .f = raster)
  extn <- extent(clmt[[34]])
  clmt <- map(.x = clmt, .f = function(k) raster::crop(k, extn))
  clmt <- raster::stack(clmt)
}

make_boxplot <- function(tbl, nme){
  
  cat('Start\n')
  
  # Tidy the table
  tbl <- gather(tbl, variable, values, -X, -Y, -class)
  vrs <- unique(tbl$variable)
  vrs <- mixedsort(vrs)
  tbl <- mutate(tbl, variable = factor(variable, levels = vrs))
  tbl <- mutate(tbl, class = factor(class, levels = 1:5))
  tbl <- drop_na(tbl)
  
  # To make the graph
  ggp <- ggplot(data = tbl, aes(x = class, y = values)) + 
    geom_boxplot() + 
    facet_wrap(.~variable, scales = 'free_y') + 
    theme_ipsum_es() +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
    labs(x = 'Cluster', y = 'Values (C/mm)')
  
  out <- glue('../png/graphs/boxplot/{run}')
  ifelse(!dir.exists(out), dir_create(out), print('Ya existe'))
  ggsave(plot = ggp, filename = glue('{out}/boxplot_clusters_{nme}.png'), units = 'in', width = 12, height = 9, dpi = 300)
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------

run <- 'run_2'

# Climate --------------------------------------------
root <- '../data/raster/climate/worldclim/2_5min'
fles <- dir_ls(root)
fles <- fles[-grep('etpv1', fles, value = F)]

# Read raster ----------------------------------------
st14 <- read_climate(tpe = 'v1_4')
st20 <- read_climate(tpe = 'v2_0')

# Read points -----------------------------------------
pn14 <- read_csv(glue('../data/tbl/{run}/03 points RF wcl 1 4.csv'))
pn20 <- read_csv(glue('../data/tbl/{run}/03 points RF wcl 2 0.csv'))

# Make the boxplot --------------------------------------------------------
make_boxplot(tbl = pn14, nme = 'wc 14')
make_boxplot(tbl = pn20, nme = 'wc 20')

