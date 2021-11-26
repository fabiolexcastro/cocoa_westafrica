

cat('Set up\n')
source(file = './00 general function.R')

cat('Load data')
run  <- 'run_4'
pnts <- read.csv('../data/tbl/run_4/03 points RF wcl 2 0.csv')
sftr <- st_as_sf(x = pnts, coords = c('X', 'Y'), crs = st_crs(4326))
prob <- raster(glue('../rf/output/run_4/wc 20/results/raw/RF_5Prob_current.asc'))
unct <- raster(glue('../rf/output/run_4/wc 20/results/raw/RF_5Unc_current.asc'))

cat('Load the threshold\n')
load(glue('../rData/run_4/wc 20/threshold_prob.rData'))
load(glue('../rData/run_4/wc 20/threshold_unc.rData'))

cat('To extract the values for each raster\n')
pnts$prob <- raster::extract(prob, pnts[,1:2])
pnts$uncr <- raster::extract(unct, pnts[,1:2])
pnts <- as_tibble(pnts)
pnts <- drop_na(pnts)

cat('Add the percentile values\n')
tprb <- as.data.frame(quantile(pnts$prob, seq(0, 1, 0.01))) %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  set_names(c('percentile', 'value_prob')) %>% 
  mutate(percentile = parse_number(percentile))

tunc <- as.data.frame(quantile(pnts$uncr, seq(0, 1, 0.01))) %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  set_names(c('percentile', 'value_unct')) %>% 
  mutate(percentile = parse_number(percentile))

tble <- inner_join(tprb, tunc, by = 'percentile')
lbls <- data.frame(x = 5, y = thr, label = round(thr, 2))

cat('To make the graph\n')
gprob <- ggplot() + 
  geom_point(data = tble, aes(x = percentile, y = value_prob)) +
  theme_ipsum_es() +
  labs(x = 'Index', y = 'Probability') + 
  ggtitle(label = 'Probability values') +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = thr), linetype = 'dashed', col = 'red') +
  geom_segment(aes(x = 0, y = thr, xend = 5, yend = thr), linetype = 'dashed', col = 'red') +
  geom_text_repel(data = lbls, aes(x = x, y = y, label = label), box.padding = unit(3, 'line'))

lbls <- data.frame(x = 10, y = thr.unc, label = round(thr.unc, 2))

gunc <- ggplot() + 
  geom_point(data = tble, aes(x = percentile, y = value_unct)) +
  theme_ipsum_es() +
  labs(x = 'Index', y = 'Uncertainty') + 
  ggtitle(label = 'Uncertainty values') +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  geom_segment(aes(x = 10, y = 0, xend = 10, yend = thr.unc), linetype = 'dashed', col = 'red') +
  geom_segment(aes(x = 0, y = thr.unc, xend = 10, yend = thr.unc), linetype = 'dashed', col = 'red') +
  geom_text_repel(data = lbls, aes(x = x, y = y, label = label), box.padding = unit(3, 'line'))

gall <- ggarrange(gprob, gunc)
ggsave(plot = gall, filename = glue('../png/graphs/{run}/prob_unc_v_20.png'), 
       units = 'in', width = 11, height = 7, dpi = 300)

1+1

