rm(list = ls())
libs = c('ggplot2','maps','sf', 'gridExtra', 'ggExtra')
sapply(libs, require, character.only = TRUE)

load('./data/slosh_1run.rda')
slosh = data.frame(long = coords[,1], lat = coords[,2], slosh = out) %>% na.omit()
states = map_data('state')

bbox = function(df, buffer = -0.1){
  return(coord_cartesian(
    xlim = c(min(df$long) - buffer, max(df$long) + buffer), 
    ylim = c(min(df$lat) - buffer, max(df$lat) + buffer)
    ))
}

plt = ggplot(data = states, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = 'lightgray') +
  geom_raster(data = slosh, aes(x = long, y = lat, fill = slosh)) + 
  scale_fill_viridis_c(limits = c(0, max(slosh$slosh))) + 
  geom_polygon(aes(group = group), color = 'black', fill = NA) +
  bbox(slosh) + 
  theme_minimal() + xlab(element_blank()) + ylab(element_blank()) + 
  theme(
    legend.position = c(0.92, 0.45), 
    legend.direction = 'vertical', 
    legend.title = element_blank(),
    legend.key.height = unit(3, 'lines')
    )
print(plt)
ggsave('~/git/exapg/plots/slosh1run.pdf', plt, height = 4, width = 3.5, units = 'in')

slosh_filtered = read.csv(gzfile('~/git/projgamma/datasets/slosh/filtered_data.csv.gz', ))
names(slosh_filtered)[c(1,2)] = c('long','lat')
slosh_filtered$threshold = apply(slosh_filtered[,9:4008], 1, function(x){quantile(x, 0.99)})

plt2 = ggplot(data = states, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = 'black', fill = 'lightgray') +
  geom_point(data = slosh_filtered, aes(color = threshold), size = .2) + 
  scale_color_viridis_c(limits = c(0, max(slosh$slosh))) + 
  bbox(slosh_filtered) + 
  theme_minimal() + xlab(element_blank()) + ylab(element_blank()) + 
  theme(
    legend.position = c(0.92, 0.45),
    legend.direction = 'vertical',
    legend.title = element_blank(),
    legend.key.height = unit(3, 'lines')
    )
# print(plt2)
ggsave('~/git/exapg/plots/sloshthreshold.pdf', plt2, height = 4, width = 3.5, units = 'in')

plt3 = plt2 + ggMarginal(data = slosh_filtered, p = plt2, type = 'histogram', margins = 'y')








kept_col_prop = function(q){
  return(mean(apply(slosh_filtered[,-c(1:8)], 1, function(x){quantile(x, q) > 0})))
}

kept_prop = function(q){
  sloshmat = as.matrix(slosh_filtered[,-c(1:8)])
  quantiles = apply(sloshmat, 1, function(x){quantile(x, q)})
  kept_locs = which(quantiles > 0)
  sloshmat_ = sloshmat[kept_locs,]
  quantiles_ = quantiles[kept_locs]
  prop_rows = mean(apply(sloshmat_, 2, function(x){any(x > quantiles_)}))
  prop_cols = length(quantiles_) / length(quantiles)
  return(c(cols = prop_cols, rows = prop_rows))
}

q = seq(0.8, 1, length.out = 101)
props = sapply(q, kept_prop)
dq = data.frame(q, t(props))
names(dq) = c('Q','pCol','pRow')
ggplot(dq, aes(x = pCol, y = pRow, color = Q)) + geom_line() + scale_color_viridis_c() + 
  xlab('Proportion of Locations with threshold > 0') + 
  ylab('Proportion of Storms ≮ Threshold') +
  theme_minimal()
p1 = ggplot(dq, aes(x = Q, y = pCol)) + geom_line() + 
  xlab('Percentile of Threshold') + 
  ylab('Proportion of Locations with threshold > 0') + 
  theme_minimal()
p2 = ggplot(dq, aes(x = Q, y = pRow)) + geom_line() +
  xlab('Percentile of Threshold') +
  ylab('Proportion of Storms ≮ Threshold') +
  theme_minimal()
grid.arrange(p1, p2, ncol = 2)


