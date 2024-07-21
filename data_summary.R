rm(list = ls())
libs = c('ggplot2','maps','sf', 'gridExtra', 'ggExtra','patchwork','scales')
sapply(libs, require, character.only = TRUE)

# load('./data/slosh_1run.rda')
# slosh = data.frame(long = coords[,1], lat = coords[,2], slosh = out) %>% na.omit()
# states = map_data('state')
# 
# bbox = function(df, buffer = -0.1){
#   return(coord_cartesian(
#     xlim = c(min(df$long) - buffer, max(df$long) + buffer), 
#     ylim = c(min(df$lat) - buffer, max(df$lat) + buffer)
#     ))
# }
# mainplt = ggplot(data = states, aes(x = long, y = lat)) +
#   geom_polygon(aes(group = group), fill = 'lightgray') +
#   geom_raster(
#     data = slosh, show.legend = FALSE, 
#     aes(x = long, y = lat, fill = slosh)
#     ) + 
#   scale_fill_viridis_c(limits = c(0, max(slosh$slosh))) + 
#   geom_polygon(aes(group = group), color = 'black', fill = NA) +
#   bbox(slosh) + 
#   theme_minimal() + xlab(element_blank()) + ylab(element_blank()) + 
#   theme(
#     legend.position = c(0.92, 0.45), 
#     legend.direction = 'vertical', 
#     legend.title = element_blank(),
#     legend.key.height = unit(3, 'lines')
#     )
# sideplt = ggplot(data=slosh, aes(y = slosh, fill = after_stat(y))) + 
#   geom_histogram(show.legend = FALSE, bins = 200) + 
#   scale_fill_viridis_c() +
#   scale_y_continuous(breaks = pretty_breaks(n = 8)) +
#   ylim(0, max(slosh$slosh)) +
#   labs(y = NULL, x = NULL) +
#   theme(
#     panel.background = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#   ) + 
#   scale_x_continuous(trans = 'log2')
# plt = mainplt + sideplt + plot_layout(widths = c(7, 1))


# print(plt)
# ggsave('~/git/exapg/plots/slosh1run.pdf', plt, height = 4, width = 3.75, units = 'in')

slosh_filtered = read.csv(gzfile('~/git/projgamma/datasets/slosh/filtered_data.csv.gz', ))
names(slosh_filtered)[c(1,2)] = c('long','lat')
slosh_filtered$threshold = apply(slosh_filtered[,9:4008], 1, function(x){quantile(x, 0.90)})

# mainplt2 = ggplot(data = states, aes(x = long, y = lat)) +
#   geom_polygon(aes(group = group), color = 'black', fill = 'lightgray') +
#   geom_point(
#     data = slosh_filtered, show.legend = FALSE,
#     aes(color = threshold), size = .2
#     ) + 
#   scale_color_viridis_c(limits = c(0, max(slosh$slosh))) + 
#   bbox(slosh_filtered) + 
#   theme_minimal() + xlab(element_blank()) + ylab(element_blank()) + 
#   theme(
#     legend.position = c(0.92, 0.45),
#     legend.direction = 'vertical',
#     legend.title = element_blank(),
#     legend.key.height = unit(3, 'lines')
#     )
# sideplt2 = ggplot(data=slosh_filtered, aes(y = threshold, fill = after_stat(y))) + 
#   geom_histogram(show.legend = FALSE, bins = 200) + 
#   scale_fill_viridis_c() +
#   scale_y_continuous(breaks = pretty_breaks(n = 8)) +
#   ylim(0, max(slosh$slosh)) +
#   labs(y = NULL, x = NULL) +
#   theme(
#     panel.background = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#   ) + 
#   scale_x_continuous(trans = 'log2')
# plt2 = mainplt2 + sideplt2 + plot_layout(widths = c(7, 1))
# # print(plt2)
# print(plt2)
# ggsave('~/git/exapg/plots/sloshthreshold.pdf', plt2, height = 4, width = 3.75, units = 'in')

#### Kept Columns

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

# Isolate data to those columns with threshold > 0, when threshold set at 0.9 quantile
isolate_cols_by_threshold = function(q){
  sloshmat = as.matrix(slosh_filtered[, -c(1:8, 4009)])
  quantiles = apply(sloshmat, 1, function(x){quantile(x, q)})
  kept_locs = which(quantiles > 0)
  sloshmat_ = sloshmat[kept_locs,]
  locs      = slosh_filtered[kept_locs,c(1:8)]
  return(cbind(locs, sloshmat_))
  # return(sloshmat_)
}

slosh9 = isolate_cols_by_threshold(0.9)
write.csv(
  data.frame(slosh9), 
  row.names = FALSE, 
  file = gzfile('~/git/projgamma/datasets/slosh/slosh_thr.90.csv.gz')
  )
inputs = read.csv('./data/inputs.csv')
write.csv(inputs, '~/git/projgamma/datasets/slosh/slosh_params.csv', row.names = FALSE)





