rm(list = ls())
libs = c('ggplot2', 'maps', 'sf', 'gridExtra', 'ggExtra', 
          'patchwork', 'scales', 'Cairo', 'elevatr', 'tigris')
sapply(libs, require, character.only = TRUE)
rm(libs)

bbox = function(df, buffer = -0.1){
  return(coord_cartesian(
    xlim = c(min(df$long) - buffer, max(df$long) + buffer),
    ylim = c(min(df$lat) - buffer, max(df$lat) + buffer)
    ))
}

get_elevation = function(dat){
  dat_as_sf = st_as_sf(dat[c('long','lat')], coords = c('long','lat'), crs = 4269)
  
}

slosh_filtered = read.csv(gzfile('~/git/projgamma/datasets/slosh/filtered_data.csv.gz'))
names(slosh_filtered)[c(1,2)] = c('long','lat')
slosh_filtered$threshold = apply(slosh_filtered[,9:4008], 1, function(x){quantile(x, 0.90)})
slosh_t90 = slosh_filtered[slosh_filtered$threshold > 0,]

plot_threshold = function(data, ln = FALSE, size = 0.2){
  main = ggplot(data = states, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), color = 'black', fill = 'lightgray') +
    geom_point(data = data, show.legend = FALSE, aes(color = threshold), size = .2) +
    scale_color_viridis_c(limits = c(0, max(slosh$slosh2))) +
    bbox(slosh_filtered) +
    theme_minimal() + xlab(element_blank()) + ylab(element_blank()) +
    theme(
      legend.position = c(0.92, 0.45), legend.direction = 'vertical',
      legend.title = element_blank(), legend.key.height = unit(3, 'lines')
      )
  side = ggplot(data=slosh_filtered, aes(y = threshold, fill = after_stat(y))) +
    geom_histogram(show.legend = FALSE, bins = 200) +
    scale_fill_viridis_c() +
    scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    ylim(0, max(slosh$slosh2)) +
    labs(y = NULL, x = NULL) +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
    )
  if(ln){
    side = side + scale_x_continuous(trans = 'log')
  }
  plt = main + side + plot_layout(widths = c(7,1))
  return(plt)
}

# plt_t90 = plot_threshold(slosh_t90, FALSE)
# ggsave('~/git/exapg/plots/slosh_t90_threshold.pdf', 
#        plt_t90, height = 4, width = 3.75, units = 'in')
# plt_t90_log = plot_threshold(slosh_t90, TRUE)
# ggsave('~/git/exapg/plots/slosh_t90_threshold_log.pdf', 
#        plt_t90_log, height = 4, width = 3.75, units = 'in')

#### Kept Columns
# kept_col_prop = function(q){
#   return(mean(apply(slosh_filtered[,-c(1:8)], 1, function(x){quantile(x, q) > 0})))
# }
# kept_prop = function(q){
#   sloshmat = as.matrix(slosh_filtered[,-c(1:8)])
#   quantiles = apply(sloshmat, 1, function(x){quantile(x, q)})
#   kept_locs = which(quantiles > 0)
#   sloshmat_ = sloshmat[kept_locs,]
#   quantiles_ = quantiles[kept_locs]
#   prop_rows = mean(apply(sloshmat_, 2, function(x){any(x > quantiles_)}))
#   prop_cols = length(quantiles_) / length(quantiles)
#   return(c(cols = prop_cols, rows = prop_rows))
# }
# q = seq(0.8, 1, length.out = 101)
# props = sapply(q, kept_prop)
# dq = data.frame(q, t(props))
# names(dq) = c('Q','pCol','pRow')
# 
# xq = c(-Inf, 0.9, 0.9)
# yloc = c(dq[dq$Q == 0.9,]$pCol[1], dq[dq$Q == 0.9,]$pCol[1], -Inf)
# yrow = c(dq[dq$Q == 0.9,]$pRow[1], dq[dq$Q == 0.9,]$pRow[1], -Inf)
# intlines = data.frame(xq = xq, yloc = yloc, yrow = yrow)
# 
# p1 = ggplot(dq, aes(x = Q, y = pCol)) + geom_line() +
#   geom_line(aes(x = xq, y = yloc), linetype = 2, data = intlines, color = 'darkgray') + 
#   xlab('Percentile of Threshold') +
#   ylab('Proportion of Locations with threshold > 0') +
#   theme_minimal()
# p2 = ggplot(dq, aes(x = Q, y = pRow)) + geom_line() +
#   geom_line(aes(x = xq, y = yrow), linetype = 2, data = intlines, color = 'darkgray') + 
#   xlab('Percentile of Threshold') +
#   ylab('Proportion of Storms ≮ Threshold') +
#   theme_minimal()
# pa = grid.arrange(p1, p2, ncol = 2)
# ggsave(pa, file = '~/git/exapg/plots/explore_threshold.pdf', 
#        device = cairo_pdf, width = 11, height = 5)

## Isolate data to those columns with threshold > 0, when threshold set at 0.9 quantile
isolate_cols_by_threshold = function(q){
  sloshmat = as.matrix(slosh_filtered[, -c(1:8, 4009)])
  quantiles = apply(sloshmat, 1, function(x){quantile(x, q)})
  kept_locs = which(quantiles > 0)
  sloshmat_ = sloshmat[kept_locs,]
  locs      = slosh_filtered[kept_locs,c(1:8)]
  return(cbind(locs, sloshmat_))
  # return(sloshmat_)
}

isolate_locs_by_coords = function(data, lat_low, lat_high, lon_low, lon_high){
  keep = which(
    (data$lat > lat_low) & (data$lat < lat_high) & 
      (data$lon > lon_low) & (data$lon < lon_high)
    )
  return(data[keep,])
}

slosh9 = isolate_cols_by_threshold(0.9)
write.csv(
  data.frame(slosh9),
  row.names = FALSE,
  file = gzfile('~/git/projgamma/datasets/slosh/slosh_thr.90.csv.gz')
  )
inputs = read.csv('./data/inputs.csv')
write.csv(inputs, '~/git/projgamma/datasets/slosh/slosh_params.csv', row.names = FALSE)

sloshltd = slosh_filtered[!(slosh_filtered$MTFCC %in% c('C3061','C3081')),]
sloshltd$threshold = apply(sloshltd[,-c(1:8,4009)], 1, function(x){quantile(x, 0.95)})
sloshltd = sloshltd[sloshltd_threshold > 0,]

# plt_ltd = plot_threshold(sloshltd, FALSE)
# ggsave('~/git/exapg/plots/slosh_ltd_threshold.pdf',
#        plt_t90, height = 4, width = 3.75, units = 'in')
# plt_ltd_log = plot_threshold(sloshltd, TRUE)
# ggsave('~/git/exapg/plots/slosh_ltd_threshold_log.pdf',
#        plt_t90_log, height = 4, width = 3.75, units = 'in')

sloshapt = slosh_filtered[slosh_filtered$MTFCC %in% c('K2451'),]
sloshapt$threshold = apply(sloshapt[,-c(1:8,4009)], 1, function(x){quantile(x, 0.95)})
sloshapt = sloshapt[sloshapt$threshold > 0,]

codes = data.frame(t(t(table(slosh_filtered$MTFCC))))
codes$MTFCC = codes$Var1
codes$meanings = c(
  'Peak','Cul-de-sac','Traffic Circle','Gate','Tower','Locality Point',
  'Crew-of-vessel Location', 'Hospital','Prison','Military','Govt Center',
  'National Park', 'State Park','Other Park', 'Fire Department', 'Airport',
  'Train Station','Marine Terminal','Helicopter Pad','School','Golf Course',
  'Cemetary','Place Of Worship'
  )
codes = codes[c('MTFCC','meanings','Freq')]
sum(codes$Freq[codes$MTFCC %in% c('K1231','K2110','K2193','K2451')])

common_codes = c('C3061','C3081')
emergency_codes = c('K1231','K2110','K2193','K2165')
transport_codes = c('K2451', 'K2452','K2454')
locale_codes = c('C3081')

slosh_emg = slosh_filtered[slosh_filtered$MTFCC %in% emergency_codes,]
slosh_emg$threshold = apply(slosh_emg[,-c(1:8,4009)], 1, function(x){quantile(x, 0.95)})
slosh_emg = slosh_emg[slosh_emg$threshold > 0,]

slosh_xpt = slosh_filtered[slosh_filtered$MTFCC %in% transport_codes,]
slosh_xpt$threshold = apply(slosh_xpt[,-c(1:8,4009)], 1, function(x){quantile(x, 0.90)})
slosh_xpt = slosh_xpt[slosh_xpt$threshold > 0,]

# slosh locales
slosh_loc = slosh_filtered[slosh_filtered$MTFCC %in% locale_codes,]
slosh_loc$threshold = apply(slosh_loc[, -c(1:8, 4009)], 1, function(x){quantile(x, 0.95)})
slosh_loc = slosh_loc[slosh_loc$threshold > 0,]

# slosh bigger delaware
slosh_del1 = isolate_locs_by_coords(slosh_filtered, 38.65, 40.07, -75.73, -74.81)
slosh_del1$threshold = apply(slosh_del1[,-c(1:8, 4009)], 1, function(x){quantile(x, 0.90)})
slosh_dbg = slosh_del1[which(slosh_del1$threshold > 0),]

slosh_del2 = slosh_del1[which(!(slosh_del1$MTFCC %in% c('C3061','C3081'))),]


# slosh delaware
slosh_del = isolate_locs_by_coords(slosh_filtered, 38.65, 40.07, -75.73, -74.81)
slosh_del = slosh_del[!(slosh_del$MTFCC %in% common_codes),]
slosh_del$threshold =  apply(slosh_del[, -c(1:8, 4009)], 1, function(x){quantile(x, 0.90)})
slosh_del = slosh_del[slosh_del$threshold > 0,]

# slosh new york
slosh_nyc = isolate_locs_by_coords(slosh_filtered, 40.25, 40.87, -74.45, -73.63)
slosh_nyc = slosh_nyc[!(slosh_nyc$MTFCC %in% common_codes),]
slosh_nyc$threshold = apply(slosh_nyc[, -c(1:8, 4009)], 1, function(x){quantile(x, 0.90)})
slosh_nyc = slosh_nyc[slosh_nyc$threshold > 0, ]

plt_apt = plot_threshold(sloshapt, FALSE, 0.4)
ggsave('~/git/exapg/plots/slosh_apt_threshold.pdf',
       plt_apt, height = 4, width = 3.75, units = 'in')
plt_apt_log = plot_threshold(sloshapt, TRUE, 0.4)
ggsave('~/git/exapg/plots/slosh_apt_threshold_log.pdf',
       plt_apt_log, height = 4, width = 3.75, units = 'in')

plt_emg = plot_threshold(slosh_emg, FALSE, 0.4)
ggsave('~/git/exapg/plots/slosh_emg_threshold.pdf',
       plt_emg, height = 4, width = 3.75, units = 'in')
plt_emg_log = plot_threshold(slosh_emg, TRUE, 0.4)
ggsave('~/git/exapg/plots/slosh_emg_threshold_log.pdf',
       plt_emg_log, height = 4, width = 3.75, units = 'in')

plt_xpt = plot_threshold(slosh_xpt, FALSE, 0.4)
ggsave('~/git/exapg/plots/slosh_emg_threshold.pdf',
       plt_emg, height = 4, width = 3.75, units = 'in')
plt_xpt_log = plot_threshold(slosh_xpt, TRUE, 0.4)
ggsave('~/git/exapg/plots/slosh_emg_threshold_log.pdf',
       plt_xpt_log, height = 4, width = 3.75, units = 'in')


write_to_disk = function(data, dsname){
  file_path = paste0('~/git/projgamma/datasets/slosh/slosh_', dsname, '_data.csv.gz')
  write.csv(data[,-c(4009)], file = gzfile(file_path), row.names = FALSE)
}
write_to_disk(slosh_emg, 'emg')
write_to_disk(slosh_xpt, 'xpt')
write_to_disk(sloshapt, 'apt')
write_to_disk(sloshltd, 'ltd')
write_to_disk(slosh_t90, 't90')
write_to_disk(slosh_loc, 'loc')
write_to_disk(slosh_nyc, 'nyc')
write_to_disk(slosh_del, 'del')
write_to_disk(slosh_dbg, 'dbg')

## Histogram of Theta parameters that survived thresholding
theta = read.csv('~/git/projgamma/datasets/slosh/slosh_params.csv')
theta$theta[theta$theta < 100] = theta$theta[theta$theta < 100] + 360

# # apt_mat = t(as.matrix(sloshapt[,-c(1:8,4009)]))
# # library(GGally)
# # ggpairs(data.frame(apt_mat[,1:20]))

thresholded = read.csv('~/git/projgamma/test/threshold_inclusion.csv')
names(thresholded) = c('ltd','apt','t90')

theta_t90 = theta[which(as.logical(thresholded$t90)),]
p_slr = ggplot(theta_t90, aes(x = slr)) + geom_histogram(bins = 50) + theme_bw() + xlab('Sea Level Rise') + ylab(element_blank())
p_the = ggplot(theta_t90, aes(x = theta)) + geom_histogram(bins = 50) + theme_bw() + xlab('Approach Angle') + ylab(element_blank())
p_v   = ggplot(theta_t90, aes(x = v)) + geom_histogram(bins = 50) + theme_bw() + xlab('Approach Speed') + ylab(element_blank())
p_pmi = ggplot(theta_t90, aes(x = pmin)) + geom_histogram(bins = 50) + theme_bw() + xlab('Minimum Pressure') + ylab(element_blank())
p_lat = ggplot(theta_t90, aes(x = lat)) + geom_histogram(bins = 50) + theme_bw() + xlab('Latitude') + ylab(element_blank())
pt = grid.arrange(p_slr, p_the, p_v, p_pmi, p_lat, ncol = 5)
ggsave(pt, file = '~/git/exapg/plots/threshold_histogram.pdf', 
       device = cairo_pdf, width = 10, height = 3, units = 'in')

# codes = data.frame(t(t(table(slosh_filtered$MTFCC))))
# codes$MTFCC = codes$Var1
# codes$meanings = c(
#   'Peak','Cul-de-sac','Traffic Circle','Gate','Tower','Locality Point',
#   'Crew-of-vessel Location', 'Hospital','Prison','Military','Govt Center',
#   'National Park', 'State Park','Other Park', 'Fire Department', 'Airport',
#   'Train Station','Marine Terminal','Helicopter Pad','School','Golf Course',
#   'Cemetary','Place Of Worship'
#   )
# codes = codes[c('MTFCC','meanings','Freq')]
# sum(codes$Freq[codes$MTFCC %in% c('K1231','K2110','K2193','K2451')])

# plotting bounds of landfall
upper2 = as.numeric(theta[c('long','lat')][which.max(theta$lat),])
lower2 = as.numeric(theta[c('long','lat')][which.min(theta$lat),])

theta$radians = theta$theta / 180
r = 0.3
upper1 = c(r * sin(max(theta$radians)), r * cos(max(theta$radians))) + upper2
upper3 = c(r * sin(min(theta$radians)), r * cos(min(theta$radians))) + upper2
lower1 = c(-r * sin(max(theta$radians)), -r * cos(max(theta$radians))) + lower2
lower3 = c(-r * sin(min(theta$radians)), -r * cos(min(theta$radians))) + lower2
bounds = data.frame(rbind(upper1,upper2,upper3,lower1,lower2,lower3))
names(bounds) = c('long','lat')
bounds$spec = c(1, 1, 1, 2, 2, 2)
# plot(sto_parm$long, sto_parm$lat, xlim = c(-77,-73.5),ylim = c(37,40))
# lines(bounds$long, bounds$lat, col = 'red')
bounds = bounds[c(1,2,3,2,4,5,6,5),]
bounds$spec = c(1,1,2,2,3,3,4,4)


load('./data/slosh_1run.rda')
slosh = data.frame(long = coords[,1], lat = coords[,2], slosh = out) %>% na.omit()
slosh$slosh2 = slosh$slosh
slosh$slosh2[slosh$slosh2 > 9] = 9
states = map_data('state')
mainplt = ggplot(data = states, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = 'lightgray') +
  geom_raster(
    data = slosh, show.legend = FALSE,
    aes(x = long, y = lat, fill = 'slosh2')
    ) +
  scale_fill_viridis_c(limits = c(0, max(slosh$slosh2))) +
  geom_polygon(aes(group = group), color = 'black', fill = NA) +
  bbox(slosh) +
  theme_minimal() + xlab(element_blank()) + ylab(element_blank()) +
  theme(
    legend.position = c(0.92, 0.45),
    legend.direction = 'vertical',
    legend.title = element_blank(),
    legend.key.height = unit(3, 'lines')
    )
sideplt = ggplot(data=slosh, aes(y = slosh, fill = after_stat(y))) +
  geom_histogram(show.legend = FALSE, bins = 200) +
  scale_fill_viridis_c() +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  ylim(0, max(slosh$slosh2)) +
  labs(y = NULL, x = NULL) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
  )
plt = mainplt + sideplt + plot_layout(widths = c(7, 1))
print(plt)
ggsave('~/git/exapg/plots/slosh1run.pdf', plt, height = 4, width = 3.75, units = 'in')
sidepltlog = sideplt + scale_x_continuous(trans = 'log')
pltlog = mainplt + sidepltlog + plot_layout(widths = c(7, 1))
print(pltlog)
ggsave('~/git/exapg/plots/slosh1run_loghist.pdf', pltlog, height = 4, width = 3.75, units = 'in')

library(ggExtra)
library(Cairo)
library(ggmap)
register_stadiamaps('bb37e42d-d438-416f-8003-d09c1e6a1347')

# delaware <- c(left = min(locdata$long) - 0.2, bottom = min(locdata$lat) - 0.1, 
#               right = max(locdata$long) + 0.2, top = max(locdata$lat) + 0.1)
basemap <- get_stadiamap(
  c(left = min(slosh$long), bottom = min(slosh$lat), 
    right = max(slosh$long), top = max(slosh$lat)), 
  zoom = 8, 
  maptype = 'stamen_toner_lite'
  )
map = ggmap(basemap) + 
  geom_line(data = bounds, mapping = aes(x = long, y = lat, group = spec), color = 'red', linewidth = 1.) +
  theme_minimal() + theme(axis.title = element_blank())
print(map)
ggsave('~/git/exapg/plots/slosh_bounds.pdf', map, height = 4, width = 3.75, units = 'in')

