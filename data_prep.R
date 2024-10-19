rm(list = ls())
libs = c('ggplot2', 'maps', 'sf', 'gridExtra', 'ggExtra', 'ggmap',
          'patchwork', 'scales', 'Cairo', 'elevatr', 'tigris','RColorBrewer')
sapply(libs, require, character.only = TRUE)
register_stadiamaps('bb37e42d-d438-416f-8003-d09c1e6a1347')
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
# write.csv(
#   data.frame(slosh9),
#   row.names = FALSE,
#   file = gzfile('~/git/projgamma/datasets/slosh/slosh_thr.90.csv.gz')
#   )
inputs = read.csv('./data/inputs.csv')
# write.csv(inputs, '~/git/projgamma/datasets/slosh/slosh_params.csv', row.names = FALSE)

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

# slosh delaware
slosh_del = isolate_locs_by_coords(slosh_t90, 38.65, 40.07, -75.73, -74.81)
# slosh restricted
slosh_res = slosh_del[which(!(slosh_del$MTFCC %in% common_codes)),]
# slosh critical
crit_idx = c(12614559,11110985,11003662,10777952,9887824,
             8061889,8568290,6982847,
             6803928,6645357,12507042,10353728)
slosh_crt = slosh_res[which(slosh_res$IDX %in% crit_idx),]
# Preparing Data Plot
slosh_mapdata = slosh_del
slosh_mapdata$slice = 'Delaware'
slosh_mapdata$slice[which(slosh_mapdata$IDX %in% slosh_res$IDX)] = 'Restricted'
slosh_mapdata$slice[which(slosh_mapdata$IDX %in% slosh_crt$IDX)] = 'Critical'
slosh_mapdata$Slice = factor(slosh_mapdata$slice, levels = c('Critical','Restricted','Delaware'))
# slosh_mapdata$Slice = factor(slosh_mapdata$slice, levels = c('Delaware','Restricted','Critical'))
basemap <- get_stadiamap(
  c(left = min(slosh_mapdata$long) - 0.2, bottom = min(slosh_mapdata$lat) - 0.2, 
    right = max(slosh_mapdata$long) + 0.2, top = max(slosh_mapdata$lat) + 0.2), 
  zoom = 9, 
  maptype = 'stamen_toner_lite'
  )
map = ggmap(basemap) + 
  geom_point(
    data = slosh_mapdata[which(slosh_mapdata$slice == 'Delaware'),], 
    mapping = aes(x = long, y = lat), color = '#4daf4a', size = 1
    ) + 
  geom_point(
    data = slosh_mapdata[which(slosh_mapdata$slice == 'Restricted'),],
    mapping = aes(x = long, y = lat), color = '#377eb8', size = 2.2
    ) + 
  geom_point(
    data = slosh_mapdata[which(slosh_mapdata$slice == 'Critical'),],
    mapping = aes(x = long, y = lat), color = '#e41a1c', size = 3
    ) + 
  theme_minimal() + 
  theme(axis.title = element_blank())
print(map)
ggsave(
  '~/git/exapg/plots/delaware_bay_locations.pdf', 
  map, height = 4, width = 3, units = 'in'
  )

# write.csv(
#   slosh_t90, row.names = FALSE,
#   file = gzfile('~/git/projgamma/datasets/slosh/slosh_t90_data.csv.gz'),
#   )
# write.csv(
#   slosh_del, row.names = FALSE,
#   file = gzfile('~/git/projgamma/datasets/slosh/slosh_dby_data.csv.gz'),
#   )
# write.csv(
#   slosh_res, row.names = FALSE,
#   file = gzfile('~/git/projgamma/datasets/slosh/slosh_res_data.csv.gz'),
#   )
write.csv( # only new one...
  slosh_crt, row.names = FALSE,
  file = gzfile('~/git/projgamma/datasets/slosh/slosh_crt_data.csv.gz'),
  )
