# Augmenting Existing Datasets:
# - Storm Characteristics
#   - Storm Eye Location
# - Site Location Characteristics
#   - Elevation
#   - Flags: Inside Bay vs On Ocean vs ?

rm(list = ls())
libs = c('sf','maps','elevatr','tigris', 'dplyr')
sapply(libs, require, character.only = TRUE)
rm(libs)

raw_path = '~/git/projgamma/datasets/slosh/slosh_*_data.csv.gz'
sto_path = '~/git/projgamma/datasets/slosh/slosh_params.csv'
zone_path = '~/git/surge/data/zone/Zone - Simplified.shp'
loc_path = '~/git/projgamma/datasets/slosh/slosh_locs.csv' # intersected with coastal zone


bbox = function(df, buffer = 0.05){
  return(c(
    xlim = c(min(df$long) - buffer, max(df$long) + buffer),
    ylim = c(min(df$lat) - buffer, max(df$lat) + buffer)
  ))
}

slosh_filtered = read.csv(gzfile('~/git/projgamma/datasets/slosh/filtered_data.csv.gz'))
names(slosh_filtered)[c(1,2)] = c('long','lat')
zone = st_read(zone_path)
zone = st_transform(zone, crs = st_crs(4269))
# Augmenting Storms with Longitude data, to calculate 'distance to eye'.
sto_parm = read.csv(sto_path)
# coastline = coastline(2022)
zonecoord = as.data.frame(st_coordinates(zone))
zonecord2 = zonecoord[
  between(zonecoord$X, -83, -66) &
  between(zonecoord$Y, 25, 50),
  ]
rightmost_approx = function(X, Y, epsilon = 0.001){
  df = data.frame(X = X, Y = Y)
  grid = seq(min(Y) - epsilon, max(Y) + epsilon, length.out = 1000)
  delta = grid[2] - grid[1]
  grid = grid - delta
  df$grp = cut(Y, grid)
  dfa = df %>% group_by(grp) %>% summarize(Y = mean(Y), X = max(X))
  return(approxfun(dfa$Y, dfa$X))
}
interpfun = rightmost_approx(zonecord2$X, zonecord2$Y)
sto_parm$long = interpfun(sto_parm$lat)

# write.csv(sto_parm, file = sto_path, row.names = FALSE)

# augmenting locations with other location-data
loc_data = read.csv(loc_path)
elevations = get_elev_point(loc_data[c('x','y','IDX')], prj = st_crs(4269))
elevati_df = data.frame(elevations)
elevati_df$elevation = elevati_df$elevation * 3.28084 # conversion to feet.
loc_data_up = merge(loc_data, elevati_df[c('IDX','elevation')], by = c('IDX'))
write.csv(loc_data_up, loc_path, row.names = FALSE)






