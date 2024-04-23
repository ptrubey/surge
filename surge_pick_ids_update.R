rm(list = ls())
libs = c('snowfall')
load('./data/slosh_1run.rda')
source('surge_fns.R')
rm(out)
resolution = 100; eps  = 0.0001

load('~/Downloads/data/res_lm.rda')

# Break Coordinates into 100x100 grid
coords = data.frame(coords)
coords$IDX = 1:nrow(coords)
xseq = seq(min(coords$x) - eps, max(coords$x) + eps, length.out = resolution)
yseq = seq(min(coords$y) - eps, max(coords$y) - eps, length.out = resolution)
coords$GRP = interaction(
  cut(coords$x, breaks = xseq),
  cut(coords$y, breaks = yseq)
  )
coordl = split(coords, coords$GRP)
# Load Landmarks file
landmarks = st_read(dsn = './data/landmarks', layer = 'landmarks')

# pts = coords[
#   (coords$x >= -76.10277777777777 & coords$x <= -75.95805555555556) &
#   (coords$y >= 36.76305555555555 & coords$y <=  36.94277777777778),
# ]

interesting_points = function(point_set){
  # Assume point_set is data.frame with (x, y, IDX, GRP)
  # Intersect with landmarks data set
  subset_points = intersect_points_with_landmarks(point_set, landmarks)
  # Extract XY locations again
  locs = as.data.frame(st_coordinates(subset_points))
  subset_points$x = locs$X
  subset_points$y = locs$Y
  # Return data frame (not sf object) with IDX, MTFCC
  out = as.data.frame(subset_points)
  return(out[c('x','y','IDX','STATEFP','POINTID','FULLNAME','MTFCC')])
}

sfInit(parallel = TRUE, cpus = 32)
sfSource('./surge_fns.R')
sfExport('landmarks')
resultl = sfLapply(coordl, interesting_points)
sfStop()
results = do.call(rbind, resultl)
save(results, './data/keep_coords.rda')

# getting data back from Devin
rm(list = ls())
keep_coords = read.csv('./data/keep_coords.csv')
load('./data/res_lm.rda')
res_lm[is.na(res_lm)] = 0.
prob_above_zero = apply(res_lm, 2, function(x){mean(x > 0)})
keep = which(prob_above_zero > 0.01)
# keep = which(apply(res_lm, 2, function(x){any(x > 0)}))

categorize = function(mtfcc){
  if (mtfcc %in% c('C3022','C3061','C3062','C3066')){
    # C3022 - Summit/Peak
    # C3061, C3062, C3066 : Road Features (cul-de-sacs, etc.)
    return('Land')
  } else if (mtfcc %in% c('C3081')){
    return('Locality')
  } else if (mtfcc %in% c('C3071','K1231', 'K1237','K2165','K2193','K2110','K1225','K2543')){
    # C3071 : Tower
    # K1231 : Hospital
    # K1237 : Federal Penitentiary / State Prison
    # K2193 : Fire Department
    # K2110 : Military Installation
    # K2165 : Government Building
    # K1225 : Crew of Vessel Location (Berth)
    # K2543 : Education (Schools)
    return('Services')
  } else if (mtfcc %in% c('K2181','K2184','K2190','K2561','K2582','K3544')){
    # K2181,2184,2190: State/Federal land for parks
    # K2561 : Golf Course
    # K2582 : Cemetery
    # K3544 : Place of Worship
    return('Parks&Rec')
  } else if (mtfcc %in% c('K2451','K2452','K2454','K2460')){
    return('Transportation')
  } else {
    stop()
  }
}
new_coord = keep_coords[keep,]
new_slosh = res_lm[,keep]
new_slosh[is.na(new_slosh)] = 0
new_coord$Category = sapply(new_coord$MTFCC, categorize)
new_slosh = data.frame(t(new_slosh))

library(stringr)
names(new_slosh) = paste0('run_', str_pad(1:4000, 4, 'left', '0'))
out = cbind(new_coord, data.frame(new_slosh))
write.csv(out, file = gzfile('./data/filtered_data.csv.gz'), row.names = FALSE)
write.csv(
  out, 
  file = gzfile('~/git/projgamma/datasets/slosh/filtered_data.csv.gz'), 
  row.names = FALSE
  )
# EOF