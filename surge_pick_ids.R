rm(list = ls())
libs = c('snowfall')
load('./data/slosh_1run.rda')
source('surge_fns.R')
rm(out)
resolution = 100; eps  = 0.0001

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
 
# EOF