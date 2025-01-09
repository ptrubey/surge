rm(list = ls())

path_i = '~/git/projgamma/datasets/slosh/crt/I.csv.gz'
path_t = '~/git/projgamma/datasets/slosh/slosh_params.csv'
path_o = '~/git/projgamma/datasets/slosh/crt/storms.csv'

theta = read.csv(path_t)
index = read.csv(gzfile(path_i))

filtered = theta[index$X0,]
filtered$theta[filtered$theta < 100] = filtered$theta[filtered$theta < 100] + 360
scaled = data.frame(apply(filtered, 2, scale))

weak_idx = which(
  (-0.5 < scaled$slr & scaled$slr < 0.5) &  # neutral SLR
  (scaled$theta < -1 | scaled$theta > 1) &  # high angle of approach
  (scaled$pmin >   0.5)                  &  # weak storm
  (scaled$v    < - 0.5)                     # ^
  )
scaled[weak_idx,] # take 329,762 -- weak storm; south latitudes, alternating direction
                  # take 688,773 -- weak storm; north latitudes, alternating direction
strong_idx = which(
  (-0.5 < scaled$slr & scaled$slr < 0.5) &  # neutral SLR
  (scaled$theta < -1 | scaled$theta > 1) &  # high angle of approach
  (scaled$pmin <  - 0.5)                 &  # strong storm
  (scaled$v    >    0.5)                    # ^
  )
scaled[strong_idx,] # take 360, 440 -- strong storm; south_latitudes, alternating direction
                    # take 672, 748 -- strong storm, north latitude, alternating direction
idxs = c(329, 762, 688, 773, 360, 440, 672, 748) - 1 # for python indexing
write.csv(data.frame(storms = idxs), file = path_o, row.names = FALSE)
