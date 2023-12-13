rm(list = ls())
load('./data/slosh_1run.rda')
coords = data.frame(coords)
coords$IDX = 1:nrow(coords)
paste(min(coords$y), ', ', min(coords$x))
paste(max(coords$y), ', ', max(coords$x))