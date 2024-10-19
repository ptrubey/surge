# cluster summary
# - analyze ncluster_<dat>.csv for all dats to pick PY cluster parms to produce 
#   consistent number of clusters.

paths = Sys.glob('~/git/projgamma/test/ncluster_*.csv')
dfs = lapply(paths, read.csv)

dfnames = c('apt','del','emg','loc','ltd','nyc','t90','xpt')
for (i in 1:length(dfnames)) {
  dfs[[i]]$dataset = dfnames[i]
  }
df = do.call(rbind, dfs)[c('dataset','eta','discount','ncluster')]
