# regression_cluster_summary.R
# - read in regression output data from ~/git/projgamma/test
# - count number of clusters for a given dataset/conc/discount
# - prepare output table.

libs = c('stringr')
sapply(libs, require, character.only = TRUE)
rm(list = ls())

# files = Sys.glob('~/git/projgamma/test/sloshltd_*.pkl')
# files = files[!grepl('_gamma', files)]
files = Sys.glob('~/git/projgamma/test/sloshltd_*_delta.csv')
pars = data.frame(str_split_fixed(files, '_', 5)[,2:4])
names(pars) = c('dataset','concentration','discount')
pars$concentration = as.numeric(pars$concentration)
pars$discount = as.numeric(pars$discount)

extract_ncluster = function(path){
  df = read.csv(path)
  return(mean(apply(df, 1, function(x){length(unique(x))})))
}
pars$ncluster = sapply(files, extract_ncluster)
print(pars)
