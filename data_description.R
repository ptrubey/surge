rm(list = ls())
libs = c('dplyr','tidyr','xtable')
sapply(libs, require, character.only = TRUE)
rm(libs)

path_base = '~/git/projgamma/datasets/slosh/slosh_%s_data.csv.gz'
# datasets= c('t90','ltd','xpt','apt','emg','loc','del','nyc')
datasets  = c('t90','dbg','del','crt')
# dsnames = c('Threshold.9', 'Limited', 'Transport', 'Airports', 'Emergency','Locales','Delaware Bay','New York MSA')
dsnames   = c('Threshold','Delaware','Restricted','Critical')
# quantiles = c(0.9, rep(0.95, 5), 0.95, 0.95)
paths     = sprintf(path_base, datasets)

dfs = list()
for(i in 1:length(datasets)){
  dfs[[i]] = read.csv(gzfile(paths[i]))
}

summarize_dataset = function(df, qtl, name, dsname){
  obs = as.array(t(df[,-c(1:8)]))
  thr = apply(obs, 2, function(x){quantile(x, qtl)})
  pas = t(t(obs) > thr)
  rows = which(as.logical(apply(pas, 1, any)))
  kept = obs[rows, ]
  return(c(
    Dataset = dsname,
    Cols = ncol(kept), 
    Rows = nrow(kept), 
    Pinc = round(nrow(kept) / 4000, 3)
    ))
}

summaries = list()
for(i in 1:length(datasets)){
  summaries[[i]] = summarize_dataset(dfs[[i]], 0.9, datasets[i], dsnames[i])
}
summary = do.call(rbind, summaries)
# summary = data.frame(Data = dsnames, Quantile = quantiles, summary)
# names(summary)[5:8] = c('T.05','T.25','T.75','T.95')
summary = summary[order(summary$Cols, decreasing = TRUE),]
names(summary)[4] = '$\\text{P}(W\\not\\leq b)$'
print(xtable(summary), include.rownames = FALSE, 
      floating = FALSE, file = '~/git/exapg/tables/datadesc.tex')

# tab = xtable(summary, digits = c(0,0,2,0,0,3,3,3,3))
# print(tab, include.rownames = FALSE, floating = FALSE,
#       file = '~/git/exapg/tables/datadesc.tex')

clusters = read.csv('~/git/projgamma/datasets/slosh/cluster_counts.csv')
clusters$Fit[clusters$Fit == 'VB'] = 'Var Bayes'
clusters$Fit[clusters$Fit == 'MC'] = 'Monte Carlo'
clusters_wide = pivot_wider(clusters, names_from = Fit, values_from = Clusters)
clusters_wide[is.na(clusters_wide)] = '~'

print(
  xtable(clusters_wide), 
  include.rownames = FALSE,
  floating = FALSE,
  file = '~/git/exapg/tables/extant_clusters.tex'
  )
