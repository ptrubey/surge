#  Setup 
#----------------------------------
rm(list = ls())
libs = c('dplyr','ggplot2', 'maps', 'sf', 'gridExtra', 'xtable',
         'ggExtra', 'patchwork', 'Cairo', 'ggmap','scales')
sapply(libs, require, character.only = TRUE)
rm(libs)
register_stadiamaps('bb37e42d-d438-416f-8003-d09c1e6a1347')
#----------------------------------
#  Data Import
#----------------------------------
states = map_data('state')
slosh = read.csv(gzfile('~/git/projgamma/datasets/slosh/slosh_del_data.csv.gz'))
locdata = slosh[,1:8]
raw = t(slosh[,-c(1:8)])
# Relevant Locs: 
# - (23) Dover AFB
# - (53) Philadelphia Intl. Airport
#----------------------------------
#  Delaware Map
#----------------------------------
# delaware <- c(left = min(locdata$long) - 0.2, bottom = min(locdata$lat) - 0.1, 
#               right = max(locdata$long) + 0.2, top = max(locdata$lat) + 0.1)
# delaware_basemap <- get_stadiamap(
#         delaware, zoom = 10, maptype = 'stamen_toner_lite'
#         )
# delaware_map = ggmap(delaware_basemap) + #, darken = 0.5) + 
#   geom_point(aes(x = long, y = lat, color = Category), data = locdata) +
#   geom_label(aes(x = long, y = lat, label = FULLNAME, vjust = 'top', hjust = 'left'), 
#              data = locdata[c(23,56),], position = position_dodge2(width = 0.5), 
#              alpha = 0.8) + xlab(element_blank()) + ylab(element_blank()) + 
#   theme(legend.title = element_blank(), legend.position = c(0.15, 0.91),
#         legend.key = element_blank(), 
#         legend.background = element_rect(fill = alpha('white',0.8)))
# print(delaware_map)
# ggsave('~/git/exapg/plots/delaware.pdf', delaware_map)
#---------------------------------
#  Posterior Predictive Data
#---------------------------------
# mc_alphas = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/mc_alphas.csv.gz'))
# vb_alphas = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/vb_alphas.csv.gz'))
# r1_alphas = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/reg_1_alphas.csv.gz'))
# r0_alphas = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/reg_0_alphas.csv.gz'))
# save(
#   mc_alphas, vb_alphas, r1_alphas, r0_alphas, compress = 'xz',
#   file = '~/Nextcloud/Research/Projects/Extremes/surge/data/delaware.rda'
#   )
load(file = '~/Nextcloud/Research/Projects/Extremes/surge/data/delaware.rda')
V = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/V.csv.gz'))
R = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/R.csv.gz'))[,1]
Z = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/Z.csv.gz'))
P = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/P.csv.gz'))
I = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/I.csv.gz'))[,1]
Zi = Z[I + 1,]
W = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/W.csv.gz'))
parms = data.frame(t(P))
names(parms) = c('b','sigma','chi')

#---------------------------------
#  Posterior Recovery of Univariate CDF's
#---------------------------------
posterior_v = function(alphas){
  gammas = apply(
    alphas[,3:ncol(alphas)], 2, 
    function(x){rgamma(shape = x, n = nrow(alphas))}
    )
  Vp = gammas / apply(gammas, 1, max)
  return(Vp)
}
posterior_z = function(alphas){
  Vp = posterior_v(alphas)
  Pp = 1 / (1 - runif(n = nrow(alphas), max = 1 - 1e-5, min = 1e-5))
  Zp = Vp * Pp
  return(Zp)
}
posterior_w = function(alphas){
  Zp = posterior_z(alphas)
  Wp = t(apply(Zp, 1, function(x){
    return((x^parms$chi - 1) * parms$sigma / parms$chi + parms$b)
    }))
  Wp[Wp < 0] = 0
  return(Wp)
}

mc_postz = posterior_z(mc_alphas)
mc_postv = posterior_v(mc_alphas)
mc_postw = posterior_w(mc_alphas)

vb_postz = posterior_z(vb_alphas)
vb_postv = posterior_v(vb_alphas)
vb_postw = posterior_w(vb_alphas)

r1_postz = posterior_z(r1_alphas)
r1_postv = posterior_v(r1_alphas)
r1_postw = posterior_w(r1_alphas)

r0_postz = posterior_z(r0_alphas)
r0_postv = posterior_v(r0_alphas)
r0_postw = posterior_w(r0_alphas)

location_plot_v = function(location){
  mc = data.frame(V = mc_postv[,location], Fit = 'Monte Carlo')
  vb = data.frame(V = vb_postv[,location], Fit = 'Var Bayes')
  r1 = data.frame(V = r1_postv[,location], Fit = 'Reg w/ FE')
  r0 = data.frame(V = r0_postv[,location], Fit = 'Reg w/o FE')
  ra = data.frame(V = V[,location], Fit = 'Empirical')
  df = do.call(rbind, list(mc, vb, r1, r0, ra))
  var_order = c('Empirical', 'Monte Carlo', 'Var Bayes', 
                'Reg w/ FE', 'Reg w/o FE')
  df$Model = factor(as.character(df$Fit), levels = var_order)
  plt = ggplot(df, aes(x = V, color = Model)) + 
    stat_ecdf() +
    xlab(element_blank()) + ylab(element_blank()) + 
    xlim(c(0,1)) + theme_minimal() +
    theme(legend.position = c(0.8, 0.25))
  return(plt)
}
location_plot_z = function(location){
  mc = data.frame(Z = mc_postz[,location], Fit = 'Monte Carlo')
  vb = data.frame(Z = vb_postz[,location], Fit = 'Var Bayes')
  r1 = data.frame(Z = r1_postz[,location], Fit = 'Reg w/ FE')
  r0 = data.frame(Z = r0_postz[,location], Fit = 'Reg w/o FE')
  ra = data.frame(Z = Zi[,location], Fit = 'Empirical')
  df = do.call(rbind, list(mc, vb, r1, r0, ra))
  var_order = c('Empirical', 'Monte Carlo', 'Var Bayes', 
                'Reg w/ FE', 'Reg w/o FE')
  df$Model = factor(as.character(df$Fit), levels = var_order)
  plt = ggplot(df, aes(x = Z, color = Model)) + 
    stat_ecdf() +
    xlab(element_blank()) + ylab(element_blank()) + 
    xlim(c(0,20)) + theme_minimal() +
    theme(legend.position = c(0.8, 0.25))
  return(plt)
}
location_plot_w = function(location){
  mc = data.frame(W = mc_postw[,location], Fit = 'Monte Carlo')
  vb = data.frame(W = vb_postw[,location], Fit = 'Var Bayes')
  r0 = data.frame(W = r0_postw[,location], Fit = 'Reg w/ FE')
  r1 = data.frame(W = r1_postw[,location], Fit = 'Reg w/o FE')
  ra = data.frame(W = W[,location], Fit = 'Empirical')
  df = do.call(rbind, list(mc, vb, r1, r0, ra))
  var_order = c('Empirical','Monte Carlo','Var Bayes','Reg w/ FE', 'Reg w/o FE')
  df$Model = factor(as.character(df$Fit), levels = var_order)
  plt = ggplot(df, aes(x = W, color = Model)) +
    stat_ecdf() + 
    xlab(element_blank()) + ylab(element_blank()) + 
    theme_minimal() + 
    theme(legend.position = c(0.8, 0.25))
  return(plt)
}

marginal_plot_dafb_z = location_plot_z(23L)
marginal_plot_dafb_v = location_plot_v(23L)
marginal_plot_dafb_w = location_plot_w(23L)
marginal_plot_pia_z  = location_plot_z(53L)
marginal_plot_pia_v  = location_plot_v(53L)
marginal_plot_pia_w  = location_plot_w(53L)

# ggsave('~/git/exapg/plots/delaware_marginal_z_dover_afb.pdf', marginal_plot_dafb_z)
# ggsave('~/git/exapg/plots/delaware_marginal_v_dover_afb.pdf', marginal_plot_dafb_v)
# ggsave('~/git/exapg/plots/delaware_marginal_z_phil_ia.pdf', marginal_plot_pia_z)
# ggsave('~/git/exapg/plots/delaware_marginal_v_phil_ia.pdf', marginal_plot_pia_v)

# marginal_dafb = grid.arrange(marginal_plot_dafb_v, marginal_plot_dafb_z, ncol = 2)
# marginal_pia  = grid.arrange(marginal_plot_pia_v, marginal_plot_pia_z, ncol = 2)
# marginal_dafb = grid.arrange(marginal_plot_dafb_v, marginal_plot_dafb_w, ncol = 2)
# marginal_pia  = grid.arrange(marginal_plot_pia_v, marginal_plot_pia_w, ncol = 2)

# ggsave('~/git/exapg/plots/delaware_marginal_dover_afb.pdf', 
#        marginal_dafb, width = 10.5, height = 4.5, units = 'in')
# ggsave('~/git/exapg/plots/delaware_marginal_phil_ia.pdf', 
#        marginal_pia, width = 10.5, height = 4.5, units = 'in')
# ggsave('~/git/exapg/plots/delaware_marginal_dover_afb.png',
#        marginal_dafb, width = 10.5, height = 4.5, units = 'in')
# ggsave('~/git/exapg/plots/delaware_marginal_phil_ia.png',
#        marginal_pia, width = 10.5, height = 4.5, units = 'in')

make_marginal_plots = function(n){
  pv = location_plot_v(n)
  pw = location_plot_w(n)
  pp = grid.arrange(pv,pw, ncol = 2, top = paste(i, locdata$FULLNAME[i]))
  ggsave(sprintf('~/scratch/delaware_%02d.png', i), pp, width = 8, height = 3.5, units = 'in')
}
# for(i in 1:ncol(W)){
#   make_marginal_plots(i)
# }

rm(list = ls())

mc_delta = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/mc_delta.csv.gz'))
vb_delta = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/vb_delta.csv.gz'))
r0_delta = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/reg_0_delta.csv.gz'))
r1_delta = read.csv(gzfile('~/git/projgamma/datasets/slosh/del/reg_1_delta.csv.gz'))

bincount = function(x){table(factor(x, levels = 0:199))}
activeclust = function(arr){
  ccount = t(apply(arr, 1, bincount))
  return(ccount[,which(!apply(ccount, 2, function(x){all(x == 0)}))])
}
mc_ccount = activeclust(mc_delta)
vb_ccount = activeclust(vb_delta)
r0_ccount = activeclust(r0_delta)
r1_ccount = activeclust(r1_delta)

cluster_concentration = function(arr, probs = c(0.9, 0.99, 0.999)){
  # assume arr is output of activeclust
  sums = apply(arr, 2, sum)
  sums = sums[order(sums, decreasing = TRUE)]
  names(sums) = 1:length(sums)
  cprob = cumsum(sums) / sum(sums)
  return(sapply(probs, function(p){sum(cprob < p) + 1}))
}

mc_conc = cluster_concentration(mc_ccount)
vb_conc = cluster_concentration(vb_ccount)
r0_conc = cluster_concentration(r0_ccount)
r1_conc = cluster_concentration(r1_ccount)

concentrations = data.frame(rbind(mc_conc, vb_conc, r0_conc, r1_conc))
names(concentrations) = c('c0.9','c0.99','c0.999')
concentrations$Model = c('Monte Carlo', 'Var Bayes', 'Reg w/o FE', 'Reg w/ FE')
concentrations = concentrations[c('Model','c0.9','c0.99','c0.999')]
names(concentrations)[2:4] = c('0.9','0.99','0.999')

print(
  xtable(
    concentrations, 
    digits = 0,
    caption = 'Cluster concentration for identified models and fitting 
        methods, on the \\emph{Restricted} Slice: columns specify quantiles 
        detailing the proportion of data contained within the table cells 
        indicated number of clusters.\\label{tab:cluster_concentration}'
    ), 
  include.rownames = FALSE,
  file = '~/git/exapg/tables/cluster_concentration.tex'
  )









