#  Code for plotting resulting cluster assignments in the thetas.
rm(list = ls())
libs = c('ggplot2','GGally','dplyr')
sapply(libs, require, character.only = TRUE)
rm(libs)

scaling = function(in_df){
  in_df$theta[in_df$theta < 100] = in_df$theta[in_df$theta < 100] + 360
  means = apply(in_df, 2, mean)
  sds   = apply(in_df, 2, sd)
  out_df = t((t(in_df) - means) / sds)
  return(list(df = out_df, mean = means, sd = sds))
}

ccount = read.csv('~/git/projgamma/test/sloshltd_cluster_counts.csv')
ccount %>% group_by(dataset) %>% slice(which.min(ncluster))

ltd_path = '~/git/projgamma/datasets/slosh/sloshltd_clusters.csv'
apt_path = '~/git/projgamma/datasets/slosh/sloshapt_clusters.csv'
t90_path = '~/git/projgamma/datasets/slosh/slosht90_clusters.csv'
emg_path = '~/git/projgamma/datasets/slosh/sloshemg_clusters.csv'
xpt_path = '~/git/projgamma/datasets/slosh/sloshxpt_clusters.csv'

ltd_clu = read.csv(ltd_path)
apt_clu = read.csv(apt_path)
t90_clu = read.csv(t90_path)
xpt_clu = read.csv(xpt_path)
emg_clu = read.csv(emg_path)

theta = read.csv('~/git/surge/data/inputs.csv')
thinc = read.csv('~/git/projgamma/test/threshold_inclusion.csv')
names(thinc) = c('ltd','apt','t90','emg','xpt')

ltd_inp = theta[which(as.logical(thinc$ltd)),]
apt_inp = theta[which(as.logical(thinc$apt)),]
t90_inp = theta[which(as.logical(thinc$t90)),]
xpt_inp = theta[which(as.logical(thinc$xpt)),]
emg_inp = theta[which(as.logical(thinc$emg)),]

ltd_inp_sca = scaling(ltd_inp)
t90_inp_sca = scaling(t90_inp)
apt_inp_sca = scaling(apt_inp)
xpt_inp_sca = scaling(xpt_inp)
emg_inp_sca = scaling(emg_inp)

ltd = cbind(ltd_inp_sca$df, ltd_clu)
t90 = cbind(t90_inp_sca$df, t90_clu)
apt = cbind(apt_inp_sca$df, apt_clu)
xpt = cbind(xpt_inp_sca$df, xpt_clu)
emg = cbind(emg_inp_sca$df, emg_clu)

custom_diagonal <- function(data, mapping, ...) {
  plt = ggplot(data = data, mapping = mapping) + geom_density(..., alpha = 0.5)
  return(plt)
}
pairs_plot <- function(dat){
  plt = ggpairs(
    dat,
    columns = c('slr','theta','v','pmin','lat'), 
    mapping = aes(color = factor(post)),
    lower = list(continuous = wrap('points', alpha = 0.7, size = 0.2)), 
    upper = list(continuous = 'blank'), 
    diag = list(continuous = wrap('densityDiag', alpha = 0.7))
    )
  return(plt)
}

t90_pplot = pairs_plot(t90)
ltd_pplot = pairs_plot(ltd)
apt_pplot = pairs_plot(apt)
emg_pplot = pairs_plot(emg)
xpt_pplot = pairs_plot(xpt)

ggsave('~/git/exapg/plots/ltd_clusters.pdf', ltd_pplot)
ggsave('~/git/exapg/plots/t90_clusters.pdf', t90_pplot)
ggsave('~/git/exapg/plots/apt_clusters.pdf', apt_pplot)
ggsave('~/git/exapg/plots/emg_clusters.pdf', emg_pplot)
ggsave('~/git/exapg/plots/xpt_clusters.pdf', xpt_pplot)

























