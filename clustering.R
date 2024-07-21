#  Code for plotting resulting cluster assignments in the thetas.
rm(list = ls())
libs = c('ggplot2','GGally')
sapply(libs, require, character.only = TRUE)
rm(libs)

ltd_pre_path = '~/git/projgamma/datasets/slosh/sloshltd_cluster_pre.csv'
ltd_pos_path = '~/git/projgamma/datasets/slosh/sloshltd_clusters_post.csv'
ful_pre_path = '~/git/projgamma/datasets/slosh/slosh_cluster_pre.csv'
ful_pos_path = '~/git/projgamma/datasets/slosh/slosh_clusters_post.csv'
ful_inp_path = '~/git/projgamma/datasets/slosh/slosh_tinputs.csv'
ltd_inp_path = '~/git/projgamma/datasets/slosh/sloshltd_tinputs.csv'

t90_pre_path = '~/git/projgamma/datasets/slosh/slosht90_cluster_pre.csv'
t90_pos_path = '~/git/projgamma/datasets/slosh/slosht90_clusters_post.csv'
t90_inp_path = '~/git/projgamma/datasets/slosh/slosht90_tinputs.csv'

scaling = function(in_df){
  in_df$theta[in_df$theta < 100] = in_df$theta[in_df$theta < 100] + 360
  means = apply(in_df, 2, mean)
  sds   = apply(in_df, 2, sd)
  out_df = t((t(in_df) - means) / sds)
  return(list(df = out_df, mean = means, sd = sds))
}

ltd_pre = read.csv(ltd_pre_path)
ltd_pos = read.csv(ltd_pos_path)
ful_pre = read.csv(ful_pre_path)
ful_pos = read.csv(ful_pos_path)
t90_pre = read.csv(t90_pre_path)
t90_pos = read.csv(t90_pos_path)

ltd_inp = read.csv(ltd_inp_path)
ful_inp = read.csv(ful_inp_path)
t90_inp = read.csv(t90_inp_path)

ltd_inp_sca = scaling(ltd_inp)
ful_inp_sca = scaling(ful_inp)
t90_inp_sca = scaling(t90_inp)

ltd = data.frame(ltd_inp_sca$df, pre = ltd_pre$cid, post = ltd_pos$cid)
ful = data.frame(ful_inp_sca$df, pre = ful_pre$cid, post = ful_pos$cid)
t90 = data.frame(t90_inp_sca$df, pre = t90_pre$cid, post = t90_pos$cid)

custom_diagonal <- function(data, mapping, ...) {
  plt = ggplot(data = data, mapping=mapping) + geom_density(..., alpha = 0.5)
  return(plt)
}
pairs_plot <- function(dat, col){
  plt = ggpairs(
    ltd,
    columns = c('slr','theta','v','pmin','lat'), 
    mapping = aes(color = factor(pre)) # , 
    # diag = custom_diagonal
    )
  return(plt)
}
# ltd_pre_plot = pairs_plot(ltd)
ltd_pre_plot = ggpairs(ltd, columns = c('slr','theta','v','pmin','lat'), mapping = aes(color = factor(pre)))
ltd_pos_plot = ggpairs(ltd, columns = c('slr','theta','v','pmin','lat'), mapping = aes(color = factor(post)))
ful_pre_plot = ggpairs(ful, columns = c('slr','theta','v','pmin','lat'), mapping = aes(color = factor(pre)))
ful_pos_plot = ggpairs(ful, columns = c('slr','theta','v','pmin','lat'), mapping = aes(color = factor(post)))
t90_pre_plot = ggpairs(t90, columns = c('slr','theta','v','pmin','lat'), mapping = aes(color = factor(pre)))
t90_pos_plot = ggpairs(t90, columns = c('slr','theta','v','pmin','lat'), mapping = aes(color = factor(post)))

ggsave('~/git/exapg/plots/ltd_clusters_pre.png', ltd_pre_plot)
ggsave('~/git/exapg/plots/ltd_clusters_post.png', ltd_pos_plot)
ggsave('~/git/exapg/plots/ful_clusters_pre.png', ful_pre_plot)
ggsave('~/git/exapg/plots/ful_clusters_post.png', ful_pos_plot)
ggsave('~/git/exapg/plots/t90_clusters_pre.png', t90_pre_plot)
ggsave('~/git/exapg/plots/t90_clusters_post.png', t90_pos_plot)



