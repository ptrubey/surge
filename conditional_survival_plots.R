rm(list = ls())
libs = c('ggplot2','ggpubr')
sapply(libs, require, character.only = TRUE)
rm(libs)
files = c(
  Sys.glob('~/git/projgamma/condsurv/*UpperBay.csv.gz'),
  Sys.glob('~/git/projgamma/condsurv/*LowerBay.csv.gz'),
  Sys.glob('~/git/projgamma/condsurv/*Mouth.csv.gz')
  )
files = files[order(files)]

files_doverafb = files[c(1,2,6)]
files_pia = files[c(13,14,18)]
files_packerave = files[c(10,11,12)]
files_doverafb_pia = files[c(3,4,5)]
files_doverafb_packer = files[c(7,8,9)]
files_pia_packer = files[c(15,16,17)]

scenarios = c('Lower Bay', 'Mouth', 'Upper Bay')
params = read.csv(gzfile('~/git/projgamma/condsurv/slosh_crt_params.csv.gz'))

plot_1d = function(paths, params){
  dfs = lapply(paths, function(x){data.frame(read.csv(gzfile(x)))})
  for (i in 1:length(scenarios)) {
    names(dfs[[i]]) = c('Z','Pw')
    dfs[[i]]$Scenario = scenarios[i]
  }
  df = do.call(rbind, dfs)
  df$W = params$a * ((((df$Z)^params$xi) - 1) / params$xi) + params$b
  df$W[df$W < 0] = 0.
  plt1 = ggplot(df, aes(x = Z, y = Pw, color = Scenario)) + 
    geom_line() +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  plt2 = ggplot(df, aes(x = W, y = Pw, color = Scenario)) +
    geom_line() + 
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  # return(ggarrange(plt1, plt2, ncol = 1, nrow = 2))#, common.legend = TRUE))
  return(list(plt1, plt2))
}
plot_2d = function(paths, params){
  dfs = lapply(paths, function(x){data.frame(read.csv(gzfile(x)))})
  for (i in 1:length(scenarios)) {
    names(dfs[[i]]) = c('Z1','Z2','Pw')
    dfs[[i]]$Scenario = scenarios[i]
  }
  df = do.call(rbind, dfs)
  df$W1 = params$a[1] * ((((df$Z1)^params$xi[1]) - 1) / params$xi[1]) + params$b[1]
  df$W2 = params$a[2] * ((((df$Z2)^params$xi[2]) - 1) / params$xi[2]) + params$b[2]
  df$W1[df$W1 < 0] = 0.
  df$W2[df$W2 < 0] = 0.
  plt1 = ggplot(df, aes(x = Z1, y = Z2, z = Pw, color = Scenario)) + 
    geom_contour() + 
    theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  plt2 = ggplot(df, aes(x = W1, y = W2, z = Pw, color = Scenario)) + 
    geom_contour() + 
    theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  # return(ggarrange(plt1,plt2, ncol = 1, nrow = 2, common.legend = TRUE))
  return(list(plt1,plt2))
}

p1_dafb = plot_1d(files_doverafb, params[1,])
p1_pak  = plot_1d(files_packerave, params[3,])
p1_pia  = plot_1d(files_pia, params[2,])

plots_p1 = ggarrange(
  p1_dafb[[1]], p1_pia[[1]], p1_pak[[1]],
  p1_dafb[[2]], p1_pia[[2]], p1_pak[[2]],
  nrow = 2, ncol = 3, common.legend = TRUE, legend = 'right'
  )
plots_p1_annotated = annotate_figure(
  plots_p1,
  top = paste0(
    '         Dover AFB        ',
    '                                    ',
    'Philadelphia Intl. Airport',
    '                             ',
    '   Packer Ave. Terminal              '
    ),
  left = paste0(
    'Real (Feet)            ',
    '         Standard Units'
    )
  )
ggsave('~/Desktop/temp.pdf', plots_p1_annotated, 
       height = 6.5, width = 12, units = 'in')
ggsave('~/git/exapg/plots/condsurv/condsurv_1d_mcmc_combined.pdf',
       plots_p1_annotated, height = 6.5, width = 12, units = 'in')
ggsave('~/git/dissertation/ch3/plots/condsurv/condsurv_1d_mcmc_combined.pdf',
       plots_p1_annotated, height = 6.5, width = 12, units = 'in')

p2_dafb_pia = plot_2d(files_doverafb_pia, params[c(1,2),])
p2_dafb_pak = plot_2d(files_doverafb_packer, params[c(1,3),])
p2_pia_pak  = plot_2d(files_pia_packer, params[c(2,3),])

plots_p2  = ggarrange(
  p2_dafb_pia[[1]], p2_dafb_pak[[1]], p2_pia_pak[[1]],
  p2_dafb_pia[[2]], p2_dafb_pak[[2]], p2_pia_pak[[2]],
  nrow = 2, ncol = 3, common.legend = TRUE, legend = 'right'
  )
plots_p2_annotated = annotate_figure(
  plots_p2,
  top = paste0(
    '    Dover AFB :: PIA    ',
    '                               ',
    'Dover AFB :: Packer Ave.',
    '                               ',
    '   PIA :: Packer Ave.               '
    ),
  left = paste0(
    'Real (Feet)           ',
    '         Standard Units'
    )
  )
ggsave('~/Desktop/temp.pdf', plots_p2_annotated, 
       height = 6.5, width = 12, units = 'in')
ggsave('~/git/exapg/plots/condsurv/condsurv_2d_mcmc_combined.pdf', 
       plots_p2_annotated, height = 6.5, width = 12, units = 'in') 
ggsave('~/git/dissertation/ch3/plots/condsurv/condsurv_2d_mcmc_combined.pdf', 
       plots_p2_annotated, height = 6.5, width = 12, units = 'in') 

#------------------------------------------------------------------------------#
#         Regression Conditional Survival                                      #
#------------------------------------------------------------------------------#
path_parse = function(paths){
  df = data.frame(path = paths)
  
  df$scenario = ''
  df$scenario[grepl('LowerBay', df$path, fixed = TRUE)] = 'Lower Bay'
  df$scenario[grepl('UpperBay', df$path, fixed = TRUE)] = 'Upper Bay'
  df$scenario[grepl('Mouth', df$path, fixed = TRUE)]    = 'Mouth'
  df$scenario[grepl('BackBay1', df$path, fixed = TRUE)] = 'Back Bay 1'
  df$scenario[grepl('BackBay2', df$path, fixed = TRUE)] = 'Back Bay 2'
  df$scenario[grepl('FullBay', df$path, fixed = TRUE)]  = 'Full Bay'
  
  df = df[df$scenario %in% c('Lower Bay','Upper Bay','Mouth'),]
  
  df$strength = ''
  df$strength[grepl('328', df$path, fixed = TRUE)] = 'Strong'
  df$strength[grepl('687', df$path, fixed = TRUE)] = 'Strong'
  df$strength[grepl('761', df$path, fixed = TRUE)] = 'Strong'
  df$strength[grepl('772', df$path, fixed = TRUE)] = 'Strong'
  df$strength[grepl('359', df$path, fixed = TRUE)] = 'Weak'
  df$strength[grepl('439', df$path, fixed = TRUE)] = 'Weak'
  df$strength[grepl('671', df$path, fixed = TRUE)] = 'Weak'
  df$strength[grepl('747', df$path, fixed = TRUE)] = 'Weak'
  
  df$landing = NA
  df$landing[grepl('687', df$path, fixed = TRUE)] = 'North'
  df$landing[grepl('772', df$path, fixed = TRUE)] = 'North'
  df$landing[grepl('671', df$path, fixed = TRUE)] = 'North'
  df$landing[grepl('747', df$path, fixed = TRUE)] = 'North'
  df$landing[grepl('328', df$path, fixed = TRUE)] = 'South'
  df$landing[grepl('761', df$path, fixed = TRUE)] = 'South'
  df$landing[grepl('359', df$path, fixed = TRUE)] = 'South'
  df$landing[grepl('439', df$path, fixed = TRUE)] = 'South'
  
  df$direction = NA
  df$direction[grepl('328', df$path, fixed = TRUE)] = 'SouthWest'
  df$direction[grepl('772', df$path, fixed = TRUE)] = 'SouthWest'
  df$direction[grepl('359', df$path, fixed = TRUE)] = 'SouthWest'
  df$direction[grepl('671', df$path, fixed = TRUE)] = 'SouthWest'
  df$direction[grepl('761', df$path, fixed = TRUE)] = 'North'
  df$direction[grepl('687', df$path, fixed = TRUE)] = 'North'
  df$direction[grepl('439', df$path, fixed = TRUE)] = 'North'
  df$direction[grepl('747', df$path, fixed = TRUE)] = 'North'
  
  idx = which(!(
    (df$direction == 'North' & df$landing == 'North') |
    (df$direction == 'SouthWest' & df$landing == 'South')
    ) | is.na(df$direction))
  df = df[idx,]
  return(df)
}

dover_meta  = path_parse(Sys.glob('~/git/projgamma/condsurv_reg/DoverAFB_*.csv.gz'))
pia_meta    = path_parse(Sys.glob('~/git/projgamma/condsurv_reg/PIA_*.csv.gz'))
packer_meta = path_parse(Sys.glob('~/git/projgamma/condsurv_reg/PackerAve_*.csv.gz'))

dover_pia_meta    = path_parse(Sys.glob('~/git/projgamma/condsurv_reg/DoverAFBPIA_*.csv.gz'))
dover_packer_meta = path_parse(Sys.glob('~/git/projgamma/condsurv_reg/DoverPackerAve_*.csv.gz'))
pia_packer_meta   = path_parse(Sys.glob('~/git/projgamma/condsurv_reg/PIAPackerAve_*.csv.gz'))

plot_1d_reg = function(meta, locname, params){ # , basepath){
  dfs = lapply(meta$path, function(x){data.frame(read.csv(gzfile(x)))})
  for (i in 1:nrow(meta)) {
    names(dfs[[i]])    = c('iter', 'Z','Pw')
    dfs[[i]]$Scenario  = meta$scenario[i]
    dfs[[i]]$Strength  = meta$strength[i]
    dfs[[i]]$Landing   = meta$landing[i]
    dfs[[i]]$Direction = meta$direction[i]
    dfs[[i]]$LandingDirection = paste(meta$landing[i], '+', meta$direction[i])
  }
  df = do.call(rbind, dfs)
  df$W = params$a * ((((df$Z)^params$xi) - 1) / params$xi) + params$b
  df$W[df$W < 0] = 0.
  df$Strength[is.na(df$Landing)] = 'Baseline'
  df$LandingDirection[is.na(df$Landing)] = 'Baseline'
  pltz = ggplot(
        df, 
        aes(x = Z, y = Pw, color = Strength, linetype = LandingDirection)
        ) + 
    geom_line() +
    theme_minimal() +
    facet_wrap(~Scenario, nrow = 1) +
    theme(
      title = element_blank(), axis.title = element_blank(),
      strip.background = element_blank(), strip.text.x = element_blank()
      )
  pltw = ggplot(
        df, 
        aes(x = W, y = Pw, color = Strength, linetype = LandingDirection)
        ) + 
    geom_line() +
    theme_minimal() +
    facet_wrap(~Scenario, nrow = 1) +
    theme(
      title = element_blank(), axis.title = element_blank(),
      strip.background = element_blank(), strip.text.x = element_blank()
    )
  return(list(pltz,pltw))
}
plot_2d_reg = function(meta, locname, params){ 
  dfs = lapply(meta$path, function(x){data.frame(read.csv(gzfile(x)))})
  for (i in 1:nrow(meta)) {
    names(dfs[[i]])    = c('iter', 'Z1','Z2','Pw')
    dfs[[i]]$Scenario  = meta$scenario[i]
    dfs[[i]]$Strength  = meta$strength[i]
    dfs[[i]]$Landing   = meta$landing[i]
    dfs[[i]]$Direction = meta$direction[i]
    dfs[[i]]$LandingDirection = paste(meta$landing[i], '+', meta$direction[i])
  }
  df = do.call(rbind, dfs)
  df$W1 = params$a[1] * ((((df$Z1)^params$xi[1]) - 1) / params$xi[1]) + params$b[1]
  df$W2 = params$a[2] * ((((df$Z2)^params$xi[2]) - 1) / params$xi[2]) + params$b[2]
  df$W1[df$W1 < 0] = 0.
  df$W2[df$W2 < 0] = 0.
  df$Strength[is.na(df$Landing)] = 'Baseline'
  df$LandingDirection[is.na(df$Landing)] = 'Baseline'
  
  pltz = ggplot(df, aes(x = Z1, y = Z2, z = Pw, color = Strength, linetype = LandingDirection)) + 
    geom_contour() + 
    theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    facet_wrap(~Scenario, nrow = 1) +
    theme(
      title = element_blank(),axis.title = element_blank(),
      strip.background = element_blank(), strip.text.x = element_blank()
      )
  pltw = ggplot(df, aes(x = W1, y = W2, z = Pw, color = Strength, linetype = LandingDirection)) + 
    geom_contour() + 
    theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    facet_wrap(~Scenario, nrow = 1) +
    theme(
      title = element_blank(),axis.title = element_blank(),
      strip.background = element_blank(), strip.text.x = element_blank()
    )
  return(list(pltz,pltw))
}

p1r_dafb = plot_1d_reg(dover_meta,  'Dover AFB', params[1,])
p1r_pia  = plot_1d_reg(pia_meta,    'PIA', params[2,])
p1r_pak  = plot_1d_reg(packer_meta, 'Packer Ave', params[3,])
p2r_dafb_pia = plot_2d_reg(dover_pia_meta,    'Dover : PIA', params[c(1,2),])
p2r_dafb_pak = plot_2d_reg(dover_packer_meta, 'Dover : Packer', params[c(1,3),])
p2r_pia_pak  = plot_2d_reg(pia_packer_meta,   'PIA : Packer', params[c(2,3),])

p1rz = ggarrange(p1r_dafb[[1]], p1r_pia[[1]], p1r_pak[[1]], common.legend = TRUE, 
                ncol = 1, nrow = 3, legend = 'right')
p1rz_annotated = annotate_figure(
  p1rz, 
  top = paste0(
    'Lower Bay    ',
    '                                      ',
    '              Mouth                   ',
    '                                      ',
    '     Upper Bay                     '
    ),
  left = paste0(
    '           Packer Ave            ',
    '                                 ',
    '              PIA                ',
    '                                 ',
    '            Dover AFB            '
    )
  )
p1rw = ggarrange(p1r_dafb[[2]], p1r_pia[[2]], p1r_pak[[2]], common.legend = TRUE, 
                 ncol = 1, nrow = 3, legend = 'right')
p1rw_annotated = annotate_figure(
  p1rw, 
  top = paste0(
    'Lower Bay    ',
    '                                      ',
    '              Mouth                   ',
    '                                      ',
    '     Upper Bay                     '
  ),
  left = paste0(
    '           Packer Ave            ',
    '                                 ',
    '              PIA                ',
    '                                 ',
    '            Dover AFB            '
  )
)
ggsave('~/Desktop/temp.png', p1rz_annotated, height = 12, width = 14, units = 'in')
ggsave('~/git/exapg/plots/condsurv_reg/condsurv_reg_1d_std.pdf', 
       p1rz_annotated, height = 12, width = 14, units = 'in')
ggsave('~/git/dissertation/ch3/plots/condsurv_reg/condsurv_reg_1d_std.pdf', 
       p1rz_annotated, height = 12, width = 14, units = 'in')
ggsave('~/git/exapg/plots/condsurv_reg/condsurv_reg_1d_real.pdf', 
       p1rw_annotated, height = 12, width = 14, units = 'in')
ggsave('~/git/dissertation/ch3/plots/condsurv_reg/condsurv_reg_1d_real.pdf', 
       p1rw_annotated, height = 12, width = 14, units = 'in')

p2rz = ggarrange(
  p2r_dafb_pia[[1]], p2r_dafb_pak[[1]], p2r_pia_pak[[1]], 
  common.legend = TRUE, ncol = 1, nrow = 3, legend = 'right'
  )
p2rz_annotated = annotate_figure(p2rz,
    top = paste0(
        'Lower Bay    ',
        '                                      ',
        '              Mouth                   ',
        '                                      ',
        '     Upper Bay                     '
    ),
    left = paste0(
      '        PIA : Packer Ave        ',
      '                                 ',
      '       Dover AFB : Packer       ',
      '                                 ',
      '        Dover AFB : PIA         '
    )
)
p2rw = ggarrange(
  p2r_dafb_pia[[2]], p2r_dafb_pak[[2]], p2r_pia_pak[[2]], 
  common.legend = TRUE, ncol = 1, nrow = 3, legend = 'right'
)
p2rw_annotated = annotate_figure(p2rw,
    top = paste0(
      'Lower Bay    ',
      '                                      ',
      '              Mouth                   ',
      '                                      ',
      '     Upper Bay                     '
    ),
    left = paste0(
      '        PIA : Packer Ave        ',
      '                                 ',
      '       Dover AFB : Packer       ',
      '                                 ',
      '        Dover AFB : PIA         '
    )
  )
ggsave('~/Desktop/temp.png', p2rz_annotated, height = 12, width = 14, units = 'in')
ggsave('~/git/exapg/plots/condsurv_reg/condsurv_reg_2d_std.pdf', 
       p2rz_annotated, height = 12, width = 14, units = 'in')
ggsave('~/git/dissertation/ch3/plots/condsurv_reg/condsurv_reg_2d_std.pdf', 
       p2rz_annotated, height = 12, width = 14, units = 'in')
ggsave('~/git/exapg/plots/condsurv_reg/condsurv_reg_2d_real.pdf', 
       p2rw_annotated, height = 12, width = 14, units = 'in')
ggsave('~/git/dissertation/ch3/plots/condsurv_reg/condsurv_reg_2d_real.pdf', 
       p2rw_annotated, height = 12, width = 14, units = 'in')

rm(list = ls())
# EOF