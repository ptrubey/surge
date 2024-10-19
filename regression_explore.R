# Setup
rm(list = ls())
libs = c('Ternary')
sapply(libs, require, character.only = TRUE)
rm(libs)

# Import Posterior Results
deltas = read.csv('~/git/projgamma/simulated/reg/postdeltas.csv')
deltas = as.matrix(sapply(deltas[seq(15001, 30000, by = 15), ], as.integer))



