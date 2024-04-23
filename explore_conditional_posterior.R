rm(list = ls())
libs = c('tidyr', 'readr')
sapply(libs, require, character.only = TRUE)

inputs = read_csv('./data/temp_inputs.csv.gz')
alphas = read_csv('./data/temp_alphas.csv.gz')

names(inputs) = c('slr', 'theta', 'v', 'pmin', 'lat')
inputs$theta = inputs$theta - 100
inputs$theta[inputs$theta < 0] = inputs$theta[inputs$theta < 0] + 360

scinputs = inputs
scinputs$slr = scale(inputs$slr)
scinputs$theta = scale(inputs$theta)
scinputs$v = scale(inputs$v)
scinputs$pmin = scale(inputs$pmin)
scinputs$lat = scale(inputs$lat)

summary(lm(log(alphas$X6) ~ scinputs$slr + scinputs$theta + scinputs$v + scinputs$pmin + scinputs$lat))
