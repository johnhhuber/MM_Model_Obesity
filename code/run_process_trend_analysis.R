# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary package
if(!require(Rfast)){install.package('Rfast'); library(Rfast)}

# load necessary functions
source('functions_convergence.R')
source('functions_likelihood.R')
source('functions_validate_inference.R')

# load the data
load('../output/data_organized_bmi.RData')

# load the posterior distribution and specify the burn in period 
burn.in = 1:2.5e5
thin.factor = 50

# bmi no spline 
get_convergence_stats(path.in = '../output/trend_analysis/bmi/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = max(burn.in),
                      thin.factor = thin.factor)
pool_chain(path.in = '../output/trend_analysis/bmi/',
           file.pattern = 'posterior_.*bz2',
           burn.in = max(burn.in),
           thin.factor = thin.factor,
           file.out = '../output/trend_analysis/bmi/posterior_pooled.csv')

# no bmi no spline 
get_convergence_stats(path.in = '../output/trend_analysis/no_bmi/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = max(burn.in),
                      thin.factor = thin.factor)
pool_chain(path.in = '../output/trend_analysis/no_bmi/',
           file.pattern = 'posterior_.*bz2',
           burn.in = max(burn.in),
           thin.factor = thin.factor,
           file.out = '../output/trend_analysis/no_bmi/posterior_pooled.csv')

# bmi spline 
burn.in = 1:5e5
thin.factor = 50

get_convergence_stats(path.in = '../output/trend_analysis/bmi_spline/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = max(burn.in),
                      thin.factor = thin.factor)
pool_chain(path.in = '../output/trend_analysis/bmi_spline/',
           file.pattern = 'posterior_.*bz2',
           burn.in = max(burn.in),
           thin.factor = thin.factor,
           file.out = '../output/trend_analysis/bmi_spline/posterior_pooled.csv')

# no bmi spline 
get_convergence_stats(path.in = '../output/trend_analysis/no_bmi_spline/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = max(burn.in),
                      thin.factor = thin.factor)
pool_chain(path.in = '../output/trend_analysis/no_bmi_spline/',
           file.pattern = 'posterior_.*bz2',
           burn.in = max(burn.in),
           thin.factor = thin.factor,
           file.out = '../output/trend_analysis/no_bmi_spline/posterior_pooled.csv')
