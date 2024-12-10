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
load('../output/data_organized.RData')

# load the posterior distribution and specify the burn in period 
burn.in = 1:5e5
thin.factor = 50

get_convergence_stats(path.in = '../output/single_year_analysis/bmi_mgus_mm/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = max(burn.in),
                      thin.factor = thin.factor)

pool_chain(path.in = '../output/single_year_analysis/bmi_mgus_mm/',
           file.pattern = 'posterior_.*bz2',
           burn.in = max(burn.in),
           thin.factor = thin.factor,
           file.out = '../output/single_year_analysis/bmi_mgus_mm/posterior_pooled.csv')

post <- read.csv('../output/single_year_analysis/bmi_mgus_mm/pooled/posterior_pooled.csv.bz2')

# load the posterior distribution and specify the burn in period for the no bmi scenario 
get_convergence_stats(path.in = '../output/single_year_analysis/no_bmi/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = max(burn.in),
                      thin.factor = thin.factor)

pool_chain(path.in = '../output/single_year_analysis/no_bmi/',
           file.pattern = 'posterior_.*bz2',
           burn.in = max(burn.in),
           thin.factor = thin.factor,
           file.out = '../output/single_year_analysis/no_bmi/posterior_pooled.csv')

post_no_bmi <- read.csv('../output/single_year_analysis/no_bmi/posterior_pooled.csv.bz2')

# generate multivariate prior distribution for trend analysis 
fit <- mvnorm.mle(as.matrix(post))
fit.mu <- fit$mu
fit.sigma <- fit$sigma

# save multivariation prior distribution to RData structure
save(fit.mu, fit.sigma, file = '../output/fit_prior_trend.RData')

# calculate posterior predictions
preds <- calc_preds(post = post,
                    burn.in = 0:0,
                    num.samps = 50,
                    data_list = data_list,
                    ages = ages,
                    pop_male_nhw = pop_male_nhw,
                    pop_male_nhb = pop_male_nhb,
                    pop_female_nhw = pop_female_nhw,
                    pop_female_nhb = pop_female_nhb,
                    mort_male_nhw = mort_male_nhw,
                    mort_female_nhw = mort_female_nhw,
                    mort_male_nhb = mort_male_nhb,
                    mort_female_nhb = mort_female_nhb,
                    mgus_mortality_multiplier_male = mgus_mortality_multiplier_male,
                    mgus_mortality_multiplier_female = mgus_mortality_multiplier_female,
                    mu_mm_male_nhw = mm_mu_nhw_male,
                    mu_mm_female_nhw = mm_mu_nhw_female,
                    mu_mm_male_nhb = mm_mu_nhb_male,
                    mu_mm_female_nhb = mm_mu_nhb_female,
                    haz_male_nhw = haz_male_nhw,
                    haz_female_nhw = haz_female_nhw,
                    haz_male_nhb = haz_male_nhb,
                    haz_female_nhb = haz_female_nhb)

# calculate posterior predictions without effects of bmi 
haz_male_nhw$haz_mgus_bmi <- 1
haz_male_nhw$haz_mm_bmi <- 1
haz_female_nhw$haz_mgus_bmi <- 1
haz_female_nhw$haz_mm_bmi <- 1
haz_male_nhb$haz_mgus_bmi <- 1
haz_male_nhb$haz_mm_bmi <- 1
haz_female_nhb$haz_mgus_bmi <- 1
haz_female_nhb$haz_mm_bmi <- 1

preds_no_bmi <- calc_preds(post = post_no_bmi,
                    burn.in = 0:0,
                    num.samps = 50,
                    data_list = data_list,
                    ages = ages,
                    pop_male_nhw = pop_male_nhw,
                    pop_male_nhb = pop_male_nhb,
                    pop_female_nhw = pop_female_nhw,
                    pop_female_nhb = pop_female_nhb,
                    mort_male_nhw = mort_male_nhw,
                    mort_female_nhw = mort_female_nhw,
                    mort_male_nhb = mort_male_nhb,
                    mort_female_nhb = mort_female_nhb,
                    mgus_mortality_multiplier_male = mgus_mortality_multiplier_male,
                    mgus_mortality_multiplier_female = mgus_mortality_multiplier_female,
                    mu_mm_male_nhw = mm_mu_nhw_male,
                    mu_mm_female_nhw = mm_mu_nhw_female,
                    mu_mm_male_nhb = mm_mu_nhb_male,
                    mu_mm_female_nhb = mm_mu_nhb_female,
                    haz_male_nhw = haz_male_nhw,
                    haz_female_nhw = haz_female_nhw,
                    haz_male_nhb = haz_male_nhb,
                    haz_female_nhb = haz_female_nhb)

# calculate the 95% confidence intervals for the mgus prevalence data 
n_male_nhw <- data_list$mgus_prev_male_nhw$n
ns_male_nhw <- data_list$mgus_prev_male_nhw$y
nf_male_nhw <- n_male_nhw - ns_male_nhw
p_male_nhw_lower <- (ns_male_nhw / n_male_nhw) - (1.96 / (n_male_nhw * sqrt(n_male_nhw))) * sqrt(ns_male_nhw * nf_male_nhw)
p_male_nhw_upper <- (ns_male_nhw / n_male_nhw) + (1.96 / (n_male_nhw * sqrt(n_male_nhw))) * sqrt(ns_male_nhw * nf_male_nhw)

n_female_nhw <- data_list$mgus_prev_female_nhw$n
ns_female_nhw <- data_list$mgus_prev_female_nhw$y
nf_female_nhw <- n_female_nhw - ns_female_nhw
p_female_nhw_lower <- (ns_female_nhw / n_female_nhw) - (1.96 / (n_female_nhw * sqrt(n_female_nhw))) * sqrt(ns_female_nhw * nf_female_nhw)
p_female_nhw_upper <- (ns_female_nhw / n_female_nhw) + (1.96 / (n_female_nhw * sqrt(n_female_nhw))) * sqrt(ns_female_nhw * nf_female_nhw)

n_male_nhb <- data_list$mgus_prev_male_nhb$n
ns_male_nhb <- data_list$mgus_prev_male_nhb$y
nf_male_nhb <- n_male_nhb - ns_male_nhb
p_male_nhb_lower <- (ns_male_nhb / n_male_nhb) - (1.96 / (n_male_nhb * sqrt(n_male_nhb))) * sqrt(ns_male_nhb * nf_male_nhb)
p_male_nhb_upper <- (ns_male_nhb / n_male_nhb) + (1.96 / (n_male_nhb * sqrt(n_male_nhb))) * sqrt(ns_male_nhb * nf_male_nhb)

n_female_nhb <- data_list$mgus_prev_female_nhb$n
ns_female_nhb <- data_list$mgus_prev_female_nhb$y
nf_female_nhb <- n_female_nhb - ns_female_nhb
p_female_nhb_lower <- (ns_female_nhb / n_female_nhb) - (1.96 / (n_female_nhb * sqrt(n_female_nhb))) * sqrt(ns_female_nhb * nf_female_nhb)
p_female_nhb_upper <- (ns_female_nhb / n_female_nhb) + (1.96 / (n_female_nhb * sqrt(n_female_nhb))) * sqrt(ns_female_nhb * nf_female_nhb)

# save output to data frame 
save(list = c('preds',
              'preds_no_bmi',
              'data_list',
              'ages',
              'p_male_nhw_lower',
              'p_male_nhw_upper',
              'p_female_nhw_lower',
              'p_female_nhw_upper',
              'p_male_nhb_lower',
              'p_male_nhb_upper',
              'p_female_nhb_lower',
              'p_female_nhb_upper'),
     file = '../output/fig_1.RData')


sum((data_list$mgus_prev_male_nhw$y >= preds$y_male_nhw_ppi['2.5%',]) & (data_list$mgus_prev_male_nhw$y <= preds$y_male_nhw_ppi['97.5%',])) / ncol(preds$y_male_nhw_ppi)
sum((data_list$mgus_prev_female_nhw$y >= preds$y_female_nhw_ppi['2.5%',]) & (data_list$mgus_prev_female_nhw$y <= preds$y_female_nhw_ppi['97.5%',])) / ncol(preds$y_female_nhw_ppi)
sum((data_list$mgus_prev_male_nhb$y >= preds$y_male_nhb_ppi['2.5%',]) & (data_list$mgus_prev_male_nhb$y <= preds$y_male_nhb_ppi['97.5%',])) / ncol(preds$y_male_nhb_ppi)
sum((data_list$mgus_prev_female_nhb$y >= preds$y_female_nhb_ppi['2.5%',]) & (data_list$mgus_prev_female_nhb$y <= preds$y_female_nhb_ppi['97.5%',])) / ncol(preds$y_female_nhb_ppi)

sum((data_list$mm_incid_male_nhw$x >= preds$x_male_nhw_ppi['2.5%',]) & (data_list$mm_incid_male_nhw$x <= preds$x_male_nhw_ppi['97.5%',])) / ncol(preds$x_male_nhw_ppi)
sum((data_list$mm_incid_female_nhw$x >= preds$x_female_nhw_ppi['2.5%',]) & (data_list$mm_incid_female_nhw$x <= preds$x_female_nhw_ppi['97.5%',])) / ncol(preds$x_female_nhw_ppi)
sum((data_list$mm_incid_male_nhb$x >= preds$x_male_nhb_ppi['2.5%',]) & (data_list$mm_incid_male_nhb$x <= preds$x_male_nhb_ppi['97.5%',])) / ncol(preds$x_male_nhb_ppi)
sum((data_list$mm_incid_female_nhb$x >= preds$x_female_nhb_ppi['2.5%',]) & (data_list$mm_incid_female_nhb$x <= preds$x_female_nhb_ppi['97.5%',])) / ncol(preds$x_female_nhb_ppi)
