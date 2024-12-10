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

# load the posterior 
post <- read.csv('../output/single_year_analysis/bmi_mgus_mm/pooled/posterior_pooled.csv.bz2')

# generate predictions for mgus prevalence by bmi 
mgus_prev_male_nhw_uw <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_male_nhw,
                                        mort = mort_male_nhw,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                        mu_mm = mm_mu_nhw_male,
                                        haz = haz_uw,
                                        data_cohort = data_list$mgus_prev_male_nhw_uw,
                                        gender = 'male',
                                        race = 'nhw')
mgus_prev_male_nhw_nw <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_male_nhw,
                                        mort = mort_male_nhw,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                        mu_mm = mm_mu_nhw_male,
                                        haz = haz_nw,
                                        data_cohort = data_list$mgus_prev_male_nhw_nw,
                                        gender = 'male',
                                        race = 'nhw')
mgus_prev_male_nhw_ow <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_male_nhw,
                                        mort = mort_male_nhw,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                        mu_mm = mm_mu_nhw_male,
                                        haz = haz_ow,
                                        data_cohort = data_list$mgus_prev_male_nhw_ow,
                                        gender = 'male',
                                        race = 'nhw')
mgus_prev_male_nhw_ob <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_male_nhw,
                                        mort = mort_male_nhw,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                        mu_mm = mm_mu_nhw_male,
                                        haz = haz_ob,
                                        data_cohort = data_list$mgus_prev_male_nhw_ob,
                                        gender = 'male',
                                        race = 'nhw')

mgus_prev_female_nhw_uw <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_female_nhw,
                                        mort = mort_female_nhw,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                        mu_mm = mm_mu_nhw_female,
                                        haz = haz_uw,
                                        data_cohort = data_list$mgus_prev_female_nhw_uw,
                                        gender = 'female',
                                        race = 'nhw')
mgus_prev_female_nhw_nw <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_female_nhw,
                                        mort = mort_female_nhw,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                        mu_mm = mm_mu_nhw_female,
                                        haz = haz_nw,
                                        data_cohort = data_list$mgus_prev_female_nhw_nw,
                                        gender = 'female',
                                        race = 'nhw')
mgus_prev_female_nhw_ow <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_female_nhw,
                                        mort = mort_female_nhw,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                        mu_mm = mm_mu_nhw_female,
                                        haz = haz_ow,
                                        data_cohort = data_list$mgus_prev_female_nhw_ow,
                                        gender = 'female',
                                        race = 'nhw')
mgus_prev_female_nhw_ob <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_female_nhw,
                                        mort = mort_female_nhw,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                        mu_mm = mm_mu_nhw_female,
                                        haz = haz_ob,
                                        data_cohort = data_list$mgus_prev_female_nhw_ob,
                                        gender = 'female',
                                        race = 'nhw')


mgus_prev_male_nhb_uw <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_male_nhb,
                                        mort = mort_male_nhb,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                        mu_mm = mm_mu_nhb_male,
                                        haz = haz_uw,
                                        data_cohort = data_list$mgus_prev_male_nhb_uw,
                                        gender = 'male',
                                        race = 'nhb')
mgus_prev_male_nhb_nw <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_male_nhb,
                                        mort = mort_male_nhb,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                        mu_mm = mm_mu_nhb_male,
                                        haz = haz_nw,
                                        data_cohort = data_list$mgus_prev_male_nhb_nw,
                                        gender = 'male',
                                        race = 'nhb')
mgus_prev_male_nhb_ow <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_male_nhb,
                                        mort = mort_male_nhb,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                        mu_mm = mm_mu_nhb_male,
                                        haz = haz_ow,
                                        data_cohort = data_list$mgus_prev_male_nhb_ow,
                                        gender = 'male',
                                        race = 'nhb')
mgus_prev_male_nhb_ob <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_male_nhb,
                                        mort = mort_male_nhb,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                        mu_mm = mm_mu_nhb_male,
                                        haz = haz_ob,
                                        data_cohort = data_list$mgus_prev_male_nhb_ob,
                                        gender = 'male',
                                        race = 'nhb')

mgus_prev_female_nhb_uw <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_female_nhb,
                                        mort = mort_female_nhb,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                        mu_mm = mm_mu_nhb_female,
                                        haz = haz_uw,
                                        data_cohort = data_list$mgus_prev_female_nhb_uw,
                                        gender = 'female',
                                        race = 'nhb')
mgus_prev_female_nhb_nw <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_female_nhb,
                                        mort = mort_female_nhb,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                        mu_mm = mm_mu_nhb_female,
                                        haz = haz_nw,
                                        data_cohort = data_list$mgus_prev_female_nhb_nw,
                                        gender = 'female',
                                        race = 'nhb')
mgus_prev_female_nhb_ow <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_female_nhb,
                                        mort = mort_female_nhb,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                        mu_mm = mm_mu_nhb_female,
                                        haz = haz_ow,
                                        data_cohort = data_list$mgus_prev_female_nhb_ow,
                                        gender = 'female',
                                        race = 'nhb')
mgus_prev_female_nhb_ob <- pred_mgus_prev(post = post,
                                        burn.in = 0,
                                        num.samps = 500,
                                        ages = ages,
                                        pop = pop_female_nhb,
                                        mort = mort_female_nhb,
                                        mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                        mu_mm = mm_mu_nhb_female,
                                        haz = haz_ob,
                                        data_cohort = data_list$mgus_prev_female_nhb_ob,
                                        gender = 'female',
                                        race = 'nhb')


# aggregate across ages 
p_male_nhw_uw <- (mgus_prev_male_nhw_uw %*% data_list$mgus_prev_male_nhw_uw$n) / sum(data_list$mgus_prev_male_nhw_uw$n)
p_male_nhw_nw <- (mgus_prev_male_nhw_nw %*% data_list$mgus_prev_male_nhw_nw$n) / sum(data_list$mgus_prev_male_nhw_nw$n)
p_male_nhw_ow <- (mgus_prev_male_nhw_ow %*% data_list$mgus_prev_male_nhw_ow$n) / sum(data_list$mgus_prev_male_nhw_ow$n)
p_male_nhw_ob <- (mgus_prev_male_nhw_ob %*% data_list$mgus_prev_male_nhw_ob$n) / sum(data_list$mgus_prev_male_nhw_ob$n)

p_female_nhw_uw <- (mgus_prev_female_nhw_uw %*% data_list$mgus_prev_female_nhw_uw$n) / sum(data_list$mgus_prev_female_nhw_uw$n)
p_female_nhw_nw <- (mgus_prev_female_nhw_nw %*% data_list$mgus_prev_female_nhw_nw$n) / sum(data_list$mgus_prev_female_nhw_nw$n)
p_female_nhw_ow <- (mgus_prev_female_nhw_ow %*% data_list$mgus_prev_female_nhw_ow$n) / sum(data_list$mgus_prev_female_nhw_ow$n)
p_female_nhw_ob <- (mgus_prev_female_nhw_ob %*% data_list$mgus_prev_female_nhw_ob$n) / sum(data_list$mgus_prev_female_nhw_ob$n)

p_male_nhb_uw <- (mgus_prev_male_nhb_uw %*% data_list$mgus_prev_male_nhb_uw$n) / sum(data_list$mgus_prev_male_nhb_uw$n)
p_male_nhb_nw <- (mgus_prev_male_nhb_nw %*% data_list$mgus_prev_male_nhb_nw$n) / sum(data_list$mgus_prev_male_nhb_nw$n)
p_male_nhb_ow <- (mgus_prev_male_nhb_ow %*% data_list$mgus_prev_male_nhb_ow$n) / sum(data_list$mgus_prev_male_nhb_ow$n)
p_male_nhb_ob <- (mgus_prev_male_nhb_ob %*% data_list$mgus_prev_male_nhb_ob$n) / sum(data_list$mgus_prev_male_nhb_ob$n)

p_female_nhb_uw <- (mgus_prev_female_nhb_uw %*% data_list$mgus_prev_female_nhb_uw$n) / sum(data_list$mgus_prev_female_nhb_uw$n)
p_female_nhb_nw <- (mgus_prev_female_nhb_nw %*% data_list$mgus_prev_female_nhb_nw$n) / sum(data_list$mgus_prev_female_nhb_nw$n)
p_female_nhb_ow <- (mgus_prev_female_nhb_ow %*% data_list$mgus_prev_female_nhb_ow$n) / sum(data_list$mgus_prev_female_nhb_ow$n)
p_female_nhb_ob <- (mgus_prev_female_nhb_ob %*% data_list$mgus_prev_female_nhb_ob$n) / sum(data_list$mgus_prev_female_nhb_ob$n)

n_uw <- sum(data_list$mgus_prev_male_nhw_uw$n) + sum(data_list$mgus_prev_female_nhw_uw$n) + sum(data_list$mgus_prev_male_nhb_uw$n) + sum(data_list$mgus_prev_female_nhb_uw$n)
n_nw <- sum(data_list$mgus_prev_male_nhw_nw$n) + sum(data_list$mgus_prev_female_nhw_nw$n) + sum(data_list$mgus_prev_male_nhb_nw$n) + sum(data_list$mgus_prev_female_nhb_nw$n)
n_ow <- sum(data_list$mgus_prev_male_nhw_ow$n) + sum(data_list$mgus_prev_female_nhw_ow$n) + sum(data_list$mgus_prev_male_nhb_ow$n) + sum(data_list$mgus_prev_female_nhb_ow$n)
n_ob <- sum(data_list$mgus_prev_male_nhw_ob$n) + sum(data_list$mgus_prev_female_nhw_ob$n) + sum(data_list$mgus_prev_male_nhb_ob$n) + sum(data_list$mgus_prev_female_nhb_ob$n)

p_uw <- ((mgus_prev_male_nhw_uw %*% data_list$mgus_prev_male_nhw_uw$n) + (mgus_prev_female_nhw_uw %*% data_list$mgus_prev_female_nhw_uw$n) + 
  (mgus_prev_male_nhb_uw %*% data_list$mgus_prev_male_nhb_uw$n) + (mgus_prev_female_nhb_uw %*% data_list$mgus_prev_female_nhb_uw$n)) / n_uw

p_nw <- ((mgus_prev_male_nhw_nw %*% data_list$mgus_prev_male_nhw_nw$n) + (mgus_prev_female_nhw_nw %*% data_list$mgus_prev_female_nhw_nw$n) + 
           (mgus_prev_male_nhb_nw %*% data_list$mgus_prev_male_nhb_nw$n) + (mgus_prev_female_nhb_nw %*% data_list$mgus_prev_female_nhb_nw$n)) / n_nw

p_ow <- ((mgus_prev_male_nhw_ow %*% data_list$mgus_prev_male_nhw_ow$n) + (mgus_prev_female_nhw_ow %*% data_list$mgus_prev_female_nhw_ow$n) + 
           (mgus_prev_male_nhb_ow %*% data_list$mgus_prev_male_nhb_ow$n) + (mgus_prev_female_nhb_ow %*% data_list$mgus_prev_female_nhb_ow$n)) / n_ow

p_ob <- ((mgus_prev_male_nhw_ob %*% data_list$mgus_prev_male_nhw_ob$n) + (mgus_prev_female_nhw_ob %*% data_list$mgus_prev_female_nhw_ob$n) + 
           (mgus_prev_male_nhb_ob %*% data_list$mgus_prev_male_nhb_ob$n) + (mgus_prev_female_nhb_ob %*% data_list$mgus_prev_female_nhb_ob$n)) / n_ob

# generate draws from the posterior prediction 
num.samps <- 500
y_male_nhw_uw <- sapply(1:nrow(p_male_nhw_uw), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_male_nhw_uw$n), prob = p_male_nhw_uw[i,1])}) /
  sum(data_list$mgus_prev_male_nhw_uw$n)
y_male_nhw_nw <- sapply(1:nrow(p_male_nhw_nw), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_male_nhw_nw$n), prob = p_male_nhw_nw[i,1])}) /
  sum(data_list$mgus_prev_male_nhw_nw$n)
y_male_nhw_ow <- sapply(1:nrow(p_male_nhw_ow), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_male_nhw_ow$n), prob = p_male_nhw_ow[i,1])}) /
  sum(data_list$mgus_prev_male_nhw_ow$n)
y_male_nhw_ob <- sapply(1:nrow(p_male_nhw_ob), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_male_nhw_ob$n), prob = p_male_nhw_ob[i,1])}) /
  sum(data_list$mgus_prev_male_nhw_ob$n)

y_female_nhw_uw <- sapply(1:nrow(p_female_nhw_uw), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_female_nhw_uw$n), prob = p_female_nhw_uw[i,1])}) /
  sum(data_list$mgus_prev_female_nhw_uw$n)
y_female_nhw_nw <- sapply(1:nrow(p_female_nhw_nw), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_female_nhw_nw$n), prob = p_female_nhw_nw[i,1])}) /
  sum(data_list$mgus_prev_female_nhw_nw$n)
y_female_nhw_ow <- sapply(1:nrow(p_female_nhw_ow), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_female_nhw_ow$n), prob = p_female_nhw_ow[i,1])}) /
  sum(data_list$mgus_prev_female_nhw_ow$n)
y_female_nhw_ob <- sapply(1:nrow(p_female_nhw_ob), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_female_nhw_ob$n), prob = p_female_nhw_ob[i,1])}) /
  sum(data_list$mgus_prev_female_nhw_ob$n)

y_male_nhb_uw <- sapply(1:nrow(p_male_nhb_uw), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_male_nhb_uw$n), prob = p_male_nhb_uw[i,1])}) /
  sum(data_list$mgus_prev_male_nhb_uw$n)
y_male_nhb_nw <- sapply(1:nrow(p_male_nhb_nw), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_male_nhb_nw$n), prob = p_male_nhb_nw[i,1])}) /
  sum(data_list$mgus_prev_male_nhb_nw$n)
y_male_nhb_ow <- sapply(1:nrow(p_male_nhb_ow), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_male_nhb_ow$n), prob = p_male_nhb_ow[i,1])}) /
  sum(data_list$mgus_prev_male_nhb_ow$n)
y_male_nhb_ob <- sapply(1:nrow(p_male_nhb_ob), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_male_nhb_ob$n), prob = p_male_nhb_ob[i,1])}) /
  sum(data_list$mgus_prev_male_nhb_ob$n)

y_female_nhb_uw <- sapply(1:nrow(p_female_nhb_uw), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_female_nhb_uw$n), prob = p_female_nhb_uw[i,1])}) /
  sum(data_list$mgus_prev_female_nhb_uw$n)
y_female_nhb_nw <- sapply(1:nrow(p_female_nhb_nw), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_female_nhb_nw$n), prob = p_female_nhb_nw[i,1])}) /
  sum(data_list$mgus_prev_female_nhb_nw$n)
y_female_nhb_ow <- sapply(1:nrow(p_female_nhb_ow), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_female_nhb_ow$n), prob = p_female_nhb_ow[i,1])}) /
  sum(data_list$mgus_prev_female_nhb_ow$n)
y_female_nhb_ob <- sapply(1:nrow(p_female_nhb_ob), function(i){rbinom(n = num.samps, size = sum(data_list$mgus_prev_female_nhb_ob$n), prob = p_female_nhb_ob[i,1])}) /
  sum(data_list$mgus_prev_female_nhb_ob$n)

y_uw <- sapply(1:nrow(p_uw), function(i){rbinom(n = num.samps, size = n_uw, prob = p_uw[i,1])}) / n_uw
y_nw <- sapply(1:nrow(p_nw), function(i){rbinom(n = num.samps, size = n_nw, prob = p_nw[i,1])}) / n_nw
y_ow <- sapply(1:nrow(p_ow), function(i){rbinom(n = num.samps, size = n_ow, prob = p_ow[i,1])}) / n_ow
y_ob <- sapply(1:nrow(p_ob), function(i){rbinom(n = num.samps, size = n_ob, prob = p_ob[i,1])}) / n_ob

# calculate the PPI 
y_male_nhw_uw_ppi <- quantile(c(y_male_nhw_uw), probs = c(0.025,0.50,0.975))
y_male_nhw_nw_ppi <- quantile(c(y_male_nhw_nw), probs = c(0.025,0.50,0.975))
y_male_nhw_ow_ppi <- quantile(c(y_male_nhw_ow), probs = c(0.025,0.50,0.975))
y_male_nhw_ob_ppi <- quantile(c(y_male_nhw_ob), probs = c(0.025,0.50,0.975))

y_female_nhw_uw_ppi <- quantile(c(y_female_nhw_uw), probs = c(0.025,0.50,0.975))
y_female_nhw_nw_ppi <- quantile(c(y_female_nhw_nw), probs = c(0.025,0.50,0.975))
y_female_nhw_ow_ppi <- quantile(c(y_female_nhw_ow), probs = c(0.025,0.50,0.975))
y_female_nhw_ob_ppi <- quantile(c(y_female_nhw_ob), probs = c(0.025,0.50,0.975))

y_male_nhb_uw_ppi <- quantile(c(y_male_nhb_uw), probs = c(0.025,0.50,0.975))
y_male_nhb_nw_ppi <- quantile(c(y_male_nhb_nw), probs = c(0.025,0.50,0.975))
y_male_nhb_ow_ppi <- quantile(c(y_male_nhb_ow), probs = c(0.025,0.50,0.975))
y_male_nhb_ob_ppi <- quantile(c(y_male_nhb_ob), probs = c(0.025,0.50,0.975))

y_female_nhb_uw_ppi <- quantile(c(y_female_nhb_uw), probs = c(0.025,0.50,0.975))
y_female_nhb_nw_ppi <- quantile(c(y_female_nhb_nw), probs = c(0.025,0.50,0.975))
y_female_nhb_ow_ppi <- quantile(c(y_female_nhb_ow), probs = c(0.025,0.50,0.975))
y_female_nhb_ob_ppi <- quantile(c(y_female_nhb_ob), probs = c(0.025,0.50,0.975))

y_uw_ppi <- quantile(c(y_uw), probs = c(0.025,0.50,0.975))
y_nw_ppi <- quantile(c(y_nw), probs = c(0.025,0.50,0.975))
y_ow_ppi <- quantile(c(y_ow), probs = c(0.025,0.50,0.975))
y_ob_ppi <- quantile(c(y_ob), probs = c(0.025,0.50,0.975))

# save output to file
save(y_male_nhw_uw_ppi,
     y_male_nhw_nw_ppi,
     y_male_nhw_ow_ppi,
     y_male_nhw_ob_ppi,
     y_female_nhw_uw_ppi,
     y_female_nhw_nw_ppi,
     y_female_nhw_ow_ppi,
     y_female_nhw_ob_ppi,
     y_male_nhb_uw_ppi,
     y_male_nhb_nw_ppi,
     y_male_nhb_ow_ppi,
     y_male_nhb_ob_ppi,
     y_female_nhb_uw_ppi,
     y_female_nhb_nw_ppi,
     y_female_nhb_ow_ppi,
     y_female_nhb_ob_ppi,
     y_uw_ppi,
     y_nw_ppi,
     y_ow_ppi,
     y_ob_ppi,
     file = '../output/fig_S2.RData')
