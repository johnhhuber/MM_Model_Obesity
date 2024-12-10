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

# load posterior 
post <- read.csv('../output/single_year_analysis/bmi_mgus_mm/posterior_pooled.csv')

# create data list for each bmi 
data_list_uw <- data_list
data_list_uw$mgus_prev_male_nhw <- data_list_uw$mgus_prev_male_nhw_uw
data_list_uw$mgus_prev_female_nhw <- data_list_uw$mgus_prev_female_nhw_uw
data_list_uw$mgus_prev_male_nhb <- data_list_uw$mgus_prev_male_nhb_uw
data_list_uw$mgus_prev_female_nhb <- data_list_uw$mgus_prev_female_nhb_uw

data_list_nw <- data_list
data_list_nw$mgus_prev_male_nhw <- data_list_nw$mgus_prev_male_nhw_nw
data_list_nw$mgus_prev_female_nhw <- data_list_nw$mgus_prev_female_nhw_nw
data_list_nw$mgus_prev_male_nhb <- data_list_nw$mgus_prev_male_nhb_nw
data_list_nw$mgus_prev_female_nhb <- data_list_nw$mgus_prev_female_nhb_nw

data_list_ow <- data_list
data_list_ow$mgus_prev_male_nhw <- data_list_ow$mgus_prev_male_nhw_ow
data_list_ow$mgus_prev_female_nhw <- data_list_ow$mgus_prev_female_nhw_ow
data_list_ow$mgus_prev_male_nhb <- data_list_ow$mgus_prev_male_nhb_ow
data_list_ow$mgus_prev_female_nhb <- data_list_ow$mgus_prev_female_nhb_ow

data_list_ob <- data_list
data_list_ob$mgus_prev_male_nhw <- data_list_ob$mgus_prev_male_nhw_ob
data_list_ob$mgus_prev_female_nhw <- data_list_ob$mgus_prev_female_nhw_ob
data_list_ob$mgus_prev_male_nhb <- data_list_ob$mgus_prev_male_nhb_ob
data_list_ob$mgus_prev_female_nhb <- data_list_ob$mgus_prev_female_nhb_ob


# calculate posterior predictions
preds_uw <- calc_preds(post = post,
                       burn.in = 0:0,
                       num.samps = 75,
                       data_list = data_list_uw,
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
                       haz_male_nhw = haz_uw,
                       haz_female_nhw = haz_uw,
                       haz_male_nhb = haz_uw,
                       haz_female_nhb = haz_uw)


preds_nw <- calc_preds(post = post,
                       burn.in = 0:0,
                       num.samps = 75,
                       data_list = data_list_nw,
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
                       haz_male_nhw = haz_nw,
                       haz_female_nhw = haz_nw,
                       haz_male_nhb = haz_nw,
                       haz_female_nhb = haz_nw)

preds_ow <- calc_preds(post = post,
                       burn.in = 0:0,
                       num.samps = 75,
                       data_list = data_list_ow,
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
                       haz_male_nhw = haz_ow,
                       haz_female_nhw = haz_ow,
                       haz_male_nhb = haz_ow,
                       haz_female_nhb = haz_ow)

preds_ob <- calc_preds(post = post,
                       burn.in = 0:0,
                       num.samps = 75,
                       data_list = data_list_ob,
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
                       haz_male_nhw = haz_ob,
                       haz_female_nhw = haz_ob,
                       haz_male_nhb = haz_ob,
                       haz_female_nhb = haz_ob)

# save output to file 
load('../output/fig_1.RData')
save(ages, preds, preds_uw, preds_nw, preds_ow, preds_ob,
     file = '../output/fig_2.RData')
