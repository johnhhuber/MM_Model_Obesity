# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# load necessary data
load('../output/trend_analysis/bmi_spline_new/preds_3_6.RData')
load('../output/data_organized.RData')

# specify the lower bound of the ages 
age_lower <- unique(data_list$mm_trend_incid_male_nhw$age_lower)

# calculate the coverage probability for in-sample predictions 
signif(sum((data_list$mm_trend_incid_male_nhw$c >= preds$x_male_nhw_ppi['2.5%',]) & (data_list$mm_trend_incid_male_nhw$c <= preds$x_male_nhw_ppi['97.5%',])) / ncol(preds$x_male_nhw_ppi), digits = 2)
signif(sum((data_list$mm_trend_incid_female_nhw$c >= preds$x_female_nhw_ppi['2.5%',]) & (data_list$mm_trend_incid_female_nhw$c <= preds$x_female_nhw_ppi['97.5%',])) / ncol(preds$x_female_nhw_ppi), digits = 2)
signif(sum((data_list$mm_trend_incid_male_nhb$c >= preds$x_male_nhb_ppi['2.5%',]) & (data_list$mm_trend_incid_male_nhb$c <= preds$x_male_nhb_ppi['97.5%',])) / ncol(preds$x_male_nhb_ppi), digits = 2)
signif(sum((data_list$mm_trend_incid_female_nhb$c >= preds$x_female_nhb_ppi['2.5%',]) & (data_list$mm_trend_incid_female_nhb$c <= preds$x_female_nhb_ppi['97.5%',])) / ncol(preds$x_female_nhb_ppi), digits = 2)

# calculate the coverage probability for out-of-sample predictions 
signif(sum((data_list_test$mm_trend_incid_male_nhw$c >= preds_test$x_male_nhw_ppi['2.5%',]) & (data_list_test$mm_trend_incid_male_nhw$c <= preds_test$x_male_nhw_ppi['97.5%',])) / ncol(preds_test$x_male_nhw_ppi), digits = 2)
signif(sum((data_list_test$mm_trend_incid_female_nhw$c >= preds_test$x_female_nhw_ppi['2.5%',]) & (data_list_test$mm_trend_incid_female_nhw$c <= preds_test$x_female_nhw_ppi['97.5%',])) / ncol(preds_test$x_female_nhw_ppi), digits = 2)
signif(sum((data_list_test$mm_trend_incid_male_nhb$c >= preds_test$x_male_nhb_ppi['2.5%',]) & (data_list_test$mm_trend_incid_male_nhb$c <= preds_test$x_male_nhb_ppi['97.5%',])) / ncol(preds_test$x_male_nhb_ppi), digits = 2)
signif(sum((data_list_test$mm_trend_incid_female_nhb$c >= preds_test$x_female_nhb_ppi['2.5%',]) & (data_list_test$mm_trend_incid_female_nhb$c <= preds_test$x_female_nhb_ppi['97.5%',])) / ncol(preds_test$x_female_nhb_ppi), digits = 2)
