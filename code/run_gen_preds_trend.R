# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_convergence.R')
source('functions_likelihood.R')
source('functions_validate_inference.R')

# load the data
load('../output/data_organized.RData')

# specify the number of knots and path in
n_knots_period <- 3
n_knots_cohort <- 6
path.in = '../output/trend_analysis/bmi_spline_0915/'
path.out = path.in

# load the posterior
post <- read.csv(paste(path.in, 'posterior_pooled_', n_knots_period, '_', n_knots_cohort, '.csv', sep = ''))

# load the posterior from fitting to the 2004 data and use median estimates 
post_prior <- read.csv('../output/single_year_analysis/bmi_mgus_mm/pooled/posterior_pooled.csv.bz2')
post_prior_median <- apply(post_prior,2,median)

gamma_mgus = as.numeric(post_prior_median[1])
beta_mgus_age = as.numeric(post_prior_median[2])
beta_mgus_sex = as.numeric(post_prior_median[3])
beta_mgus_race = as.numeric(post_prior_median[4])

gamma_mm = as.numeric(post_prior_median[5])
beta_mm_age = as.numeric(post_prior_median[6])
beta_mm_age_quad = as.numeric(post_prior_median[7])
beta_mm_sex = as.numeric(post_prior_median[8])
beta_mm_race = as.numeric(post_prior_median[9])

# simulate model
out_male_nhw <- sim_model(ages = ages,
                          gamma_mgus = gamma_mgus,
                          beta_mgus_age = beta_mgus_age,
                          beta_mgus_age_quad = 0,
                          beta_mgus_sex = 0,
                          beta_mgus_race = 0,
                          gamma_mm = gamma_mm,
                          beta_mm_age = beta_mm_age,
                          beta_mm_age_quad = beta_mm_age_quad,
                          beta_mm_sex = 0,
                          beta_mm_race = 0,
                          mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                          mort = mort_male_nhw,
                          mu_mm = mm_mu_nhw_male,
                          haz = haz_male_nhw)

out_male_nhb <- sim_model(ages = ages,
                          gamma_mgus = gamma_mgus,
                          beta_mgus_age = beta_mgus_age,
                          beta_mgus_age_quad = 0,
                          beta_mgus_sex = 0,
                          beta_mgus_race = beta_mgus_race,
                          gamma_mm = gamma_mm,
                          beta_mm_age = beta_mm_age,
                          beta_mm_age_quad = beta_mm_age_quad,
                          beta_mm_sex = 0,
                          beta_mm_race = beta_mm_race,
                          mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                          mort = mort_male_nhb,
                          mu_mm = mm_mu_nhb_male,
                          haz = haz_male_nhb)

out_female_nhw <- sim_model(ages = ages,
                            gamma_mgus = gamma_mgus,
                            beta_mgus_age = beta_mgus_age,
                            beta_mgus_age_quad = 0,
                            beta_mgus_sex = beta_mgus_sex,
                            beta_mgus_race = 0,
                            gamma_mm = gamma_mm,
                            beta_mm_age = beta_mm_age,
                            beta_mm_age_quad = beta_mm_age_quad,
                            beta_mm_sex = beta_mm_sex,
                            beta_mm_race = 0,
                            mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                            mort = mort_female_nhw,
                            mu_mm = mm_mu_nhw_female,
                            haz = haz_female_nhw)

out_female_nhb <- sim_model(ages = ages,
                            gamma_mgus = gamma_mgus,
                            beta_mgus_age = beta_mgus_age,
                            beta_mgus_age_quad = 0,
                            beta_mgus_sex = beta_mgus_sex,
                            beta_mgus_race = beta_mgus_race,
                            gamma_mm = gamma_mm,
                            beta_mm_age = beta_mm_age,
                            beta_mm_age_quad = beta_mm_age_quad,
                            beta_mm_sex = beta_mm_sex,
                            beta_mm_race = beta_mm_race,
                            mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                            mort = mort_female_nhb,
                            mu_mm = mm_mu_nhb_female,
                            haz = haz_female_nhb)

# specify knots for spline and reference years
data_reference_year = 2004
period_reference_year = 2000
cohort_reference_year = 1920
diagnosis_reference_year = 2013

knots_x_period <- seq(from = min(data_list$mm_trend_incid_male_nhw$year) - period_reference_year, 
                      to = max(data_list$mm_trend_incid_male_nhw$year) - period_reference_year,
                      length.out = n_knots_period)
knots_x_period[which.min(abs(knots_x_period))] <- 0
knots_x_period <- knots_x_period + period_reference_year

knots_x_cohort <- seq(from = min(data_list$mm_trend_incid_male_nhw$year) - max(data_list$mm_trend_incid_male_nhw$age_upper) - cohort_reference_year,
                      to = max(data_list$mm_trend_incid_male_nhw$year) - min(data_list$mm_trend_incid_male_nhw$age_lower) - cohort_reference_year,
                      length.out = n_knots_cohort)
knots_x_cohort[which.min(abs(knots_x_cohort))] <- 0
knots_x_cohort <- knots_x_cohort + cohort_reference_year

# generate predictions
preds <- calc_preds_trend(post = post,
                          burn.in = 0:0,
                          num.samps = 500,
                          data_list = data_list,
                          knots_x_period = knots_x_period,
                          knots_x_cohort = knots_x_cohort,
                          haz_trend_male_nhw_list = haz_trend_male_nhw_list,
                          haz_trend_female_nhw_list = haz_trend_female_nhw_list,
                          haz_trend_male_nhb_list = haz_trend_male_nhb_list,
                          haz_trend_female_nhb_list = haz_trend_female_nhb_list,
                          out_male_nhw = out_male_nhw,
                          out_female_nhw = out_female_nhw,
                          out_male_nhb = out_male_nhb,
                          out_female_nhb = out_female_nhb,
                          ages = ages,
                          pop_trend_male_nhw = pop_trend_male_nhw,
                          pop_trend_female_nhw = pop_trend_female_nhw,
                          pop_trend_male_nhb = pop_trend_male_nhb,
                          pop_trend_female_nhb = pop_trend_female_nhb,
                          weights_by_age_group_male_nhw = weights_by_age_group_male_nhw,
                          weights_by_age_group_female_nhw = weights_by_age_group_female_nhw,
                          weights_by_age_group_male_nhb = weights_by_age_group_male_nhb,
                          weights_by_age_group_female_nhb = weights_by_age_group_female_nhb)

# generate predictions for out-of-sample data
preds_test <- calc_preds_trend(post = post,
                          burn.in = 0:0,
                          num.samps = 500,
                          data_list = data_list_test,
                          knots_x_period = knots_x_period,
                          knots_x_cohort = knots_x_cohort,
                          haz_trend_male_nhw_list = haz_trend_male_nhw_list,
                          haz_trend_female_nhw_list = haz_trend_female_nhw_list,
                          haz_trend_male_nhb_list = haz_trend_male_nhb_list,
                          haz_trend_female_nhb_list = haz_trend_female_nhb_list,
                          out_male_nhw = out_male_nhw,
                          out_female_nhw = out_female_nhw,
                          out_male_nhb = out_male_nhb,
                          out_female_nhb = out_female_nhb,
                          ages = ages,
                          pop_trend_male_nhw = pop_trend_male_nhw,
                          pop_trend_female_nhw = pop_trend_female_nhw,
                          pop_trend_male_nhb = pop_trend_male_nhb,
                          pop_trend_female_nhb = pop_trend_female_nhb,
                          weights_by_age_group_male_nhw = weights_by_age_group_male_nhw_test,
                          weights_by_age_group_female_nhw = weights_by_age_group_female_nhw_test,
                          weights_by_age_group_male_nhb = weights_by_age_group_male_nhb_test,
                          weights_by_age_group_female_nhb = weights_by_age_group_female_nhb_test)

# generate predictions for all data
preds_combined <- calc_preds_trend(post = post,
                               burn.in = 0:0,
                               num.samps = 500,
                               data_list = data_list_combined,
                               knots_x_period = knots_x_period,
                               knots_x_cohort = knots_x_cohort,
                               haz_trend_male_nhw_list = haz_trend_male_nhw_list,
                               haz_trend_female_nhw_list = haz_trend_female_nhw_list,
                               haz_trend_male_nhb_list = haz_trend_male_nhb_list,
                               haz_trend_female_nhb_list = haz_trend_female_nhb_list,
                               out_male_nhw = out_male_nhw,
                               out_female_nhw = out_female_nhw,
                               out_male_nhb = out_male_nhb,
                               out_female_nhb = out_female_nhb,
                               ages = ages,
                               pop_trend_male_nhw = pop_trend_male_nhw,
                               pop_trend_female_nhw = pop_trend_female_nhw,
                               pop_trend_male_nhb = pop_trend_male_nhb,
                               pop_trend_female_nhb = pop_trend_female_nhb,
                               weights_by_age_group_male_nhw = weights_by_age_group_male_nhw_combined,
                               weights_by_age_group_female_nhw = weights_by_age_group_female_nhw_combined,
                               weights_by_age_group_male_nhb = weights_by_age_group_male_nhb_combined,
                               weights_by_age_group_female_nhb = weights_by_age_group_female_nhb_combined)

# calculate the coverage probabilities 
signif(sum(data_list$mm_trend_incid_male_nhw$c >= preds$x_male_nhw_ppi['2.5%',] & data_list$mm_trend_incid_male_nhw$c <= preds$x_male_nhw_ppi['97.5%',]) / length(data_list$mm_trend_incid_male_nhw$c), digits = 2)
signif(sum(data_list$mm_trend_incid_female_nhw$c >= preds$x_female_nhw_ppi['2.5%',] & data_list$mm_trend_incid_female_nhw$c <= preds$x_female_nhw_ppi['97.5%',]) / length(data_list$mm_trend_incid_female_nhw$c), digits = 2)
signif(sum(data_list$mm_trend_incid_male_nhb$c >= preds$x_male_nhb_ppi['2.5%',] & data_list$mm_trend_incid_male_nhb$c <= preds$x_male_nhb_ppi['97.5%',]) / length(data_list$mm_trend_incid_male_nhb$c), digits = 2)
signif(sum(data_list$mm_trend_incid_female_nhb$c >= preds$x_female_nhb_ppi['2.5%',] & data_list$mm_trend_incid_female_nhb$c <= preds$x_female_nhb_ppi['97.5%',]) / length(data_list$mm_trend_incid_female_nhb$c), digits = 2)

signif(sum(data_list_test$mm_trend_incid_male_nhw$c >= preds_test$x_male_nhw_ppi['2.5%',] & data_list_test$mm_trend_incid_male_nhw$c <= preds_test$x_male_nhw_ppi['97.5%',]) / length(data_list_test$mm_trend_incid_male_nhw$c), digits = 2)
signif(sum(data_list_test$mm_trend_incid_female_nhw$c >= preds_test$x_female_nhw_ppi['2.5%',] & data_list_test$mm_trend_incid_female_nhw$c <= preds_test$x_female_nhw_ppi['97.5%',]) / length(data_list_test$mm_trend_incid_female_nhw$c), digits = 2)
signif(sum(data_list_test$mm_trend_incid_male_nhb$c >= preds_test$x_male_nhb_ppi['2.5%',] & data_list_test$mm_trend_incid_male_nhb$c <= preds_test$x_male_nhb_ppi['97.5%',]) / length(data_list_test$mm_trend_incid_male_nhb$c), digits = 2)
signif(sum(data_list_test$mm_trend_incid_female_nhb$c >= preds_test$x_female_nhb_ppi['2.5%',] & data_list_test$mm_trend_incid_female_nhb$c <= preds_test$x_female_nhb_ppi['97.5%',]) / length(data_list_test$mm_trend_incid_female_nhb$c), digits = 2)

signif(sum(data_list_combined$mm_trend_incid_male_nhw$c >= preds_combined$x_male_nhw_ppi['2.5%',] & data_list_combined$mm_trend_incid_male_nhw$c <= preds_combined$x_male_nhw_ppi['97.5%',]) / length(data_list_combined$mm_trend_incid_male_nhw$c), digits = 2)
signif(sum(data_list_combined$mm_trend_incid_female_nhw$c >= preds_combined$x_female_nhw_ppi['2.5%',] & data_list_combined$mm_trend_incid_female_nhw$c <= preds_combined$x_female_nhw_ppi['97.5%',]) / length(data_list_combined$mm_trend_incid_female_nhw$c), digits = 2)
signif(sum(data_list_combined$mm_trend_incid_male_nhb$c >= preds_combined$x_male_nhb_ppi['2.5%',] & data_list_combined$mm_trend_incid_male_nhb$c <= preds_combined$x_male_nhb_ppi['97.5%',]) / length(data_list_combined$mm_trend_incid_male_nhb$c), digits = 2)
signif(sum(data_list_combined$mm_trend_incid_female_nhb$c >= preds_combined$x_female_nhb_ppi['2.5%',] & data_list_combined$mm_trend_incid_female_nhb$c <= preds_combined$x_female_nhb_ppi['97.5%',]) / length(data_list_combined$mm_trend_incid_female_nhb$c), digits = 2)

# calculate the percentage increase in MM incidence over the 44 year period for 50-54 and 80-84 year age groups 
indices_50_54 <- which(data_list$mm_trend_incid_male_nhw$age_lower == 50)
indices_50_54 <- c(head(indices_50_54, n = 1), tail(indices_50_54, n = 1))

indices_80_84 <- which(data_list$mm_trend_incid_male_nhw$age_lower == 80)
indices_80_84 <- c(head(indices_80_84, n = 1), tail(indices_80_84, n = 1))


increase_nhw_male_50_54 <- quantile(c(preds$x_pred_male_nhw[,indices_50_54[2],] / data_list$mm_trend_incid_male_nhw$n[indices_50_54[2]]) / 
  c(preds$x_pred_male_nhw[,indices_50_54[1],] / data_list$mm_trend_incid_male_nhw$n[indices_50_54[1]]), probs = c(0.025,0.50,0.975))
increase_nhw_male_80_84 <- quantile(c(preds$x_pred_male_nhw[,indices_80_84[2],] / data_list$mm_trend_incid_male_nhw$n[indices_80_84[2]]) / 
                                    c(preds$x_pred_male_nhw[,indices_80_84[1],] / data_list$mm_trend_incid_male_nhw$n[indices_80_84[1]]), probs = c(0.025,0.50,0.975))

increase_nhw_female_50_54 <- quantile(c(preds$x_pred_female_nhw[,indices_50_54[2],] / data_list$mm_trend_incid_female_nhw$n[indices_50_54[2]]) / 
                                      c(preds$x_pred_female_nhw[,indices_50_54[1],] / data_list$mm_trend_incid_female_nhw$n[indices_50_54[1]]), probs = c(0.025,0.50,0.975))
increase_nhw_female_80_84 <- quantile(c(preds$x_pred_female_nhw[,indices_80_84[2],] / data_list$mm_trend_incid_female_nhw$n[indices_80_84[2]]) / 
                                      c(preds$x_pred_female_nhw[,indices_80_84[1],] / data_list$mm_trend_incid_female_nhw$n[indices_80_84[1]]), probs = c(0.025,0.50,0.975))

increase_nhb_male_50_54 <- quantile(c(preds$x_pred_male_nhb[,indices_50_54[2],] / data_list$mm_trend_incid_male_nhb$n[indices_50_54[2]]) / 
                                      c(preds$x_pred_male_nhb[,indices_50_54[1],] / data_list$mm_trend_incid_male_nhb$n[indices_50_54[1]]), probs = c(0.025,0.50,0.975))
increase_nhb_male_80_84 <- quantile(c(preds$x_pred_male_nhb[,indices_80_84[2],] / data_list$mm_trend_incid_male_nhb$n[indices_80_84[2]]) / 
                                      c(preds$x_pred_male_nhb[,indices_80_84[1],] / data_list$mm_trend_incid_male_nhb$n[indices_80_84[1]]), probs = c(0.025,0.50,0.975))

increase_nhb_female_50_54 <- quantile(c(preds$x_pred_female_nhb[,indices_50_54[2],] / data_list$mm_trend_incid_female_nhb$n[indices_50_54[2]]) / 
                                        c(preds$x_pred_female_nhb[,indices_50_54[1],] / data_list$mm_trend_incid_female_nhb$n[indices_50_54[1]]), probs = c(0.025,0.50,0.975))
increase_nhb_female_80_84 <- quantile(c(preds$x_pred_female_nhb[,indices_80_84[2],] / data_list$mm_trend_incid_female_nhb$n[indices_80_84[2]]) / 
                                        c(preds$x_pred_female_nhb[,indices_80_84[1],] / data_list$mm_trend_incid_female_nhb$n[indices_80_84[1]]), probs = c(0.025,0.50,0.975))


# save output to file
preds$x_pred_male_nhw <- NULL
preds$x_pred_male_nhb <- NULL
preds$x_pred_female_nhw <- NULL
preds$x_pred_female_nhb <- NULL

preds_combined$x_pred_male_nhw <- NULL
preds_combined$x_pred_male_nhb <- NULL
preds_combined$x_pred_female_nhw <- NULL
preds_combined$x_pred_female_nhb <- NULL

preds_test$x_pred_male_nhw <- NULL
preds_test$x_pred_male_nhb <- NULL
preds_test$x_pred_female_nhw <- NULL
preds_test$x_pred_female_nhb <- NULL

save(preds_combined, preds_test, preds, file = paste(path.out, 'preds_', n_knots_period, '_', n_knots_cohort, '.RData', sep = ''))
