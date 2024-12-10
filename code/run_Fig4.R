# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(infotheo)){install.packages('infotheo'); library(infotheo)}

# load necessary functions
source('functions_likelihood.R')

# Defining APC Mutual Information score function
score <- function(df){
  num_bins <- min(length(unique(df$age_lower)), length(unique(df$period)))
  df <- discretize(df, disc = 'equalwidth', nbins = num_bins)
  rinfo <- entropy(df$rate)  # self info
  remaining <- condentropy(df$rate, data.frame(df$age_effect, df$period_effect, df$cohort_effect, df$bmi_effect))  # Info after conditioning
  
  a_mi <- mutinformation(df$rate, df$age_effect)
  p_mi <- mutinformation(df$rate, df$period_effect)
  c_mi <- mutinformation(df$rate, df$cohort_effect)
  bmi_mi <- mutinformation(df$rate, df$bmi_effect)
  
  tot = a_mi + p_mi + c_mi + bmi_mi
  combined <- round(100*c((rinfo-remaining)/rinfo, a_mi/tot, p_mi/tot, c_mi/tot, bmi_mi/tot), 3)
  names(combined) <- c('Total Information Contained (%)','Age (%)','Period (%)','Cohort (%)', 'BMI (%)')
  return(combined)
}

# load the necessary data 
load('../output/data_organized.RData')

# load the posterior from fitting to the 2004 data and use median estimates 
data_reference_year = 2004
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

# load the model predictions 
load('../output/trend_analysis/bmi_spline_0915/preds_3_6.RData')

# generate dataframe for nhw males
df_mi_nhw_male <- data.frame(rate = data_list_combined$mm_trend_incid_male_nhw$x,
                             age_lower = data_list_combined$mm_trend_incid_male_nhw$age_lower,
                             age_upper = data_list_combined$mm_trend_incid_male_nhw$age_upper,
                             period = data_list_combined$mm_trend_incid_male_nhw$year,
                             age_effect = NA,
                             period_effect = NA,
                             cohort_effect = NA,
                             bmi_effect = NA)
df_mi_nhw_male$period_effect <- preds_combined$period_effect_CI['50%', match(df_mi_nhw_male$period, preds_combined$years_period)]

for(ii in 1:nrow(df_mi_nhw_male))
{
  # get the age indices 
  age_indices <- match(df_mi_nhw_male$age_lower[ii], ages):match(df_mi_nhw_male$age_upper[ii], ages)
  
  # get the vectors for the bmi, age, and cohort effects across the age groupings
  vec_bmi_effect <- haz_trend_male_nhw_list[[df_mi_nhw_male$period[ii]]]$haz_mm_bmi[age_indices] / haz_trend_male_nhw_list[[data_reference_year]]$haz_mm_bmi[age_indices]
  vec_age_effect <- out_male_nhw$i_mm[age_indices]
  birth_years <- df_mi_nhw_male$period[ii] - df_mi_nhw_male$age_lower[ii]:df_mi_nhw_male$age_upper[ii]
  vec_cohort_effect <- preds_combined$cohort_effect_CI['50%', match(birth_years, preds_combined$years_cohort)]
  
  # weight by age group 
  df_mi_nhw_male$age_effect[ii] <- sum(vec_age_effect * weights_by_age_group_male_nhw_combined[[ii]])
  df_mi_nhw_male$cohort_effect[ii] <- sum(vec_cohort_effect * weights_by_age_group_male_nhw_combined[[ii]])
  df_mi_nhw_male$bmi_effect[ii] <- sum(vec_bmi_effect * weights_by_age_group_male_nhw_combined[[ii]])
}

# generate dataframe for nhw females
df_mi_nhw_female <- data.frame(rate = data_list_combined$mm_trend_incid_female_nhw$x,
                             age_lower = data_list_combined$mm_trend_incid_female_nhw$age_lower,
                             age_upper = data_list_combined$mm_trend_incid_female_nhw$age_upper,
                             period = data_list_combined$mm_trend_incid_female_nhw$year,
                             age_effect = NA,
                             period_effect = NA,
                             cohort_effect = NA,
                             bmi_effect = NA)
df_mi_nhw_female$period_effect <- preds_combined$period_effect_CI['50%', match(df_mi_nhw_female$period, preds_combined$years_period)]

for(ii in 1:nrow(df_mi_nhw_female))
{
  # get the age indices 
  age_indices <- match(df_mi_nhw_female$age_lower[ii], ages):match(df_mi_nhw_female$age_upper[ii], ages)
  
  # get the vectors for the bmi, age, and cohort effects across the age groupings
  vec_bmi_effect <- haz_trend_female_nhw_list[[df_mi_nhw_female$period[ii]]]$haz_mm_bmi[age_indices] / haz_trend_female_nhw_list[[data_reference_year]]$haz_mm_bmi[age_indices]
  vec_age_effect <- out_female_nhw$i_mm[age_indices]
  birth_years <- df_mi_nhw_female$period[ii] - df_mi_nhw_female$age_lower[ii]:df_mi_nhw_female$age_upper[ii]
  vec_cohort_effect <- preds_combined$cohort_effect_CI['50%', match(birth_years, preds_combined$years_cohort)]
  
  # weight by age group 
  df_mi_nhw_female$age_effect[ii] <- sum(vec_age_effect * weights_by_age_group_female_nhw_combined[[ii]])
  df_mi_nhw_female$cohort_effect[ii] <- sum(vec_cohort_effect * weights_by_age_group_female_nhw_combined[[ii]])
  df_mi_nhw_female$bmi_effect[ii] <- sum(vec_bmi_effect * weights_by_age_group_female_nhw_combined[[ii]])
}

# generate dataframe for nhb males
df_mi_nhb_male <- data.frame(rate = data_list_combined$mm_trend_incid_male_nhb$x,
                             age_lower = data_list_combined$mm_trend_incid_male_nhb$age_lower,
                             age_upper = data_list_combined$mm_trend_incid_male_nhb$age_upper,
                             period = data_list_combined$mm_trend_incid_male_nhb$year,
                             age_effect = NA,
                             period_effect = NA,
                             cohort_effect = NA,
                             bmi_effect = NA)
df_mi_nhb_male$period_effect <- preds_combined$period_effect_CI['50%', match(df_mi_nhb_male$period, preds_combined$years_period)]

for(ii in 1:nrow(df_mi_nhb_male))
{
  # get the age indices 
  age_indices <- match(df_mi_nhb_male$age_lower[ii], ages):match(df_mi_nhb_male$age_upper[ii], ages)
  
  # get the vectors for the bmi, age, and cohort effects across the age groupings
  vec_bmi_effect <- haz_trend_male_nhb_list[[df_mi_nhb_male$period[ii]]]$haz_mm_bmi[age_indices] / haz_trend_male_nhb_list[[data_reference_year]]$haz_mm_bmi[age_indices]
  vec_age_effect <- out_male_nhb$i_mm[age_indices]
  birth_years <- df_mi_nhb_male$period[ii] - df_mi_nhb_male$age_lower[ii]:df_mi_nhb_male$age_upper[ii]
  vec_cohort_effect <- preds_combined$cohort_effect_CI['50%', match(birth_years, preds_combined$years_cohort)]
  
  # weight by age group 
  df_mi_nhb_male$age_effect[ii] <- sum(vec_age_effect * weights_by_age_group_male_nhb_combined[[ii]])
  df_mi_nhb_male$cohort_effect[ii] <- sum(vec_cohort_effect * weights_by_age_group_male_nhb_combined[[ii]])
  df_mi_nhb_male$bmi_effect[ii] <- sum(vec_bmi_effect * weights_by_age_group_male_nhb_combined[[ii]])
}

# generate dataframe for nhb females
df_mi_nhb_female <- data.frame(rate = data_list_combined$mm_trend_incid_female_nhb$x,
                               age_lower = data_list_combined$mm_trend_incid_female_nhb$age_lower,
                               age_upper = data_list_combined$mm_trend_incid_female_nhb$age_upper,
                               period = data_list_combined$mm_trend_incid_female_nhb$year,
                               age_effect = NA,
                               period_effect = NA,
                               cohort_effect = NA,
                               bmi_effect = NA)
df_mi_nhb_female$period_effect <- preds_combined$period_effect_CI['50%', match(df_mi_nhb_female$period, preds_combined$years_period)]

for(ii in 1:nrow(df_mi_nhb_female))
{
  # get the age indices 
  age_indices <- match(df_mi_nhb_female$age_lower[ii], ages):match(df_mi_nhb_female$age_upper[ii], ages)
  
  # get the vectors for the bmi, age, and cohort effects across the age groupings
  vec_bmi_effect <- haz_trend_female_nhb_list[[df_mi_nhb_female$period[ii]]]$haz_mm_bmi[age_indices] / haz_trend_female_nhb_list[[data_reference_year]]$haz_mm_bmi[age_indices]
  vec_age_effect <- out_female_nhb$i_mm[age_indices]
  birth_years <- df_mi_nhb_female$period[ii] - df_mi_nhb_female$age_lower[ii]:df_mi_nhb_female$age_upper[ii]
  vec_cohort_effect <- preds_combined$cohort_effect_CI['50%', match(birth_years, preds_combined$years_cohort)]
  
  # weight by age group 
  df_mi_nhb_female$age_effect[ii] <- sum(vec_age_effect * weights_by_age_group_female_nhb_combined[[ii]])
  df_mi_nhb_female$cohort_effect[ii] <- sum(vec_cohort_effect * weights_by_age_group_female_nhb_combined[[ii]])
  df_mi_nhb_female$bmi_effect[ii] <- sum(vec_bmi_effect * weights_by_age_group_female_nhb_combined[[ii]])
}

# generate the mutual information for each of the gender-ethnicity pairings
mi_nhw_male <- score(df_mi_nhw_male)
mi_nhw_female <- score(df_mi_nhw_female)
mi_nhb_male <- score(df_mi_nhb_male)
mi_nhb_female <- score(df_mi_nhb_female)

# save mutual information scores to output 
save(mi_nhw_male,
     mi_nhw_female,
     mi_nhb_male,
     mi_nhb_female,
     file = '../output/fig_4.RData')
