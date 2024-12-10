# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_convergence.R')
source('functions_likelihood.R')

# specify the path in
path.in = '../output/trend_analysis/no_bmi_spline/'

# load the different combinations of knots 
input_params <- read.csv('../output/trend_spline_input.csv')
n_knots_period <- unique(input_params$n_knots_period)
n_knots_cohort <- unique(input_params$n_knots_cohort)

# loop through and calculate the convergence and pool the posteriors 
for(pp in 1:length(n_knots_period))
{
  for(cc in 1:length(n_knots_cohort))
  {
    print(cc)
    indices <- which(input_params$n_knots_period == n_knots_period[pp] & 
                       input_params$n_knots_cohort == n_knots_cohort[cc])
    
    files <- paste(path.in, 'posterior_', indices, '.csv.bz2', sep = '')
    get_convergence_stats_files(path.in = path.in,
                                post.files = files,
                                burn.in = 2.5e5,
                                thin.factor = 50)
    pool_chain_files(path.in = path.in,
                     post.files = files,
                     burn.in = 2.5e5,
                     thin.factor = 50,
                     file.out = paste(path.in, 'posterior_pooled_', n_knots_period[pp],'_',n_knots_cohort[cc],'.csv',sep=''))
  }
}


### Calculate the DIC of each model fit ### 

# load the data list 
load('../output/data_organized.RData')

# calculate the number of data points 
num.data <- nrow(data_list$mm_trend_incid_male_nhw) + nrow(data_list$mm_trend_incid_female_nhw) +
  nrow(data_list$mm_trend_incid_male_nhb) + nrow(data_list$mm_trend_incid_female_nhb)

# load the posterior from fitting to the 2004 data and use median estimates 
post_prior <- read.csv('../output/single_year_analysis/no_bmi/posterior_pooled.csv.bz2')
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

# set BMI hazard to 1 since not including the effect of BMI in this scenario 
for(yr in 1975:2018)
{
  haz_trend_male_nhw_list[[yr]]$haz_mgus_bmi <- 1
  haz_trend_male_nhw_list[[yr]]$haz_mm_bmi <- 1
  
  haz_trend_female_nhw_list[[yr]]$haz_mgus_bmi <- 1
  haz_trend_female_nhw_list[[yr]]$haz_mm_bmi <- 1
  
  haz_trend_male_nhb_list[[yr]]$haz_mgus_bmi <- 1
  haz_trend_male_nhb_list[[yr]]$haz_mm_bmi <- 1
  
  haz_trend_female_nhb_list[[yr]]$haz_mgus_bmi <- 1
  haz_trend_female_nhb_list[[yr]]$haz_mm_bmi <- 1
}

haz_male_nhw$haz_mgus_bmi <- 1
haz_male_nhw$haz_mm_bmi <- 1
haz_female_nhw$haz_mgus_bmi <- 1
haz_female_nhw$haz_mm_bmi <- 1

haz_male_nhb$haz_mgus_bmi <- 1
haz_male_nhb$haz_mm_bmi <- 1
haz_female_nhb$haz_mgus_bmi <- 1
haz_female_nhb$haz_mm_bmi <- 1

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

### Loop through and calculate DIC ###
df <- expand.grid(knots_period = n_knots_period,
                  knots_cohort = n_knots_cohort)
df$DIC <- NA
df$BIC <- NA

for(pp in 1:length(n_knots_period))
{
  for(cc in 1:length(n_knots_cohort))
  {
    print(cc)
    knots_x_period <- seq(from = min(data_list$mm_trend_incid_male_nhw$year) - period_reference_year, 
                          to = max(data_list$mm_trend_incid_male_nhw$year) - period_reference_year,
                          length.out = n_knots_period[pp])
    knots_x_period[which.min(abs(knots_x_period))] <- 0
    knots_x_period <- knots_x_period + period_reference_year
    
    knots_x_cohort <- seq(from = min(data_list$mm_trend_incid_male_nhw$year) - max(data_list$mm_trend_incid_male_nhw$age_upper) - cohort_reference_year,
                          to = max(data_list$mm_trend_incid_male_nhw$year) - min(data_list$mm_trend_incid_male_nhw$age_lower) - cohort_reference_year,
                          length.out = n_knots_cohort[cc])
    knots_x_cohort[which.min(abs(knots_x_cohort))] <- 0
    knots_x_cohort <- knots_x_cohort + cohort_reference_year
    
    post <- read.csv(paste('../output/trend_analysis/no_bmi_spline/posterior_pooled_',n_knots_period[pp],'_',n_knots_cohort[cc],'.csv', sep = ''))
    val_DIC <- DIC(post = post, likelihood = ll_with_trend_spline_cmp, num.samps = 100)
    val_BIC <- BIC(post = post, likelihood = ll_with_trend_spline_cmp, num.samps = 100, num.data = num.data)
    
    df$DIC[which(df$knots_period == n_knots_period[pp] & df$knots_cohort == n_knots_cohort[cc])] <- val_DIC
    df$BIC[which(df$knots_period == n_knots_period[pp] & df$knots_cohort == n_knots_cohort[cc])] <- val_BIC
  }
}

# best fitting model 3-3
