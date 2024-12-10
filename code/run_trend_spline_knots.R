# get the command line arguments 
args <- commandArgs(trailingOnly = T)

# get the chain
index = as.integer(args[1])

# specify the file path to the posterior 
file.post = args[2]

# specify the file path to the spline input
file.spline = args[3]

# get the path out 
path.out = args[4]

# get the integer for whether this is with or without BMI effects: 1 == with BMI; 0 == without BMI
with_BMI = as.integer(args[5])

# get the chain, number of knots 
df.spline <- read.csv(file.spline)
chain <- as.numeric(df.spline$seed[index])
n_knots_period <- as.numeric(df.spline$n_knots_period[index])
n_knots_cohort <- as.numeric(df.spline$n_knots_cohort[index])

# set the seed
set.seed(chain)

# load necessary functions
source('functions_likelihood.R')

# load the data list 
load('output/data_organized.RData')

# set BMI hazard to 1 
if(with_BMI != 1)
{
  
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
  
  haz_male_nhb$haz_mgus_bmi <- 1
  haz_male_nhw$haz_mgus_bmi <- 1
  haz_female_nhb$haz_mgus_bmi <- 1
  haz_female_nhw$haz_mgus_bmi <- 1
  
  haz_male_nhb$haz_mm_bmi <- 1
  haz_male_nhw$haz_mm_bmi <- 1
  haz_female_nhb$haz_mm_bmi <- 1
  haz_female_nhw$haz_mm_bmi <- 1
}

# load the posterior from fitting to the 2004 data and use median estimates 
post_prior <- read.csv(file.post)
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

# bayesian set up 
params_lower <- c(rep(-10,length(knots_x_period)-1),
                  rep(-10,length(knots_x_cohort)-1))
params_upper <- c(rep(10,length(knots_x_period)-1),
                  rep(10,length(knots_x_cohort)-1))

bayesianSetup <- createBayesianSetup(ll_with_trend_spline_cmp, lower = params_lower, upper = params_upper)
settings = list(iterations = 1e5, message = T)

# run mcmc
out <- runMCMC(bayesianSetup, settings, sampler = "DEzs")
post <- out$Z  

# specify burn in and thin factor 
burn.in = 0
thin.factor = 1

# write to file 
write.csv(post[seq(from = burn.in + 1, to = nrow(post), by = thin.factor),],
          file = paste(path.out, 'posterior_', index, '.csv', sep = ''),
          row.names = F)
