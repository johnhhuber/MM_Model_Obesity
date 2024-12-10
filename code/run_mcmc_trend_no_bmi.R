# get the command line arguments 
args <- commandArgs(trailingOnly = T)

# get the chain
chain = as.integer(args[1])

# get the path out 
path.out = args[2]

# set the seed
set.seed(chain)

# load necessary functions
source('functions_likelihood.R')

# load the data list 
load('output/data_organized.RData')

# set BMI hazard to 1 
haz_male_nhb$haz_mm_bmi <- 1
haz_male_nhw$haz_mm_bmi <- 1
haz_female_nhb$haz_mm_bmi <- 1
haz_female_nhw$haz_mm_bmi <- 1

for(yr in unique(data_list$mm_trend_incid_male_nhw$year))
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

# load the posterior from fitting to the 2004 data and use median estimates 
post_prior <- read.csv('output/no_bmi/posterior_pooled.csv.bz2')
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
period_reference_year = 2000
cohort_reference_year = 1950

#knots_x_period = c(round(seq(from = 1975, to = period_reference_year, length.out = 2)), round(seq(from = period_reference_year, to = 2019, length.out = 2))[-1])
#knots_x_cohort = c(round(seq(from = 1876, to = cohort_reference_year, length.out = 2)), round(seq(from = cohort_reference_year, to = 2019, length.out = 2))[-1])

# bayesian setup
#params_lower <- rep(-10, length(knots_x_period) - 1 + length(knots_x_cohort) - 1)
#params_upper <- rep(10, length(knots_x_period) - 1 + length(knots_x_cohort) - 1)

params_lower <- c(-10,
                  -10)
params_upper <- c(10,
                  10)


bayesianSetup <- createBayesianSetup(ll_with_trend_cmp, lower = params_lower, upper = params_upper)
settings = list(iterations = 5e5, message = F)

# run mcmc
out <- runMCMC(bayesianSetup, settings, sampler = "DEzs")
post <- out$Z  

# specify burn in and thin factor 
burn.in = 0
thin.factor = 1

# write to file 
write.csv(post[seq(from = burn.in + 1, to = nrow(post), by = thin.factor),],
          file = paste(path.out, 'posterior_', chain, '.csv', sep = ''),
          row.names = F)