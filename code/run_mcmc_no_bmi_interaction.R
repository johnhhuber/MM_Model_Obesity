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

# load necessary data
load('output/data_organized.RData')

# set BMI hazard to 1 
haz_male_nhb$haz_mm_bmi <- 1
haz_male_nhw$haz_mm_bmi <- 1
haz_female_nhb$haz_mm_bmi <- 1
haz_female_nhw$haz_mm_bmi <- 1


# bayesian setup
params_lower <- c(-20, # gamma_mgus 
                  0, # beta_mgus_age
                  -15, # beta_mgus_nhw_female
                  -15, # beta_mgus_nhb_female
                  -15, # beta_mgus_nhb_male
                  -20, # gamma_mm
                  -15, # beta_mm_age
                  -15, # beta_mm_age_quad
                  -15, # beta_mm_nhw_female
                  -15, # beta_mm_nhb_female
                  -15) # beta_mm_nhb_male

params_upper <- c(0, # gamma_mgus 
                  1, # beta_mgus_age
                  5, # beta_mgus_nhw_female
                  5, # beta_mgus_nhb_female
                  5, # beta_mgus_nhb_male
                  0, # gamma_mm
                  1, # beta_mm_age
                  1, # beta_mm_age_quad
                  5, # beta_mm_nhw_female
                  5, # beta_mm_nhb_female
                  5) # beta_mm_nhb_male)

bayesianSetup <- createBayesianSetup(ll_interactions_sex_race_cmp, lower = params_lower, upper = params_upper)
settings = list(iterations = 1e3, message = F)

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
