# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing worspace
rm(list = ls())

# load the data 
load('../output/data_organized.RData')

# get the years of the period and cohort effects
years_period <- min(data_list$mm_trend_incid_male_nhw$year):max(data_list$mm_trend_incid_male_nhw$year)
years_cohort <- (min(data_list$mm_trend_incid_male_nhw$year) - max(data_list$mm_trend_incid_male_nhw$age_upper)):
  (max(data_list$mm_trend_incid_male_nhw$year) - min(data_list$mm_trend_incid_male_nhw$age_upper))

# calculate the maximum knots as number of 10 year intervals
max_knots_period <- round((max(years_period) - min(years_period)) / 10)
max_knots_cohort <- round((max(years_cohort) - min(years_cohort)) / 10)

# specify the number of knots for the period and cohort effects 
n_knots_period <- 3:max_knots_period
n_knots_cohort <- 3:max_knots_cohort
seed <- 1:5

# create dataframe with all possible combinations
df <- expand.grid(n_knots_period = n_knots_period,
                  n_knots_cohort = n_knots_cohort,
                  seed = seed)

# write to file
write.csv(df, file = '../output/trend_spline_input.csv', row.names = F)
