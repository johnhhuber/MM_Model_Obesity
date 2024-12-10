# install necessary packages
library(RcppEigen)

# load necessary functions
source('functions_model.R')
Rcpp::sourceCpp(file = 'functions_rcpp.cpp')

sim_mm_incid_trend_spline <- function(df_mm_trend,
                                     haz_trend_list,
                                     i_mm_reference,
                                     ages, 
                                     pop_trend,
                                     y_knots_period, 
                                     y_knots_cohort,
                                     weights_by_age_group,
                                     num.samps)
{
  
  # compute the weighted incidence incorporating the period-cohort effects 
  i_mm_trend_weighted <- rep(NA, length(df_mm_trend$age_lower))
  
  # compute the period and cohort effects over time
  period_effect <- period_effect_mm(y_knots_period = y_knots_period, x_knots_period = knots_x_period)
  cohort_effect <- cohort_effect_mm(y_knots_cohort = y_knots_cohort, x_knots_cohort = knots_x_cohort)
  
  for(ii in 1:length(df_mm_trend$age_lower))
  {
    pc_mm <- period_effect[df_mm_trend$year[ii] - min(knots_x_period) + 1] * cohort_effect[(df_mm_trend$year[ii] - df_mm_trend$age_lower[ii]:df_mm_trend$age_upper[ii]) - min(knots_x_cohort) + 1]
    
    age_indices <- match(df_mm_trend$age_lower[ii], ages):match(df_mm_trend$age_upper[ii], ages)
    haz_mm_bmi <- haz_trend_list[[df_mm_trend$year[ii]]]$haz_mm_bmi[age_indices] / haz_trend_list[[data_reference_year]]$haz_mm_bmi[age_indices]
    i_mm_trend_weighted[ii] = sum(haz_mm_bmi * pc_mm * i_mm_reference[age_indices] * weights_by_age_group[[ii]])
    
  }
  
  pred_mm <- sapply(1:length(i_mm_trend_weighted), function(i){rpois(n = num.samps, lambda =  i_mm_trend_weighted[i] * df_mm_trend$n[i])})
  return(t(pred_mm))
}


# function to simulate the MGUS prevalence data
sim_data_mgus_prev <- function(n,
                               age_lower,
                               age_upper,
                               pop,
                               ages,
                               p_mgus)
{
  # weight p_mgus if necessary for upper age groups 
  p_mgus_weighted <- rep(NA, length(n))
  if(any(age_upper == age_lower))
  {
    p_mgus_weighted[which(age_upper == age_lower)] = p_mgus[sapply(which(age_upper == age_lower), function(i){match(age_lower[i], ages)})]
  }
  
  for(ii in which(age_lower != age_upper))
  {
    p_mgus_weighted[ii] = sum(p_mgus[match(age_lower[ii], ages):match(age_upper[ii], ages)] * pop$N[match(age_lower[ii], pop$age):match(age_upper[ii], pop$age)]) /
      sum(pop$N[match(age_lower[ii], pop$age):match(age_upper[ii], pop$age)])
  }
  
  # simulate number of positive samples y as a binomial distribution
  y <- rbinom(n = length(n), size = n, prob = p_mgus_weighted)
  
  # generate dataframe and return
  df <- data.frame(age_lower = age_lower,
                   age_upper = age_upper,
                   y = y,
                   n = n)
  return(df)
}

# function to simulate mm incidence 
sim_data_mm_incid <- function(ages,
                              pop,
                              age_lower,
                              age_upper,
                              i_mm,
                              denom)
{
  # compute the weighted incidence 
  i_mm_weighted <- rep(NA, length(age_lower))
  
  for(ii in 1:length(age_lower))
  {
    i_mm_weighted[ii] = sum(i_mm[match(age_lower[ii], ages):match(age_upper[ii], ages)] * pop$N[match(age_lower[ii], pop$age):match(age_upper[ii], pop$age)]) /
      sum(pop$N[match(age_lower[ii], pop$age):match(age_upper[ii], pop$age)])
  }
  
  # simulate incidence from a poisson distribution
  x <- rpois(n = length(i_mm_weighted), lambda = i_mm_weighted * denom) / denom
 
  # generate dataframe and return
  df <- data.frame(age_lower = age_lower,
                   age_upper = age_upper,
                   x = x)
  return(df)
}

# function to simulate mm trend incidence data
sim_data_mm_incid_trend <- function(df_mm_trend,
                                    i_mm_reference,
                                    ages, 
                                    pop_trend,
                                    beta_period,
                                    #beta_period_quad,
                                    beta_cohort,
                                    #beta_cohort_quad,
                                    weights_by_age_group)
{
  i_mm_trend_weighted <- rep(NA, length(df_mm_trend$age_lower))
  
  #period_effect <- period_effect_mm(y_knots_period = knots_period)
  #cohort_effect <- cohort_effect_mm(y_knots_cohort = knots_period)
  
  for(ii in 1:length(df_mm_trend$age_lower))
  {
    #pc_mm <- period_effect[df_mm_trend$year[ii] - 1975 + 1] 
   # pc_mm <-cohort_effect[(df_mm_trend$year[ii] - df_mm_trend$age_lower[ii]:df_mm_trend$age_upper[ii]) - 1876 + 1]
    
    period <- df_mm_trend$year[ii] - 2000
    cohort <- df_mm_trend$year[ii] - df_mm_trend$age_lower[ii]:df_mm_trend$age_upper[ii] - 1950
    
    pc_mm <- exp(beta_period * (period)) * exp(beta_cohort * (cohort))
    
    # pc_mm <- pc_effect_mm(age = df_mm_trend$age_lower[ii]:df_mm_trend$age_upper[ii],
    #                       year = df_mm_trend$year[ii],
    #                       height_cohort = height_cohort,
    #                       slope_cohort = slope_cohort,
    #                       height_period = height_period,
    #                       slope_period = slope_period)
    
    i_mm_trend_weighted[ii] = sum(pc_mm * i_mm_reference[match(df_mm_trend$age_lower[ii], ages):match(df_mm_trend$age_upper[ii], ages)] * weights_by_age_group[[ii]])
  }
  
  # simulate incidence from a poisson distribution
  x <- rpois(n = length(i_mm_trend_weighted), lambda = i_mm_trend_weighted * df_mm_trend$n) / df_mm_trend$n
  
  # return
  return(x)
}

# generate simulated data
sim_data <- function(ages,
                     age_lower_mgus_prev,
                     age_upper_mgus_prev,
                     age_lower_mm_incid,
                     age_upper_mm_incid,
                     gamma_mgus,
                     beta_mgus_age,
                     gamma_mm,
                     beta_mm_age,
                     n_mgus,
                     pop,
                     mort,
                     denom)
{
  # simulate model
  out <- sim_model(ages = ages,
                   gamma_mgus = gamma_mgus,
                   beta_mgus_age = beta_mgus_age,
                   gamma_mm = gamma_mm,
                   beta_mm_age = beta_mm_age)
  
  # simulate the mgus prevalence data
  df_mgus <- sim_data_mgus_prev(n = n_mgus, age_lower = age_lower_mgus_prev, age_upper = age_upper_mgus_prev, pop = pop, ages = ages, p_mgus = out$p_mgus)
  
  # simulate the mm incidence data
  df_mm <- sim_data_mm_incid(ages = ages, pop = pop, age_lower = age_lower_mm_incid, age_upper = age_upper_mm_incid, i_mm = out$i_mm, denom = denom)
  
  # generate list and return
  data_list <- list(mgus_prev = df_mgus, mm_incid = df_mm)
  return(data_list)
}

# function to simulate period effect for MM incidence
# period_effect_mm <- function(year,
#                              period_year_reference = 2004,
#                              height_period,
#                              slope_period)
# {
#   effect_period <- height_period / (1 + exp(-slope_period * (year - period_year_reference)))
#   return(effect_period)
# }

period_effect_mm <- function(y_knots_period,
                             x_knots_period)
{
  return(exp(computeSpline(x_pred = min(x_knots_period):max(x_knots_period), x_knots = x_knots_period, y_knots = y_knots_period)))
}

# function to simulate cohort effect for MM incidence 
# cohort_effect_mm <- function(year,
#                              cohort_year_reference = 1950,
#                              height_cohort,
#                              slope_cohort)
# {
#   effect_cohort <-
#     height_cohort / (1 + exp(-slope_cohort * (year - cohort_year_reference)))
#   return(effect_cohort)
# }

cohort_effect_mm <- function(y_knots_cohort,
                            x_knots_cohort)
{
 return(exp(computeSpline(x_pred = min(x_knots_cohort):max(x_knots_cohort), x_knots = x_knots_cohort, y_knots = y_knots_cohort)))
}

# calculate the period cohort effect for a given year and age 
# pc_effect_mm <- function(age,
#                          year,
#                          knots_period,
#                          knots_cohort)
# {
#   return(period_effect_mm(year = year, y_knots_period = knots_period) * 
#            cohort_effect_mm(year = year - age, y_knots_cohort = knots_cohort))
# }


# pc_effect_mm <- function(age,
#                          year,
#                          cohort_year_reference = 1950,
#                          period_year_reference = 2004,
#                          height_cohort,
#                          slope_cohort,
#                          height_period,
#                          slope_period)
# {
#   effect_pc <- cohort_effect_mm(year = year - age,
#                                 cohort_year_reference = cohort_year_reference,
#                                 height_cohort = height_cohort,
#                                 slope_cohort = slope_cohort) *
#     period_effect_mm(year = year,
#                      period_year_reference = period_year_reference,
#                      height_period = height_period,
#                      slope_period = slope_period)
# 
#   return(effect_pc)
# }