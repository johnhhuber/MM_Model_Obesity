# load necessary functions
source('functions_likelihood.R')

# generate prediction for mgus prev 
pred_mgus_prev <- function(post,
                           burn.in,
                           num.samps,
                           ages,
                           pop,
                           mort,
                           mgus_mortality_multiplier,
                           mu_mm,
                           haz,
                           data_cohort,
                           gender,
                           race)
{
  # draw samples
  samps <- sample(max(burn.in):nrow(post), size = num.samps, replace = T)
  p_prev <- matrix(nrow = num.samps, ncol = nrow(data_cohort))
  
  for(i in 1:num.samps)
  {
    print(i)
    # simulate model
    sim <- sim_model(ages = ages,
              gamma_mgus = post[samps[i],1],
              beta_mgus_age = post[samps[i],2],
              beta_mgus_age_quad = 0,
              beta_mgus_sex = ifelse(gender == 'male',0,post[samps[i],3]),
              beta_mgus_race = ifelse(race == 'nhw', 0, post[samps[i],4]),
              gamma_mm = post[samps[i],5],
              beta_mm_age = post[samps[i],6],
              beta_mm_age_quad = post[samps[i],7],
              beta_mm_sex = ifelse(gender == 'male',0,post[samps[i],8]),
              beta_mm_race = ifelse(race == 'nhw',0,post[samps[i],9]),
              mgus_mortality_multiplier = mgus_mortality_multiplier,
              mort = mort,
              mu_mm = mu_mm,
              haz = haz)
    p_mgus <- sim$p_mgus
    
    # generate weighted prevalence 
    p_mgus_weighted <- rep(NA, nrow(data_cohort))
    if(any(data_cohort$age_upper == data_cohort$age_lower))
    {
      p_mgus_weighted[which(data_cohort$age_upper == data_cohort$age_lower)] = p_mgus[sapply(which(data_cohort$age_upper == data_cohort$age_lower), function(i){match(data_cohort$age_lower[i], ages)})]
    }
    
    for(ii in which(data_cohort$age_lower != data_cohort$age_upper))
    {
      p_mgus_weighted[ii] = sum(p_mgus[match(data_cohort$age_lower[ii], ages):match(data_cohort$age_upper[ii], ages)] * pop$N[match(data_cohort$age_lower[ii], pop$age):match(data_cohort$age_upper[ii], pop$age)]) /
        sum(pop$N[match(data_cohort$age_lower[ii], pop$age):match(data_cohort$age_upper[ii], pop$age)])
    }
    
    # add to matrix 
    p_prev[i,] <- p_mgus_weighted
  }
  
  # return matrix 
  return(p_prev)
}


# generate prediction for mgus cohort
pred_mgus_cohort <- function(post,
                             burn.in,
                             num.samps,
                             ages,
                             pop,
                             data_cohort)
{
  # draw samples 
  samps <- sample(max(burn.in):nrow(post), size = num.samps, replace = T)
  y_pred <- array(dim = c(num.samps, nrow(data_cohort), num.samps))
  
  for(i in 1:num.samps)
  {
    sim <- sim_model(ages = ages,
                     gamma_mgus = post[samps[i],1],
                     beta_mgus_age = post[samps[i],2],
                     gamma_mm = post[samps[i],3],
                     beta_mm_age = post[samps[i],4])
    for(j in 1:num.samps)
    {
      y_pred[i,,j] <- sim_data_mgus_prev(n = data_cohort$n, ages = ages, p_mgus = sim$p_mgus,
                                         age_lower = data_cohort$age_lower, age_upper = data_cohort$age_upper,
                                         pop = pop)$y
    }
  }
  
  # compute PPI 
  y_ppi <- apply(y_pred, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  
  # return output
  return(y_ppi)
}

# function to calc preds for trend model
calc_preds_trend <- function(post,
                             burn.in,
                             num.samps,
                             data_list,
                             knots_x_period,
                             knots_x_cohort,
                             haz_trend_male_nhw_list,
                             haz_trend_female_nhw_list,
                             haz_trend_male_nhb_list,
                             haz_trend_female_nhb_list,
                             out_male_nhw,
                             out_female_nhw,
                             out_male_nhb,
                             out_female_nhb,
                             ages,
                             pop_trend_male_nhw,
                             pop_trend_female_nhw,
                             pop_trend_male_nhb,
                             pop_trend_female_nhb,
                             weights_by_age_group_male_nhw,
                             weights_by_age_group_female_nhw,
                             weights_by_age_group_male_nhb,
                             weights_by_age_group_female_nhb)
{
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
  
  years_period = min(knots_x_period):max(knots_x_period)
  years_cohort = min(knots_x_cohort):max(knots_x_cohort)
  
  # draw samples 
  samps <- sample(max(burn.in):nrow(post), size = num.samps, replace = T)
  
  # create matrix to store quantiles
  x_pred_male_nhw <- array(dim = c(num.samps, nrow(data_list$mm_trend_incid_male_nhw), num.samps))
  x_pred_female_nhw <- array(dim = c(num.samps, nrow(data_list$mm_trend_incid_female_nhw), num.samps))
  x_pred_male_nhb <- array(dim = c(num.samps, nrow(data_list$mm_trend_incid_male_nhb), num.samps))
  x_pred_female_nhb <- array(dim = c(num.samps, nrow(data_list$mm_trend_incid_female_nhb), num.samps))
  
  mat_period_effect <- matrix(NA, nrow = num.samps, ncol = length(min(knots_x_period):max(knots_x_period)))
  mat_cohort_effect <- matrix(NA, nrow = num.samps, ncol = length(min(knots_x_cohort):max(knots_x_cohort)))
  
  # loop through and generate predictions
  for(i in 1:num.samps)
  {
    print(i)
    
    # get the parameters
    y_knots_period <- rep(0, length(knots_x_period))
    y_knots_cohort <- rep(0, length(knots_x_cohort))
    
    y_knots_period[-which(knots_x_period == period_reference_year)] <- head(as.numeric(post[samps[i],]), length(knots_x_period) - 1)
    y_knots_cohort[-which(knots_x_cohort == cohort_reference_year)] <- tail(as.numeric(post[samps[i],]), length(knots_x_cohort) - 1)
    
    # predict the period and cohort effects 
    mat_period_effect[i,] <- period_effect_mm(y_knots_period = y_knots_period, x_knots_period = knots_x_period)
    mat_cohort_effect[i,] <- cohort_effect_mm(y_knots_cohort = y_knots_cohort, x_knots_cohort = knots_x_cohort)
    
    # simulate the incidence 
    # n_data x n_samps
    x_pred_male_nhw[i,,] <- sim_mm_incid_trend_spline(df_mm_trend = data_list[['mm_trend_incid_male_nhw']], haz_trend_list = haz_trend_male_nhw_list, i_mm_reference = out_male_nhw$i_mm, ages = ages, pop_trend = pop_trend_male_nhw,
                             y_knots_period = y_knots_period, y_knots_cohort = y_knots_cohort,
                             weights_by_age_group = weights_by_age_group_male_nhw,num.samps = num.samps)
    x_pred_female_nhw[i,,] <- sim_mm_incid_trend_spline(df_mm_trend = data_list[['mm_trend_incid_female_nhw']], haz_trend_list = haz_trend_female_nhw_list, i_mm_reference = out_female_nhw$i_mm, ages = ages, pop_trend = pop_trend_female_nhw,
                                                      y_knots_period = y_knots_period, y_knots_cohort = y_knots_cohort,
                                                      weights_by_age_group = weights_by_age_group_female_nhw,num.samps = num.samps)
    x_pred_male_nhb[i,,] <- sim_mm_incid_trend_spline(df_mm_trend = data_list[['mm_trend_incid_male_nhb']], haz_trend_list = haz_trend_male_nhb_list, i_mm_reference = out_male_nhb$i_mm, ages = ages, pop_trend = pop_trend_male_nhb,
                                                      y_knots_period = y_knots_period, y_knots_cohort = y_knots_cohort,
                                                      weights_by_age_group = weights_by_age_group_male_nhb,num.samps = num.samps)
    x_pred_female_nhb[i,,] <- sim_mm_incid_trend_spline(df_mm_trend = data_list[['mm_trend_incid_female_nhb']], haz_trend_list = haz_trend_female_nhb_list, i_mm_reference = out_female_nhb$i_mm, ages = ages, pop_trend = pop_trend_female_nhb,
                                                        y_knots_period = y_knots_period, y_knots_cohort = y_knots_cohort,
                                                        weights_by_age_group = weights_by_age_group_female_nhb,num.samps = num.samps)
  }
  
  # calculate CI
  period_effect_CI <- apply(mat_period_effect, 2, quantile, prob = c(0.025, 0.25, 0.50, 0.75, 0.975))
  cohort_effect_CI <- apply(mat_cohort_effect, 2, quantile, prob = c(0.025, 0.25, 0.50, 0.75, 0.975))
  
  # compute PPI 
  x_male_nhw_ppi <- apply(x_pred_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_male_nhb_ppi <- apply(x_pred_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_female_nhw_ppi <- apply(x_pred_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_female_nhb_ppi <- apply(x_pred_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  
  # return list
  return(list(period_effect_CI = period_effect_CI,
              cohort_effect_CI = cohort_effect_CI,
              x_male_nhw_ppi = x_male_nhw_ppi,
              x_female_nhw_ppi = x_female_nhw_ppi,
              x_male_nhb_ppi = x_male_nhb_ppi,
              x_female_nhb_ppi = x_female_nhb_ppi,
              x_pred_male_nhw = x_pred_male_nhw,
              x_pred_male_nhb = x_pred_male_nhb,
              x_pred_female_nhw = x_pred_female_nhw,
              x_pred_female_nhb = x_pred_female_nhb,
              years_period = years_period,
              years_cohort = years_cohort))
  
}
  
# function to calculate preds for model
calc_preds <- function(post,
                       burn.in,
                       num.samps,
                       data_list,
                       ages,
                       pop_male_nhw,
                       pop_female_nhw,
                       pop_male_nhb,
                       pop_female_nhb,
                       mort_male_nhw,
                       mort_female_nhw,
                       mort_male_nhb,
                       mort_female_nhb,
                       mgus_mortality_multiplier_male,
                       mgus_mortality_multiplier_female,
                       mu_mm_male_nhw,
                       mu_mm_female_nhw,
                       mu_mm_male_nhb,
                       mu_mm_female_nhb,
                       haz_male_nhw,
                       haz_female_nhw,
                       haz_male_nhb,
                       haz_female_nhb)
{
  # draw samples 
  samps <- sample(max(burn.in):nrow(post), size = num.samps, replace = T)
  
  # create matrix to store quantities
  mat_i_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_i_mm_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mm_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mm_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mm_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))

  mat_i_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_i_mm_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mm_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mm_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mm_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  
  mat_i_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_i_mm_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mm_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mm_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mm_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  
  mat_i_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_i_mm_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mm_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mm_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mm_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  
  y_pred_male_nhw <- array(dim = c(num.samps, nrow(data_list$mgus_prev_male_nhw), num.samps))
  x_pred_male_nhw <- array(dim = c(num.samps, nrow(data_list$mm_incid_male_nhw), num.samps))
  
  y_pred_male_nhb <- array(dim = c(num.samps, nrow(data_list$mgus_prev_male_nhb), num.samps))
  x_pred_male_nhb <- array(dim = c(num.samps, nrow(data_list$mm_incid_male_nhb), num.samps))
  
  y_pred_female_nhw <- array(dim = c(num.samps, nrow(data_list$mgus_prev_female_nhw), num.samps))
  x_pred_female_nhw <- array(dim = c(num.samps, nrow(data_list$mm_incid_female_nhw), num.samps))
  
  y_pred_female_nhb <- array(dim = c(num.samps, nrow(data_list$mgus_prev_female_nhb), num.samps))
  x_pred_female_nhb <- array(dim = c(num.samps, nrow(data_list$mm_incid_female_nhb), num.samps))
  
  for(i in 1:num.samps)
  {
    print(i)
    sim_male_nhw <- sim_model(ages = ages,
                          gamma_mgus = post[samps[i],1],
                          beta_mgus_age = post[samps[i],2],
                          beta_mgus_age_quad = 0,
                          beta_mgus_sex = 0,
                          beta_mgus_race = 0,
                          gamma_mm = post[samps[i],5],
                          beta_mm_age = post[samps[i],6],
                          beta_mm_age_quad = post[samps[i],7],
                          beta_mm_sex = 0,
                          beta_mm_race = 0,
                          mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                          mort = mort_male_nhw,
                          mu_mm = mu_mm_male_nhw,
                          haz = haz_male_nhw)
    
    sim_male_nhb <- sim_model(ages = ages,
                              gamma_mgus = post[samps[i],1],
                              beta_mgus_age = post[samps[i],2],
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = 0,
                              beta_mgus_race = post[samps[i],4],
                              gamma_mm = post[samps[i],5],
                              beta_mm_age = post[samps[i],6],
                              beta_mm_age_quad = post[samps[i],7],
                              beta_mm_sex = 0,
                              beta_mm_race = post[samps[i],9],
                              mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                              mort = mort_male_nhb,
                              mu_mm = mu_mm_male_nhb,
                              haz = haz_male_nhb)
    
    sim_female_nhw <- sim_model(ages = ages,
                              gamma_mgus = post[samps[i],1],
                              beta_mgus_age = post[samps[i],2],
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = post[samps[i],3],
                              beta_mgus_race = 0,
                              gamma_mm = post[samps[i],5],
                              beta_mm_age = post[samps[i],6],
                              beta_mm_age_quad = post[samps[i],7],
                              beta_mm_sex = post[samps[i],8],
                              beta_mm_race = 0,
                              mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                              mort = mort_female_nhw,
                              mu_mm = mu_mm_female_nhw,
                              haz = haz_female_nhw)
    
    sim_female_nhb <- sim_model(ages = ages,
                                gamma_mgus = post[samps[i],1],
                                beta_mgus_age = post[samps[i],2],
                                beta_mgus_age_quad = 0,
                                beta_mgus_sex = post[samps[i],3],
                                beta_mgus_race = post[samps[i],4],
                                gamma_mm = post[samps[i],5],
                                beta_mm_age = post[samps[i],6],
                                beta_mm_age_quad = post[samps[i],7],
                                beta_mm_sex = post[samps[i],8],
                                beta_mm_race = post[samps[i],9],
                                mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                mort = mort_female_nhb,
                                mu_mm = mu_mm_female_nhb,
                                haz = haz_female_nhb)
    
    
    mat_p_mgus_male_nhw[i,] <- sim_male_nhw$p_mgus
    mat_p_mm_male_nhw[i,] <- sim_male_nhw$p_mm
    mat_i_mgus_male_nhw[i,] <- sim_male_nhw$i_mgus
    mat_i_mm_male_nhw[i,] <- sim_male_nhw$i_mm
    mat_cum_prop_mgus_male_nhw[i,] <- sim_male_nhw$cum_prop_mgus
    mat_cum_prop_mm_male_nhw[i,] <- sim_male_nhw$cum_prop_mm
    mat_lambda_mgus_male_nhw[i,] <- exp(post[samps[i],1] + post[samps[i],2] * ages)
    mat_lambda_mm_male_nhw[i,] <- exp(post[samps[i],5] + post[samps[i],6] * ages + post[samps[i],7] * ages^2)
    
    mat_p_mgus_male_nhb[i,] <- sim_male_nhb$p_mgus
    mat_p_mm_male_nhb[i,] <- sim_male_nhb$p_mm
    mat_i_mgus_male_nhb[i,] <- sim_male_nhb$i_mgus
    mat_i_mm_male_nhb[i,] <- sim_male_nhb$i_mm
    mat_cum_prop_mgus_male_nhb[i,] <- sim_male_nhb$cum_prop_mgus
    mat_cum_prop_mm_male_nhb[i,] <- sim_male_nhb$cum_prop_mm
    mat_lambda_mgus_male_nhb[i,] <- exp(post[samps[i],1] + post[samps[i],2] * ages + post[samps[i],4])
    mat_lambda_mm_male_nhb[i,] <- exp(post[samps[i],5] + post[samps[i],6] * ages + post[samps[i],7] * ages^2 + post[samps[i],9])
    
    mat_p_mgus_female_nhw[i,] <- sim_female_nhw$p_mgus
    mat_p_mm_female_nhw[i,] <- sim_female_nhw$p_mm
    mat_i_mgus_female_nhw[i,] <- sim_female_nhw$i_mgus
    mat_i_mm_female_nhw[i,] <- sim_female_nhw$i_mm
    mat_cum_prop_mgus_female_nhw[i,] <- sim_female_nhw$cum_prop_mgus
    mat_cum_prop_mm_female_nhw[i,] <- sim_female_nhw$cum_prop_mm
    mat_lambda_mgus_female_nhw[i,] <- exp(post[samps[i],1] + post[samps[i],2] * ages + post[samps[i],3])
    mat_lambda_mm_female_nhw[i,] <- exp(post[samps[i],5] + post[samps[i],6] * ages + post[samps[i],7] * ages^2 + post[samps[i],8])
    
    mat_p_mgus_female_nhb[i,] <- sim_female_nhb$p_mgus
    mat_p_mm_female_nhb[i,] <- sim_female_nhb$p_mm
    mat_i_mgus_female_nhb[i,] <- sim_female_nhb$i_mgus
    mat_i_mm_female_nhb[i,] <- sim_female_nhb$i_mm
    mat_cum_prop_mgus_female_nhb[i,] <- sim_female_nhb$cum_prop_mgus
    mat_cum_prop_mm_female_nhb[i,] <- sim_female_nhb$cum_prop_mm
    mat_lambda_mgus_female_nhb[i,] <- exp(post[samps[i],1] + post[samps[i],2] * ages + post[samps[i],3] + post[samps[i],4])
    mat_lambda_mm_female_nhb[i,] <- exp(post[samps[i],5] + post[samps[i],6] * ages + post[samps[i],7] * ages^2 + post[samps[i],8] + post[samps[i],9])
    
    for(j in 1:num.samps)
    {
      y_pred_male_nhw[i,,j] <- sim_data_mgus_prev(n = data_list$mgus_prev_male_nhw$n, ages = ages, p_mgus = sim_male_nhw$p_mgus,
                                              age_lower = data_list$mgus_prev_male_nhw$age_lower, age_upper = data_list$mgus_prev_male_nhw$age_upper,
                                              pop = pop_male_nhw)$y
      y_pred_male_nhb[i,,j] <- sim_data_mgus_prev(n = data_list$mgus_prev_male_nhb$n, ages = ages, p_mgus = sim_male_nhb$p_mgus,
                                                  age_lower = data_list$mgus_prev_male_nhb$age_lower, age_upper = data_list$mgus_prev_male_nhb$age_upper,
                                                  pop = pop_male_nhb)$y
      
      x_pred_male_nhw[i,,j] <- sim_data_mm_incid(ages = ages, i_mm = sim_male_nhw$i_mm, pop = pop_male_nhw,
                                             age_lower = data_list$mm_incid_male_nhw$age_lower, age_upper = data_list$mm_incid_male_nhw$age_upper,
                                             denom = data_list$mm_incid_male_nhw$n)$x
      x_pred_male_nhb[i,,j] <- sim_data_mm_incid(ages = ages, i_mm = sim_male_nhb$i_mm, pop = pop_male_nhb,
                                                 age_lower = data_list$mm_incid_male_nhb$age_lower, age_upper = data_list$mm_incid_male_nhb$age_upper,
                                                 denom = data_list$mm_incid_male_nhb$n)$x
      
      
      y_pred_female_nhw[i,,j] <- sim_data_mgus_prev(n = data_list$mgus_prev_female_nhw$n, ages = ages, p_mgus = sim_female_nhw$p_mgus,
                                                age_lower = data_list$mgus_prev_female_nhw$age_lower, age_upper = data_list$mgus_prev_female_nhw$age_upper,
                                                pop = pop_female_nhw)$y
      y_pred_female_nhb[i,,j] <- sim_data_mgus_prev(n = data_list$mgus_prev_female_nhb$n, ages = ages, p_mgus = sim_female_nhb$p_mgus,
                                                    age_lower = data_list$mgus_prev_female_nhb$age_lower, age_upper = data_list$mgus_prev_female_nhb$age_upper,
                                                    pop = pop_female_nhb)$y
      
      x_pred_female_nhw[i,,j] <- sim_data_mm_incid(ages = ages, i_mm = sim_female_nhw$i_mm, pop = pop_female_nhw,
                                                 age_lower = data_list$mm_incid_female_nhw$age_lower, age_upper = data_list$mm_incid_female_nhw$age_upper,
                                                 denom = data_list$mm_incid_female_nhw$n)$x
      x_pred_female_nhb[i,,j] <- sim_data_mm_incid(ages = ages, i_mm = sim_female_nhb$i_mm, pop = pop_female_nhb,
                                                 age_lower = data_list$mm_incid_female_nhb$age_lower, age_upper = data_list$mm_incid_female_nhb$age_upper,
                                                 denom = data_list$mm_incid_female_nhb$n)$x
    }
  }
  
  # compute CI 
  p_mgus_male_nhw_CI <- apply(mat_p_mgus_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  i_mgus_male_nhw_CI <- apply(mat_i_mgus_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  p_mm_male_nhw_CI <- apply(mat_p_mm_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  i_mm_male_nhw_CI <- apply(mat_i_mm_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mgus_male_nhw_CI <- apply(mat_cum_prop_mgus_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mm_male_nhw_CI <- apply(mat_cum_prop_mm_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  lambda_mgus_male_nhw_CI <- apply(mat_lambda_mgus_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  lambda_mm_male_nhw_CI <- apply(mat_lambda_mm_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  
  p_mgus_male_nhb_CI <- apply(mat_p_mgus_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  i_mgus_male_nhb_CI <- apply(mat_i_mgus_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  p_mm_male_nhb_CI <- apply(mat_p_mm_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  i_mm_male_nhb_CI <- apply(mat_i_mm_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mgus_male_nhb_CI <- apply(mat_cum_prop_mgus_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mm_male_nhb_CI <- apply(mat_cum_prop_mm_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  lambda_mgus_male_nhb_CI <- apply(mat_lambda_mgus_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  lambda_mm_male_nhb_CI <- apply(mat_lambda_mm_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  
  p_mgus_female_nhw_CI <- apply(mat_p_mgus_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  i_mgus_female_nhw_CI <- apply(mat_i_mgus_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  p_mm_female_nhw_CI <- apply(mat_p_mm_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  i_mm_female_nhw_CI <- apply(mat_i_mm_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mgus_female_nhw_CI <- apply(mat_cum_prop_mgus_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mm_female_nhw_CI <- apply(mat_cum_prop_mm_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  lambda_mgus_female_nhw_CI <- apply(mat_lambda_mgus_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  lambda_mm_female_nhw_CI <- apply(mat_lambda_mm_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  
  p_mgus_female_nhb_CI <- apply(mat_p_mgus_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  i_mgus_female_nhb_CI <- apply(mat_i_mgus_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  p_mm_female_nhb_CI <- apply(mat_p_mm_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  i_mm_female_nhb_CI <- apply(mat_i_mm_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mgus_female_nhb_CI <- apply(mat_cum_prop_mgus_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mm_female_nhb_CI <- apply(mat_cum_prop_mm_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  lambda_mgus_female_nhb_CI <- apply(mat_lambda_mgus_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  lambda_mm_female_nhb_CI <- apply(mat_lambda_mm_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  
  # compute PPI 
  y_male_nhw_ppi <- apply(y_pred_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  y_male_nhb_ppi <- apply(y_pred_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_male_nhw_ppi <- apply(x_pred_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_male_nhb_ppi <- apply(x_pred_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  
  y_female_nhw_ppi <- apply(y_pred_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  y_female_nhb_ppi <- apply(y_pred_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_female_nhw_ppi <- apply(x_pred_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_female_nhb_ppi <- apply(x_pred_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  
  # return output
  return(list(p_mgus_male_nhw_CI = p_mgus_male_nhw_CI,
              i_mgus_male_nhw_CI = i_mgus_male_nhw_CI,
              p_mm_male_nhw_CI = p_mm_male_nhw_CI,
              i_mm_male_nhw_CI = i_mm_male_nhw_CI,
              cum_prop_mgus_male_nhw_CI = cum_prop_mgus_male_nhw_CI,
              cum_prop_mm_male_nhw_CI = cum_prop_mm_male_nhw_CI,
              lambda_mgus_male_nhw_CI = lambda_mgus_male_nhw_CI,
              lambda_mm_male_nhw_CI = lambda_mm_male_nhw_CI,
              y_male_nhw_ppi = y_male_nhw_ppi,
              x_male_nhw_ppi = x_male_nhw_ppi,
              p_mgus_male_nhb_CI = p_mgus_male_nhb_CI,
              i_mgus_male_nhb_CI = i_mgus_male_nhb_CI,
              p_mm_male_nhb_CI = p_mm_male_nhb_CI,
              i_mm_male_nhb_CI = i_mm_male_nhb_CI,
              cum_prop_mgus_male_nhb_CI = cum_prop_mgus_male_nhb_CI,
              cum_prop_mm_male_nhb_CI = cum_prop_mm_male_nhb_CI,
              lambda_mgus_male_nhb_CI = lambda_mgus_male_nhb_CI,
              lambda_mm_male_nhb_CI = lambda_mm_male_nhb_CI,
              y_male_nhb_ppi = y_male_nhb_ppi,
              x_male_nhb_ppi = x_male_nhb_ppi,
              p_mgus_female_nhw_CI = p_mgus_female_nhw_CI,
              i_mgus_female_nhw_CI = i_mgus_female_nhw_CI,
              p_mm_female_nhw_CI = p_mm_female_nhw_CI,
              i_mm_female_nhw_CI = i_mm_female_nhw_CI,
              cum_prop_mgus_female_nhw_CI = cum_prop_mgus_female_nhw_CI,
              cum_prop_mm_female_nhw_CI = cum_prop_mm_female_nhw_CI,
              lambda_mgus_female_nhw_CI = lambda_mgus_female_nhw_CI,
              lambda_mm_female_nhw_CI = lambda_mm_female_nhw_CI,
              y_female_nhw_ppi = y_female_nhw_ppi,
              x_female_nhw_ppi = x_female_nhw_ppi,
              p_mgus_female_nhb_CI = p_mgus_female_nhb_CI,
              i_mgus_female_nhb_CI = i_mgus_female_nhb_CI,
              p_mm_female_nhb_CI = p_mm_female_nhb_CI,
              i_mm_female_nhb_CI = i_mm_female_nhb_CI,
              cum_prop_mgus_female_nhb_CI = cum_prop_mgus_female_nhb_CI,
              cum_prop_mm_female_nhb_CI = cum_prop_mm_female_nhb_CI,
              lambda_mgus_female_nhb_CI = lambda_mgus_female_nhb_CI,
              lambda_mm_female_nhb_CI = lambda_mm_female_nhb_CI,
              y_female_nhb_ppi = y_female_nhb_ppi,
              x_female_nhb_ppi = x_female_nhb_ppi))
}

# generate prediction for mgus cohort
pred_mgus_cohort <- function(post,
                             burn.in,
                             num.samps,
                             data_list,
                             ages,
                             pop_male_nhw,
                             pop_female_nhw,
                             mort_male_nhw,
                             mort_female_nhw,
                             mgus_mortality_multiplier_male,
                             mgus_mortality_multiplier_female,
                             mu_mm_male_nhw,
                             mu_mm_female_nhw,
                             data_cohort,
                             is_male)
{
  # draw samples 
  samps <- sample(max(burn.in):nrow(post), size = num.samps, replace = T)
  
  # subset data cohort according to sex and make prediction matrix 
  if(is_male)
  {
    data_cohort <- subset(data_cohort, sex == 'Male')
  }else{
    data_cohort <- subset(data_cohort, sex == 'Female')
  }
  y_pred <- array(dim = c(num.samps, nrow(data_cohort), num.samps))
  
  for(i in 1:num.samps)
  {
    if(is_male)
    {
      sim <- sim_model(ages = ages,
                       gamma_mgus = post[samps[i],1],
                       beta_mgus_age = post[samps[i],2],
                       beta_mgus_age_quad = 0,
                       beta_mgus_sex = 0,
                       beta_mgus_race = 0,
                       gamma_mm = post[samps[i],5],
                       beta_mm_age = post[samps[i],6],
                       beta_mm_age_quad = post[samps[i],7],
                       beta_mm_sex = 0,
                       beta_mm_race = 0,
                       mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                       mort = mort_male_nhw,
                       mu_mm = mu_mm_male_nhw)
      
      for(j in 1:num.samps)
      {
        y_pred[i,,j] <- sim_data_mgus_prev(n = data_cohort$n, ages = ages, p_mgus = sim$p_mgus,
                                           age_lower = data_cohort$age_lower, age_upper = data_cohort$age_upper,
                                           pop = pop_male_nhw)$y
      }
    }else{
      sim <- sim_model(ages = ages,
                       gamma_mgus = post[samps[i],1],
                       beta_mgus_age = post[samps[i],2],
                       beta_mgus_age_quad = 0,
                       beta_mgus_sex = post[samps[i],3],
                       beta_mgus_race = 0,
                       gamma_mm = post[samps[i],5],
                       beta_mm_age = post[samps[i],6],
                       beta_mm_age_quad = post[samps[i],7],
                       beta_mm_sex = post[samps[i],8],
                       beta_mm_race = 0,
                       mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                       mort = mort_female_nhw,
                       mu_mm = mu_mm_female_nhw)
      
      for(j in 1:num.samps)
      {
        y_pred[i,,j] <- sim_data_mgus_prev(n = data_cohort$n, ages = ages, p_mgus = sim$p_mgus,
                                           age_lower = data_cohort$age_lower, age_upper = data_cohort$age_upper,
                                           pop = pop_female_nhw)$y
      }
    }
  }
  
  # compute PPI 
  y_ppi <- apply(y_pred, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  
  # return output
  return(y_ppi)
}