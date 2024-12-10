# install necessary packages
if(!require(BayesianTools)){install.packages('BayesianTools'); library(BayesianTools)}
if(!require(compiler)){install.packages('compiler'); library(compiler)}

# load necessary functions
source('functions_sim_data.R')

# function to normalize
normalize <- function(x){x / sum(x)}

# specify likelihood for mgus prevalence data
ll_mgus_prev <- function(df_mgus, ages, p_mgus, pop)
{
  # generate weighted prevalence 
  p_mgus_weighted <- rep(NA, nrow(df_mgus))
  if(any(df_mgus$age_upper == df_mgus$age_lower))
  {
    p_mgus_weighted[which(df_mgus$age_upper == df_mgus$age_lower)] = p_mgus[sapply(which(df_mgus$age_upper == df_mgus$age_lower), function(i){match(df_mgus$age_lower[i], ages)})]
  }
  
  for(ii in which(df_mgus$age_lower != df_mgus$age_upper))
  {
    p_mgus_weighted[ii] = sum(p_mgus[match(df_mgus$age_lower[ii], ages):match(df_mgus$age_upper[ii], ages)] * pop$N[match(df_mgus$age_lower[ii], pop$age):match(df_mgus$age_upper[ii], pop$age)]) /
      sum(pop$N[match(df_mgus$age_lower[ii], pop$age):match(df_mgus$age_upper[ii], pop$age)])
  }
  
  ll <- sum(dbinom(x = df_mgus$y, size = df_mgus$n, prob = p_mgus_weighted, log = T))
  return(ll)
}

#specify likelihood for mm incidence data
ll_mm_incid <- function(df_mm, i_mm, ages, pop)
{
  # compute the weighted incidence
  i_mm_weighted <- rep(NA, length(df_mm$age_lower))

  for(ii in 1:length(df_mm$age_lower))
  {
    i_mm_weighted[ii] = sum(i_mm[match(df_mm$age_lower[ii], ages):match(df_mm$age_upper[ii], ages)] * pop$N[match(df_mm$age_lower[ii], pop$age):match(df_mm$age_upper[ii], pop$age)]) /
      sum(pop$N[match(df_mm$age_lower[ii], pop$age):match(df_mm$age_upper[ii], pop$age)])
  }
  ll <- sum(dpois(x = df_mm$c, lambda = i_mm_weighted * df_mm$n, log = T))
  return(ll)
}

#specify likelihood for mm incidence trend data
ll_mm_incid_trend_spline <- function(df_mm_trend,
                              haz_trend_list,
                              i_mm_reference,
                              ages, 
                              pop_trend,
                              y_knots_period, 
                              y_knots_cohort,
                              weights_by_age_group)
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
  
  ll <- sum(dpois(x = df_mm_trend$c, lambda = i_mm_trend_weighted * df_mm_trend$n, log = T))
  return(ll)
}



#specify likelihood for mm incidence trend data
ll_mm_incid_trend <- function(df_mm_trend,
                              haz_trend_list,
                        i_mm_reference,
                        ages, 
                        pop_trend,
                        beta_period, 
                        beta_cohort,
                        beta_diagnosis,
                        weights_by_age_group)
{
  
  # compute the weighted incidence incorporating the period-cohort effects 
  i_mm_trend_weighted <- rep(NA, length(df_mm_trend$age_lower))
  
  # compute the period and cohort effects over time
  #period_effect <- period_effect_mm(y_knots_period = y_knots_period, x_knots_period = knots_x_period)
  #cohort_effect <- cohort_effect_mm(y_knots_cohort = y_knots_cohort, x_knots_cohort = knots_x_cohort)
  
  for(ii in 1:length(df_mm_trend$age_lower))
  {
    #pc_mm <- period_effect[df_mm_trend$year[ii] - min(knots_x_period) + 1] * cohort_effect[(df_mm_trend$year[ii] - df_mm_trend$age_lower[ii]:df_mm_trend$age_upper[ii]) - min(knots_x_cohort) + 1]
    
    
    period <- df_mm_trend$year[ii] - period_reference_year
    cohort <- df_mm_trend$year[ii] - df_mm_trend$age_lower[ii]:df_mm_trend$age_upper[ii] - cohort_reference_year
    diagnosis <- ifelse(df_mm_trend$year[ii] > diagnosis_reference_year, 1,0)
    pc_mm <- exp(beta_period * (period)) * exp(beta_cohort * (cohort)) * exp(beta_diagnosis * diagnosis)
    
    age_indices <- match(df_mm_trend$age_lower[ii], ages):match(df_mm_trend$age_upper[ii], ages)
    haz_mm_bmi <- haz_trend_list[[df_mm_trend$year[ii]]]$haz_mm_bmi[age_indices] / haz_trend_list[[data_reference_year]]$haz_mm_bmi[age_indices]
    i_mm_trend_weighted[ii] = sum(haz_mm_bmi * pc_mm * i_mm_reference[age_indices] * weights_by_age_group[[ii]])
      
  }
  
  ll <- sum(dpois(x = df_mm_trend$c, lambda = i_mm_trend_weighted * df_mm_trend$n, log = T))
  return(ll)
}


# specify ll function with sex-race interactions
ll_interactions_sex_race <- function(params)
{
  # get the parameters 
  gamma_mgus = params[1]
  beta_mgus_age = params[2]
  beta_mgus_female_nhw = params[3]
  beta_mgus_female_nhb = params[4]
  beta_mgus_male_nhb = params[5]
  gamma_mm = params[6]
  beta_mm_age = params[7]
  beta_mm_age_quad = params[8]
  beta_mm_female_nhw = params[9]
  beta_mm_female_nhb = params[10]
  beta_mm_male_nhb = params[11]
  
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
                            beta_mgus_race = beta_mgus_male_nhb,
                            gamma_mm = gamma_mm,
                            beta_mm_age = beta_mm_age,
                            beta_mm_age_quad = beta_mm_age_quad,
                            beta_mm_sex = 0,
                            beta_mm_race = beta_mm_male_nhb,
                            mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                            mort = mort_male_nhb,
                            mu_mm = mm_mu_nhb_male,
                            haz = haz_male_nhb)
  
  out_female_nhw <- sim_model(ages = ages,
                              gamma_mgus = gamma_mgus,
                              beta_mgus_age = beta_mgus_age,
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = 0,
                              beta_mgus_race = beta_mgus_female_nhw,
                              gamma_mm = gamma_mm,
                              beta_mm_age = beta_mm_age,
                              beta_mm_age_quad = beta_mm_age_quad,
                              beta_mm_sex = 0,
                              beta_mm_race = beta_mm_female_nhw,
                              mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                              mort = mort_female_nhw,
                              mu_mm = mm_mu_nhw_female,
                              haz = haz_female_nhw)
  
  out_female_nhb <- sim_model(ages = ages,
                              gamma_mgus = gamma_mgus,
                              beta_mgus_age = beta_mgus_age,
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = 0,
                              beta_mgus_race = beta_mgus_female_nhb,
                              gamma_mm = gamma_mm,
                              beta_mm_age = beta_mm_age,
                              beta_mm_age_quad = beta_mm_age_quad,
                              beta_mm_sex = 0,
                              beta_mm_race = beta_mm_female_nhb,
                              mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                              mort = mort_female_nhb,
                              mu_mm = mm_mu_nhb_female,
                              haz = haz_female_nhb)
  
  # calculate the log-likelihood
  ll <- ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhw']], ages = ages, p_mgus = out_male_nhw$p_mgus, pop = pop_male_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhb']], ages = ages, p_mgus = out_male_nhb$p_mgus, pop = pop_male_nhb)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhw']], ages = ages, p_mgus = out_female_nhw$p_mgus, pop = pop_female_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhb']], ages = ages, p_mgus = out_female_nhb$p_mgus, pop = pop_female_nhb)
  
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhw']], i_mm = out_male_nhw$i_mm, ages = ages, pop = pop_male_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhb']], i_mm = out_male_nhb$i_mm, ages = ages, pop = pop_male_nhb)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhw']], i_mm = out_female_nhw$i_mm, ages = ages, pop = pop_female_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhb']], i_mm = out_female_nhb$i_mm, ages = ages, pop = pop_female_nhb)
  
  # return log-likelihood
  if(is.infinite(ll) | is.na(ll)){ll <- -1e20}
  return(ll)
}

ll_interactions_sex_race_cmp <- cmpfun(ll_interactions_sex_race)

# specify ll function
ll <- function(params)
{
  # get the parameters 
  gamma_mgus = params[1]
  beta_mgus_age = params[2]
  beta_mgus_sex = params[3]
  beta_mgus_race = params[4]
  
  gamma_mm = params[5]
  beta_mm_age = params[6]
  beta_mm_age_quad = params[7]
  beta_mm_sex = params[8]
  beta_mm_race = params[9]
  
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
  
  # calculate the log-likelihood
  ll <- ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhw']], ages = ages, p_mgus = out_male_nhw$p_mgus, pop = pop_male_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhb']], ages = ages, p_mgus = out_male_nhb$p_mgus, pop = pop_male_nhb)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhw']], ages = ages, p_mgus = out_female_nhw$p_mgus, pop = pop_female_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhb']], ages = ages, p_mgus = out_female_nhb$p_mgus, pop = pop_female_nhb)
  
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhw']], i_mm = out_male_nhw$i_mm, ages = ages, pop = pop_male_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhb']], i_mm = out_male_nhb$i_mm, ages = ages, pop = pop_male_nhb)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhw']], i_mm = out_female_nhw$i_mm, ages = ages, pop = pop_female_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhb']], i_mm = out_female_nhb$i_mm, ages = ages, pop = pop_female_nhb)
  
  # return log-likelihood
  if(is.infinite(ll) | is.na(ll)){ll <- -1e20}
  return(ll)
}

ll_cmp <- cmpfun(ll)

# specify ll function
ll_with_trend <- function(params)
{
  # get the parameters 
  # gamma_mgus = params[1]
  # beta_mgus_age = params[2]
  # beta_mgus_sex = params[3]
  # beta_mgus_race = params[4]
  # 
  # gamma_mm = params[5]
  # beta_mm_age = params[6]
  # beta_mm_age_quad = params[7]
  # beta_mm_sex = params[8]
  # beta_mm_race = params[9]
  
  # get period and cohort knots
  #y_knots_period <- rep(0, length(knots_x_period))
  #y_knots_cohort <- rep(0, length(knots_x_cohort))
  beta_period = params[1]
  beta_cohort = params[2]
  beta_diagnosis = params[3]
  
  #_knots_period[-which(knots_x_period == period_reference_year)] <- head(params, length(knots_x_period) - 1)
  #y_knots_cohort[-which(knots_x_cohort == cohort_reference_year)] <- tail(params, length(knots_x_cohort) - 1)
  
  # # simulate model
  # out_male_nhw <- sim_model(ages = ages,
  #                           gamma_mgus = gamma_mgus,
  #                           beta_mgus_age = beta_mgus_age,
  #                           beta_mgus_age_quad = 0,
  #                           beta_mgus_sex = 0,
  #                           beta_mgus_race = 0,
  #                           gamma_mm = gamma_mm,
  #                           beta_mm_age = beta_mm_age,
  #                           beta_mm_age_quad = beta_mm_age_quad,
  #                           beta_mm_sex = 0,
  #                           beta_mm_race = 0,
  #                           mgus_mortality_multiplier = mgus_mortality_multiplier_male,
  #                           mort = mort_male_nhw,
  #                           mu_mm = mm_mu_nhw_male,
  #                           haz = haz_male_nhw)
  # 
  # out_male_nhb <- sim_model(ages = ages,
  #                           gamma_mgus = gamma_mgus,
  #                           beta_mgus_age = beta_mgus_age,
  #                           beta_mgus_age_quad = 0,
  #                           beta_mgus_sex = 0,
  #                           beta_mgus_race = beta_mgus_race,
  #                           gamma_mm = gamma_mm,
  #                           beta_mm_age = beta_mm_age,
  #                           beta_mm_age_quad = beta_mm_age_quad,
  #                           beta_mm_sex = 0,
  #                           beta_mm_race = beta_mm_race,
  #                           mgus_mortality_multiplier = mgus_mortality_multiplier_male,
  #                           mort = mort_male_nhb,
  #                           mu_mm = mm_mu_nhb_male,
  #                           haz = haz_male_nhb)
  # 
  # out_female_nhw <- sim_model(ages = ages,
  #                             gamma_mgus = gamma_mgus,
  #                             beta_mgus_age = beta_mgus_age,
  #                             beta_mgus_age_quad = 0,
  #                             beta_mgus_sex = beta_mgus_sex,
  #                             beta_mgus_race = 0,
  #                             gamma_mm = gamma_mm,
  #                             beta_mm_age = beta_mm_age,
  #                             beta_mm_age_quad = beta_mm_age_quad,
  #                             beta_mm_sex = beta_mm_sex,
  #                             beta_mm_race = 0,
  #                             mgus_mortality_multiplier = mgus_mortality_multiplier_female,
  #                             mort = mort_female_nhw,
  #                             mu_mm = mm_mu_nhw_female,
  #                             haz = haz_female_nhw)
  # 
  # out_female_nhb <- sim_model(ages = ages,
  #                             gamma_mgus = gamma_mgus,
  #                             beta_mgus_age = beta_mgus_age,
  #                             beta_mgus_age_quad = 0,
  #                             beta_mgus_sex = beta_mgus_sex,
  #                             beta_mgus_race = beta_mgus_race,
  #                             gamma_mm = gamma_mm,
  #                             beta_mm_age = beta_mm_age,
  #                             beta_mm_age_quad = beta_mm_age_quad,
  #                             beta_mm_sex = beta_mm_sex,
  #                             beta_mm_race = beta_mm_race,
  #                             mgus_mortality_multiplier = mgus_mortality_multiplier_female,
  #                             mort = mort_female_nhb,
  #                             mu_mm = mm_mu_nhb_female,
  #                             haz = haz_female_nhb)
  
  # calculate the log-likelihood
  #ll <- ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhw']], ages = ages, p_mgus = out_male_nhw$p_mgus, pop = pop_male_nhw)
  #ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhb']], ages = ages, p_mgus = out_male_nhb$p_mgus, pop = pop_male_nhb)
  #ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhw']], ages = ages, p_mgus = out_female_nhw$p_mgus, pop = pop_female_nhw)
  #ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhb']], ages = ages, p_mgus = out_female_nhb$p_mgus, pop = pop_female_nhb)
  
  #ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhw']], i_mm = out_male_nhw$i_mm, ages = ages, pop = pop_male_nhw)
  #ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhb']], i_mm = out_male_nhb$i_mm, ages = ages, pop = pop_male_nhb)
  #ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhw']], i_mm = out_female_nhw$i_mm, ages = ages, pop = pop_female_nhw)
  #ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhb']], i_mm = out_female_nhb$i_mm, ages = ages, pop = pop_female_nhb)
  
  ll <- 0
  ll <- ll + ll_mm_incid_trend(df_mm_trend = data_list[['mm_trend_incid_male_nhw']], haz_trend_list = haz_trend_male_nhw_list, i_mm_reference = out_male_nhw$i_mm, ages = ages, pop_trend = pop_trend_male_nhw,
                               beta_period = beta_period, beta_cohort = beta_cohort, beta_diagnosis = beta_diagnosis,
                               weights_by_age_group = weights_by_age_group_male_nhw)
  ll <- ll + ll_mm_incid_trend(df_mm_trend = data_list[['mm_trend_incid_male_nhb']], haz_trend_list = haz_trend_male_nhb_list, i_mm_reference = out_male_nhb$i_mm, ages = ages, pop_trend = pop_trend_male_nhb,
                               beta_period = beta_period, beta_cohort = beta_cohort, beta_diagnosis = beta_diagnosis,
                               weights_by_age_group = weights_by_age_group_male_nhb)
  ll <- ll + ll_mm_incid_trend(df_mm_trend = data_list[['mm_trend_incid_female_nhw']], haz_trend_list = haz_trend_female_nhw_list, i_mm_reference = out_female_nhw$i_mm, ages = ages, pop_trend = pop_trend_female_nhw,
                               beta_period = beta_period, beta_cohort = beta_cohort, beta_diagnosis = beta_diagnosis,
                               weights_by_age_group = weights_by_age_group_female_nhw)
  ll <- ll + ll_mm_incid_trend(df_mm_trend = data_list[['mm_trend_incid_female_nhb']], haz_trend_list = haz_trend_female_nhb_list, i_mm_reference = out_female_nhb$i_mm, ages = ages, pop_trend = pop_trend_female_nhb,
                               beta_period = beta_period, beta_cohort = beta_cohort, beta_diagnosis = beta_diagnosis, 
                               weights_by_age_group = weights_by_age_group_female_nhb)
  
  # return log-likelihood
  if(is.infinite(ll) | is.na(ll)){ll <- -1e20}
  return(ll)
}

ll_with_trend_cmp <- cmpfun(ll_with_trend)



# specify ll function
ll_with_trend_spline <- function(params)
{
  # get period and cohort knots
  y_knots_period <- rep(0, length(knots_x_period))
  y_knots_cohort <- rep(0, length(knots_x_cohort))
  #beta_diagnosis = params[1]
  
  y_knots_period[-which(knots_x_period == period_reference_year)] <- head(params, length(knots_x_period) - 1)
  y_knots_cohort[-which(knots_x_cohort == cohort_reference_year)] <- tail(params, length(knots_x_cohort) - 1)

  ll <- 0
  ll <- ll + ll_mm_incid_trend_spline(df_mm_trend = data_list[['mm_trend_incid_male_nhw']], haz_trend_list = haz_trend_male_nhw_list, i_mm_reference = out_male_nhw$i_mm, ages = ages, pop_trend = pop_trend_male_nhw,
                               y_knots_period = y_knots_period, y_knots_cohort = y_knots_cohort,
                               weights_by_age_group = weights_by_age_group_male_nhw)
  ll <- ll + ll_mm_incid_trend_spline(df_mm_trend = data_list[['mm_trend_incid_male_nhb']], haz_trend_list = haz_trend_male_nhb_list, i_mm_reference = out_male_nhb$i_mm, ages = ages, pop_trend = pop_trend_male_nhb,
                               y_knots_period = y_knots_period, y_knots_cohort = y_knots_cohort,
                               weights_by_age_group = weights_by_age_group_male_nhb)
  ll <- ll + ll_mm_incid_trend_spline(df_mm_trend = data_list[['mm_trend_incid_female_nhw']], haz_trend_list = haz_trend_female_nhw_list, i_mm_reference = out_female_nhw$i_mm, ages = ages, pop_trend = pop_trend_female_nhw,
                               y_knots_period = y_knots_period, y_knots_cohort = y_knots_cohort,
                               weights_by_age_group = weights_by_age_group_female_nhw)
  ll <- ll + ll_mm_incid_trend_spline(df_mm_trend = data_list[['mm_trend_incid_female_nhb']], haz_trend_list = haz_trend_female_nhb_list, i_mm_reference = out_female_nhb$i_mm, ages = ages, pop_trend = pop_trend_female_nhb,
                               y_knots_period = y_knots_period, y_knots_cohort = y_knots_cohort,
                               weights_by_age_group = weights_by_age_group_female_nhb)
  
  # return log-likelihood
  if(is.infinite(ll) | is.na(ll)){ll <- -1e20}
  return(ll)
}

ll_with_trend_spline_cmp <- cmpfun(ll_with_trend_spline)

