# install necessary packages
library(deSolve)

# function specifying model
model <- function(t, x, params, mort, haz)
{
  # get states 
  H <- x[1]
  MGUS <- x[2]
  MM <- x[3]
  D_H <- x[4]
  D_MGUS <- x[5]
  D_MM <- x[6]
  
  # get params
  gamma_mgus = params['gamma_mgus']
  beta_mgus_age = params['beta_mgus_age']
  beta_mgus_age_quad = params['beta_mgus_age_quad']
  beta_mgus_sex = params['beta_mgus_sex']
  beta_mgus_race = params['beta_mgus_race']
  gamma_mm = params['gamma_mm']
  beta_mm_age = params['beta_mm_age']
  beta_mm_age_quad = params['beta_mm_age_quad']
  beta_mm_sex = params['beta_mm_sex']
  beta_mm_race = params['beta_mm_race']
  mgus_mortality_multiplier = params['mgus_mortality_multiplier']
  
  # construct the rates as a function of age
  haz_mgus_bmi = haz$haz_mgus_bmi[match(t,haz$age)]
  haz_mm_bmi = haz$haz_mm_bmi[match(t,haz$age)]
  
  lambda_mgus = haz_mgus_bmi * exp(gamma_mgus + beta_mgus_age * t + beta_mgus_age_quad * (t^2) + beta_mgus_sex + beta_mgus_race)
  lambda_mm = haz_mm_bmi * exp(gamma_mm + beta_mm_age * t + beta_mm_age_quad * (t^2) + beta_mm_sex + beta_mm_race)
  
  mu_h = mort$mu[match(t,mort$age)]
  mu_mgus = mgus_mortality_multiplier * mu_h
  mu_mm = params['mu_mm']
  
  # specify the model 
  dH = -lambda_mgus * H - mu_h * H
  dMGUS = lambda_mgus * H - lambda_mm * MGUS - mu_mgus * MGUS
  dMM = lambda_mm * MGUS - mu_mm * MM
  dD_H = mu_h * H
  dD_MGUS = mu_mgus * MGUS
  dD_MM = mu_mm * MM
  
  # return
  list(c(dH, dMGUS, dMM, dD_H, dD_MGUS, dD_MM))
}

# function to run ode
sim_ode <- function(ages,
                      gamma_mgus,
                      beta_mgus_age,
                      beta_mgus_age_quad,
                      beta_mgus_sex,
                      beta_mgus_race,
                      gamma_mm,
                      beta_mm_age,
                      beta_mm_age_quad,
                      beta_mm_sex,
                      beta_mm_race,
                      mgus_mortality_multiplier,
                      mort,
                      mu_mm,
                      haz,
                      xstart = c(H = 1, MGUS = 0, MM = 0, D_H = 0, D_MGUS = 0, D_MM = 0))
{
  # run simulation 
  params <- c(gamma_mgus = gamma_mgus, beta_mgus_age = beta_mgus_age, beta_mgus_age_quad = beta_mgus_age_quad, beta_mgus_sex = beta_mgus_sex, beta_mgus_race = beta_mgus_race,
              gamma_mm = gamma_mm, beta_mm_age = beta_mm_age, beta_mm_age_quad = beta_mm_age_quad, beta_mm_sex = beta_mm_sex, beta_mm_race = beta_mm_race,
              mgus_mortality_multiplier = mgus_mortality_multiplier, mu_mm = mu_mm)
  
  #xstart <- c(H = 1, MGUS = 0, MM = 0, D_H = 0, D_MGUS = 0, D_MM = 0)
  times <- seq(from = min(ages), to = max(ages), by = 1)
  out <- ode(y = xstart, time = times, func = model, parms = params, method = 'euler', mort = mort, haz = haz)
  
  # get relevant output 
  indices <- match(ages, out[,'time'])
  
  P_H = out[indices,'H']
  P_MGUS = out[indices,'MGUS']
  P_MM = out[indices,'MM']
  P_D_H = out[indices, 'D_H']
  P_D_MGUS = out[indices, 'D_MGUS']
  P_D_MM = out[indices, 'D_MM']

  # organize output and return
  df <- data.frame(age = ages,
                   P_H = P_H,
                   P_MGUS = P_MGUS,
                   P_MM = P_MM,
                   P_D = P_D_H + P_D_MGUS + P_D_MM)
  return(df)
}


# function to simulate model 
sim_model <- function(ages,
                      gamma_mgus,
                      beta_mgus_age,
                      beta_mgus_age_quad,
                      beta_mgus_sex,
                      beta_mgus_race,
                      gamma_mm,
                      beta_mm_age,
                      beta_mm_age_quad,
                      beta_mm_sex,
                      beta_mm_race,
                      mgus_mortality_multiplier,
                      mort,
                      mu_mm,
                      haz,
                      xstart = c(H = 1, MGUS = 0, MM = 0, D_H = 0, D_MGUS = 0, D_MM = 0))
{
  # run simulation 
  params <- c(gamma_mgus = gamma_mgus, beta_mgus_age = beta_mgus_age, beta_mgus_age_quad = beta_mgus_age_quad, beta_mgus_sex = beta_mgus_sex, beta_mgus_race = beta_mgus_race,
              gamma_mm = gamma_mm, beta_mm_age = beta_mm_age, beta_mm_age_quad = beta_mm_age_quad, beta_mm_sex = beta_mm_sex, beta_mm_race = beta_mm_race,
              mgus_mortality_multiplier = mgus_mortality_multiplier, mu_mm = mu_mm)
  
  times <- seq(from = min(ages), to = max(ages), by = 1)
  out <- ode(y = xstart, time = times, func = model, parms = params, method = 'euler', mort = mort, haz = haz)
  
  # get relevant output 
  indices <- match(ages, out[,'time'])
  
  P_H = out[indices,'H']
  P_MGUS = out[indices,'MGUS']
  P_MM = out[indices,'MM']
  P_D_H = out[indices, 'D_H']
  P_D_MGUS = out[indices, 'D_MGUS']
  P_D_MM = out[indices, 'D_MM']
  
  # calculate incidence and prevalence
  p_h = P_H / (P_H + P_MGUS + P_MM)
  p_mgus = P_MGUS / (P_H + P_MGUS + P_MM)
  p_mm = P_MM / (P_H + P_MGUS + P_MM)
  
  haz_mgus_bmi = haz$haz_mgus_bmi
  haz_mm_bmi = haz$haz_mm_bmi
  
  lambda_mgus = haz_mgus_bmi * exp(gamma_mgus + beta_mgus_age * ages + beta_mgus_age_quad * (ages^ 2) + beta_mgus_sex + beta_mgus_race)
  i_mgus = lambda_mgus * p_h
  
  lambda_mm = haz_mm_bmi * exp(gamma_mm + beta_mm_age * ages + beta_mm_age_quad * (ages ^ 2) + beta_mm_sex + beta_mm_race)
  i_mm = lambda_mm * p_mgus
  rate_mm = lambda_mm * P_MGUS
  
  # compute cumulative lifetime risk of developing mgus and mm
  cum_prop_mgus <- P_MGUS + P_D_MGUS + P_MM + P_D_MM
  cum_prop_mm <- P_MM + P_D_MM
  
  # organize output and return
  df <- data.frame(age = ages,
                   p_mgus = p_mgus,
                   p_mm = p_mm,
                   i_mgus = i_mgus,
                   i_mm = i_mm,
                   rate_mm = rate_mm,
                   cum_prop_mgus = cum_prop_mgus,
                   cum_prop_mm = cum_prop_mm,
                   P_H = P_H,
                   P_MGUS = P_MGUS,
                   P_MM = P_MM,
                   P_D_H = P_D_H,
                   P_D_MGUS = P_D_MGUS,
                   P_D_MM = P_D_MM)
  return(df)
}


