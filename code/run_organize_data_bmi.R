# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(openxlsx)){install.packages('openxlsx'); library(openxlsx)}

# load data structures
load('../output/Huber_NatComms_fig_S1.RData')

# load population data and subset
pop <- read.csv('../data/Population_Data_Disaggregated.csv')
pop <- subset(pop, year == 2004)

pop_male_nhw <- subset(pop, gender == 'Male' & race == 'Non_Hispanic_White')
pop_female_nhw <- subset(pop, gender == 'Female' & race == 'Non_Hispanic_White')
pop_male_nhb <- subset(pop, gender == 'Male' & race == 'Non_Hispanic_Black')
pop_female_nhb <- subset(pop, gender == 'Female' & race == 'Non_Hispanic_Black')

pop_trend <- read.csv('../data/Population_Data_Disaggregated.csv')

pop_trend_male_nhw <- subset(pop_trend, gender == 'Male' & race == 'Non_Hispanic_White')
pop_trend_female_nhw <- subset(pop_trend, gender == 'Female' & race == 'Non_Hispanic_White')
pop_trend_male_nhb <- subset(pop_trend, gender == 'Male' & race == 'Non_Hispanic_Black')
pop_trend_female_nhb <- subset(pop_trend, gender == 'Female' & race == 'Non_Hispanic_Black')

# load mortality data
mort <- read.xlsx('../data/CDC_Life_Tables.xlsx')
mort <- subset(mort, Age < 100 & Year == 2004)

mort_male_nhw <- subset(mort, Sex == 'Male' & Race == 'Non_Hispanic_White')
mort_female_nhw <- subset(mort, Sex == 'Female' & Race == 'Non_Hispanic_White')
mort_male_nhb <- subset(mort, Sex == 'Male' & Race == 'Non_Hispanic_Black')
mort_female_nhb <- subset(mort, Sex == 'Female' & Race == 'Non_Hispanic_Black')

mort_male_nhw <- data.frame(age = mort_male_nhw$Age, 
                            mu = -log(1 - mort_male_nhw$Death_Prob))
mort_female_nhw <- data.frame(age = mort_female_nhw$Age, 
                              mu = -log(1 - mort_female_nhw$Death_Prob))
mort_male_nhb <- data.frame(age = mort_male_nhb$Age, 
                            mu = -log(1 - mort_male_nhb$Death_Prob))
mort_female_nhb <- data.frame(age = mort_female_nhb$Age, 
                              mu = -log(1 - mort_female_nhb$Death_Prob))

# specify the mgus mortality multipliers
mgus_mortality_multiplier_male = 1.25
mgus_mortality_multiplier_female = 1.11

# specify ages
ages = 0:99

# load the epi data for single year analysis
mgus_prev <- read.csv('../data/MGUS_Data_Reformatted_BMI.csv')

mgus_prev_male_nhw_uw <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_White' & bmi == 'uw')
mgus_prev_male_nhw_nw <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_White' & bmi == 'nw')
mgus_prev_male_nhw_ow <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_White' & bmi == 'ow')
mgus_prev_male_nhw_ob <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_White' & bmi == 'ob')

mgus_prev_female_nhw_uw <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_White' & bmi == 'uw')
mgus_prev_female_nhw_nw <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_White' & bmi == 'nw')
mgus_prev_female_nhw_ow <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_White' & bmi == 'ow')
mgus_prev_female_nhw_ob <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_White' & bmi == 'ob')

mgus_prev_male_nhb_uw <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_Black' & bmi == 'uw')
mgus_prev_male_nhb_nw <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_Black' & bmi == 'nw')
mgus_prev_male_nhb_ow <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_Black' & bmi == 'ow')
mgus_prev_male_nhb_ob <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_Black' & bmi == 'ob')

mgus_prev_female_nhb_uw <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_Black' & bmi == 'uw')
mgus_prev_female_nhb_nw <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_Black' & bmi == 'nw')
mgus_prev_female_nhb_ow <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_Black' & bmi == 'ow')
mgus_prev_female_nhb_ob <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_Black' & bmi == 'ob')

mm_incid <- read.xlsx('../data/SEER9_MM_Trend.xlsx')
mm_incid <- subset(mm_incid, Year == 2004)
mm_incid <- data.frame(age_lower = mm_incid$Age_Lower,
                       age_upper = mm_incid$Age_Upper,
                       sex = mm_incid$Sex,
                       race = mm_incid$Race,
                       x = mm_incid$MM_Incidence_Mean,
                       x_lower = mm_incid$MM_Incidence_Lower,
                       x_upper = mm_incid$MM_Incidence_Upper,
                       c = mm_incid$MM_Count,
                       n = mm_incid$Sample_Size)

mm_incid$x <- mm_incid$x / 1e5
mm_incid$x_lower = mm_incid$x_lower / 1e5
mm_incid$x_upper = mm_incid$x_upper / 1e5
mm_incid <- subset(mm_incid, age_lower >= 0)

mm_incid_male_nhw <- subset(mm_incid, sex == 'Male' & race == 'White')
mm_incid_female_nhw <- subset(mm_incid, sex == 'Female' & race == 'White')
mm_incid_male_nhb <- subset(mm_incid, sex == 'Male' & race == 'Black')
mm_incid_female_nhb <- subset(mm_incid, sex == 'Female' & race == 'Black')

# load the epi data for apc analysis 
mm_incid_trend <- read.xlsx('../data/SEER9_MM_Trend.xlsx')
mm_incid_trend <- data.frame(age_lower = mm_incid_trend$Age_Lower,
                             age_upper = mm_incid_trend$Age_Upper,
                             year = mm_incid_trend$Year,
                             sex = mm_incid_trend$Sex,
                             race = mm_incid_trend$Race,
                             x = mm_incid_trend$MM_Incidence_Mean,
                             x_lower = mm_incid_trend$MM_Incidence_Lower,
                             x_upper = mm_incid_trend$MM_Incidence_Upper,
                             c = mm_incid_trend$MM_Count,
                             n = mm_incid_trend$Sample_Size)

yrs_train <- seq(from = 1976, to = 2018, by = 2)
mm_incid_trend <- subset(mm_incid_trend, year %in% yrs_train)

mm_incid_trend$x <- mm_incid_trend$x / 1e5
mm_incid_trend$x_lower <- mm_incid_trend$x_lower / 1e5
mm_incid_trend$x_upper <- mm_incid_trend$x_upper / 1e5
mm_incid_trend <- subset(mm_incid_trend, age_lower >= 40)

mm_trend_incid_male_nhw <- subset(mm_incid_trend, sex == 'Male' & race == 'White')
mm_trend_incid_female_nhw <- subset(mm_incid_trend, sex == 'Female' & race == 'White')
mm_trend_incid_male_nhb <- subset(mm_incid_trend, sex == 'Male' & race == 'Black')
mm_trend_incid_female_nhb <- subset(mm_incid_trend, sex == 'Female' & race == 'Black')

# construct data list 
data_list = list(mgus_prev_male_nhw_uw = mgus_prev_male_nhw_uw,
                 mgus_prev_male_nhw_nw = mgus_prev_male_nhw_nw,
                 mgus_prev_male_nhw_ow = mgus_prev_male_nhw_ow,
                 mgus_prev_male_nhw_ob = mgus_prev_male_nhw_ob,
                 mgus_prev_female_nhw_uw = mgus_prev_female_nhw_uw,
                 mgus_prev_female_nhw_nw = mgus_prev_female_nhw_nw,
                 mgus_prev_female_nhw_ow = mgus_prev_female_nhw_ow,
                 mgus_prev_female_nhw_ob = mgus_prev_female_nhw_ob,
                 mgus_prev_male_nhb_uw = mgus_prev_male_nhb_uw,
                 mgus_prev_male_nhb_nw = mgus_prev_male_nhb_nw,
                 mgus_prev_male_nhb_ow = mgus_prev_male_nhb_ow,
                 mgus_prev_male_nhb_ob = mgus_prev_male_nhb_ob,
                 mgus_prev_female_nhb_uw = mgus_prev_female_nhb_uw,
                 mgus_prev_female_nhb_nw = mgus_prev_female_nhb_nw,
                 mgus_prev_female_nhb_ow = mgus_prev_female_nhb_ow,
                 mgus_prev_female_nhb_ob = mgus_prev_female_nhb_ob,
                 mm_incid_male_nhw = mm_incid_male_nhw,
                 mm_incid_female_nhw = mm_incid_female_nhw,
                 mm_incid_male_nhb = mm_incid_male_nhb,
                 mm_incid_female_nhb = mm_incid_female_nhb,
                 mm_trend_incid_male_nhw = mm_trend_incid_male_nhw,
                 mm_trend_incid_female_nhw = mm_trend_incid_female_nhw,
                 mm_trend_incid_male_nhb = mm_trend_incid_male_nhb,
                 mm_trend_incid_female_nhb = mm_trend_incid_female_nhb)

# calculate the weight by age group for each gender/race pairing 
weights_by_age_group_male_nhw <- list()
weights_by_age_group_female_nhw <- list()
weights_by_age_group_male_nhb <- list()
weights_by_age_group_female_nhb <- list()

for(ii in 1:nrow(data_list$mm_trend_incid_male_nhw))
{
  age_vec <- data_list$mm_trend_incid_male_nhw$age_lower[ii]:data_list$mm_trend_incid_male_nhw$age_upper[ii]
  pop_subset <- subset(pop_trend_male_nhw, year == data_list$mm_trend_incid_male_nhw$year[ii])
  pop_vec <- pop_subset$N[match(age_vec, pop_subset$age)]
  weights_by_age_group_male_nhw[[ii]] <- pop_vec / sum(pop_vec)
}

for(ii in 1:nrow(data_list$mm_trend_incid_female_nhw))
{
  age_vec <- data_list$mm_trend_incid_female_nhw$age_lower[ii]:data_list$mm_trend_incid_female_nhw$age_upper[ii]
  pop_subset <- subset(pop_trend_female_nhw, year == data_list$mm_trend_incid_female_nhw$year[ii])
  pop_vec <- pop_subset$N[match(age_vec, pop_subset$age)]
  weights_by_age_group_female_nhw[[ii]] <- pop_vec / sum(pop_vec)
}


for(ii in 1:nrow(data_list$mm_trend_incid_male_nhb))
{
  age_vec <- data_list$mm_trend_incid_male_nhb$age_lower[ii]:data_list$mm_trend_incid_male_nhb$age_upper[ii]
  pop_subset <- subset(pop_trend_male_nhb, year == data_list$mm_trend_incid_male_nhb$year[ii])
  pop_vec <- pop_subset$N[match(age_vec, pop_subset$age)]
  weights_by_age_group_male_nhb[[ii]] <- pop_vec / sum(pop_vec)
}

for(ii in 1:nrow(data_list$mm_trend_incid_female_nhb))
{
  age_vec <- data_list$mm_trend_incid_female_nhb$age_lower[ii]:data_list$mm_trend_incid_female_nhb$age_upper[ii]
  pop_subset <- subset(pop_trend_female_nhb, year == data_list$mm_trend_incid_female_nhb$year[ii])
  pop_vec <- pop_subset$N[match(age_vec, pop_subset$age)]
  weights_by_age_group_female_nhb[[ii]] <- pop_vec / sum(pop_vec)
}

###
#construct the hazard of BMI prevalence across 198
### 
haz_trend_male_nhw_list <- list()
haz_trend_female_nhw_list <- list()
haz_trend_male_nhb_list <- list()
haz_trend_female_nhb_list <- list()


haz_male_nhw <- data.frame(age = 0:99,
                           haz_mgus_bmi = 1,
                           haz_mm_bmi = 1)
haz_female_nhw <- haz_male_nhw
haz_male_nhb <- haz_male_nhw
haz_female_nhb <- haz_male_nhw

# load the bmi prevalence data 
bmi_prev <- read.xlsx('../data/BRFSS_BMI_1987_2021.xlsx')
bmi_prev_male_nhw <- subset(bmi_prev, Gender == 'Male' & Race == 'Non_Hispanic_White')
bmi_prev_female_nhw <- subset(bmi_prev, Gender == 'Female' & Race == 'Non_Hispanic_White')
bmi_prev_male_nhb <- subset(bmi_prev, Gender == 'Male' & Race == 'Non_Hispanic_Black')
bmi_prev_female_nhb <- subset(bmi_prev, Gender == 'Female' & Race == 'Non_Hispanic_Black')

# specify the hazard for development of mgus from Ji et al
haz_mgus_under = 1
haz_mgus_normal = 1
haz_mgus_over = 1.32
haz_mgus_obese = 1.60

# specify the hazard for progression from mgus to mm from Chang et al
haz_mm_under = 1
haz_mm_normal = 1
haz_mm_over = 1.55
haz_mm_obese = 1.98

# loop through the years of data and construct the hazards 
for(year in 1975:2018)
{
  if(year < 1987)
  {
    bmi_prev_male_nhw_subset <- subset(bmi_prev_male_nhw, Year == 1987)
    bmi_prev_female_nhw_subset <- subset(bmi_prev_female_nhw, Year == 1987)
    bmi_prev_male_nhb_subset <- subset(bmi_prev_male_nhb, Year == 1987)
    bmi_prev_female_nhb_subset <- subset(bmi_prev_female_nhb, Year == 1987)
  }else{
    bmi_prev_male_nhw_subset <- subset(bmi_prev_male_nhw, Year == year)
    bmi_prev_female_nhw_subset <- subset(bmi_prev_female_nhw, Year == year)
    bmi_prev_male_nhb_subset <- subset(bmi_prev_male_nhb, Year == year)
    bmi_prev_female_nhb_subset <- subset(bmi_prev_female_nhb, Year == year)
  }
  
  for(ii in 1:nrow(bmi_prev_male_nhw_subset))
  {
    ## FIX THE COLUMN NAME OF BMI PREV FOR AGE_LOWER AND AGE_UPPER - THEY ARE SWITCHED
    haz_male_nhw$haz_mgus_bmi[match(bmi_prev_male_nhw_subset$Age_Upper[ii]:bmi_prev_male_nhw_subset$Age_Lower[ii],haz_male_nhw$age)] = 
      bmi_prev_male_nhw_subset$Prev_Normal[ii] * haz_mgus_normal + bmi_prev_male_nhw_subset$Prev_Under[ii] * haz_mgus_under + 
      bmi_prev_male_nhw_subset$Prev_Over[ii] * haz_mgus_over + bmi_prev_male_nhw_subset$Prev_Obese[ii] * haz_mgus_obese
    
    haz_female_nhw$haz_mgus_bmi[match(bmi_prev_female_nhw_subset$Age_Upper[ii]:bmi_prev_female_nhw_subset$Age_Lower[ii],haz_female_nhw$age)] = 
      bmi_prev_female_nhw_subset$Prev_Normal[ii] * haz_mgus_normal + bmi_prev_female_nhw_subset$Prev_Under[ii] * haz_mgus_under + 
      bmi_prev_female_nhw_subset$Prev_Over[ii] * haz_mgus_over + bmi_prev_female_nhw_subset$Prev_Obese[ii] * haz_mgus_obese
    
    haz_male_nhb$haz_mgus_bmi[match(bmi_prev_male_nhb_subset$Age_Upper[ii]:bmi_prev_male_nhb_subset$Age_Lower[ii],haz_male_nhb$age)] = 
      bmi_prev_male_nhb_subset$Prev_Normal[ii] * haz_mgus_normal + bmi_prev_male_nhb_subset$Prev_Under[ii] * haz_mgus_under + 
      bmi_prev_male_nhb_subset$Prev_Over[ii] * haz_mgus_over + bmi_prev_male_nhb_subset$Prev_Obese[ii] * haz_mgus_obese
    
    haz_female_nhb$haz_mgus_bmi[match(bmi_prev_female_nhb_subset$Age_Upper[ii]:bmi_prev_female_nhb_subset$Age_Lower[ii],haz_female_nhb$age)] = 
      bmi_prev_female_nhb_subset$Prev_Normal[ii] * haz_mgus_normal + bmi_prev_female_nhb_subset$Prev_Under[ii] * haz_mgus_under + 
      bmi_prev_female_nhb_subset$Prev_Over[ii] * haz_mgus_over + bmi_prev_female_nhb_subset$Prev_Obese[ii] * haz_mgus_obese
    
    # Progression from MGUS to MM
    haz_male_nhw$haz_mm_bmi[match(bmi_prev_male_nhw_subset$Age_Upper[ii]:bmi_prev_male_nhw_subset$Age_Lower[ii],haz_male_nhw$age)] = 
      bmi_prev_male_nhw_subset$Prev_Normal[ii] * haz_mm_normal + bmi_prev_male_nhw_subset$Prev_Under[ii] * haz_mm_under + 
      bmi_prev_male_nhw_subset$Prev_Over[ii] * haz_mm_over + bmi_prev_male_nhw_subset$Prev_Obese[ii] * haz_mm_obese
    
    haz_female_nhw$haz_mm_bmi[match(bmi_prev_female_nhw_subset$Age_Upper[ii]:bmi_prev_female_nhw_subset$Age_Lower[ii],haz_female_nhw$age)] = 
      bmi_prev_female_nhw_subset$Prev_Normal[ii] * haz_mm_normal + bmi_prev_female_nhw_subset$Prev_Under[ii] * haz_mm_under + 
      bmi_prev_female_nhw_subset$Prev_Over[ii] * haz_mm_over + bmi_prev_female_nhw_subset$Prev_Obese[ii] * haz_mm_obese
    
    haz_male_nhb$haz_mm_bmi[match(bmi_prev_male_nhb_subset$Age_Upper[ii]:bmi_prev_male_nhb_subset$Age_Lower[ii],haz_male_nhb$age)] = 
      bmi_prev_male_nhb_subset$Prev_Normal[ii] * haz_mm_normal + bmi_prev_male_nhb_subset$Prev_Under[ii] * haz_mm_under + 
      bmi_prev_male_nhb_subset$Prev_Over[ii] * haz_mm_over + bmi_prev_male_nhb_subset$Prev_Obese[ii] * haz_mm_obese
    
    haz_female_nhb$haz_mm_bmi[match(bmi_prev_female_nhb_subset$Age_Upper[ii]:bmi_prev_female_nhb_subset$Age_Lower[ii],haz_female_nhb$age)] = 
      bmi_prev_female_nhb_subset$Prev_Normal[ii] * haz_mm_normal + bmi_prev_female_nhb_subset$Prev_Under[ii] * haz_mm_under + 
      bmi_prev_female_nhb_subset$Prev_Over[ii] * haz_mm_over + bmi_prev_female_nhb_subset$Prev_Obese[ii] * haz_mm_obese
  }
  
  haz_trend_male_nhw_list[[year]] <- haz_male_nhw
  haz_trend_female_nhw_list[[year]] <- haz_female_nhw
  haz_trend_male_nhb_list[[year]] <- haz_male_nhb
  haz_trend_female_nhb_list[[year]] <- haz_female_nhb
}

# store the hazards from 2004 in a separate dataframe
haz_male_nhw <- haz_trend_male_nhw_list[[2004]]
haz_female_nhw <- haz_trend_female_nhw_list[[2004]]
haz_male_nhb <- haz_trend_male_nhb_list[[2004]]
haz_female_nhb <- haz_trend_female_nhb_list[[2004]]

# hazards by bmi 
haz_uw <- data.frame(age = ages, 
                     haz_mgus_bmi = haz_mgus_under, 
                     haz_mm_bmi = haz_mm_under)
haz_nw <- data.frame(age = ages, 
                     haz_mgus_bmi = haz_mgus_normal, 
                     haz_mm_bmi = haz_mm_normal)
haz_ow <- data.frame(age = ages, 
                     haz_mgus_bmi = haz_mgus_over, 
                     haz_mm_bmi = haz_mm_over)
haz_ob <- data.frame(age = ages, 
                     haz_mgus_bmi = haz_mgus_obese, 
                     haz_mm_bmi = haz_mm_obese)

# save output to file 
save(data_list,
     haz_uw,
     haz_nw,
     haz_ow,
     haz_ob,
     mort_male_nhw,
     mort_male_nhb,
     mort_female_nhw,
     mort_female_nhb,
     pop_male_nhw,
     pop_male_nhb,
     pop_female_nhw,
     pop_female_nhb,
     ages,
     mgus_mortality_multiplier_male,
     mgus_mortality_multiplier_female,
     mm_mu_nhw_male,
     mm_mu_nhb_male,
     mm_mu_nhw_female,
     mm_mu_nhb_female,
     file = '../output/data_organized_bmi.RData')
