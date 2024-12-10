# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(openxlsx)){install.packages('openxlsx'); library(openxlsx)}

# load the data 
df <- read.xlsx('../data/NHANES_MGUS_Raw_Data_1999_2004.xlsx')

# specify age bounds 
age_lower <- c(50,
               55,
               60,
               65,
               70,
               75,
               80,
               85)
age_upper <- c(54,
               59,
               64,
               69,
               74,
               79,
               84,
               99)

# calculate the sample size and the number positive
df_male_nhw <- subset(df, riagendr == 'male' & ridreth2 == 1)
mgus_male_nhw <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhw$ridageyr >= age_lower[a] & df_male_nhw$ridageyr <= age_upper[a]), 
                                                           sum(df_male_nhw$mgus[df_male_nhw$ridageyr >= age_lower[a] & df_male_nhw$ridageyr <= age_upper[a]]))})

df_female_nhw <- subset(df, riagendr == 'female' & ridreth2 == 1)
mgus_female_nhw <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhw$ridageyr >= age_lower[a] & df_female_nhw$ridageyr <= age_upper[a]), 
                                                           sum(df_female_nhw$mgus[df_female_nhw$ridageyr >= age_lower[a] & df_female_nhw$ridageyr <= age_upper[a]]))})

df_male_nhb <- subset(df, riagendr == 'male' & ridreth2 == 2)
mgus_male_nhb <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhb$ridageyr >= age_lower[a] & df_male_nhb$ridageyr <= age_upper[a]), 
                                                           sum(df_male_nhb$mgus[df_male_nhb$ridageyr >= age_lower[a] & df_male_nhb$ridageyr <= age_upper[a]]))})

df_female_nhb <- subset(df, riagendr == 'female' & ridreth2 == 2)
mgus_female_nhb <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhb$ridageyr >= age_lower[a] & df_female_nhb$ridageyr <= age_upper[a]), 
                                                           sum(df_female_nhb$mgus[df_female_nhb$ridageyr >= age_lower[a] & df_female_nhb$ridageyr <= age_upper[a]]))})

# create the data frames 
df_final_male_nhw <- data.frame(age_lower = age_lower,
                                age_upper = age_upper,
                                sex = 'Male',
                                race = 'Non_Hispanic_White',
                                n = mgus_male_nhw[1,],
                                y = mgus_male_nhw[2,])

df_final_male_nhb <- data.frame(age_lower = age_lower,
                                age_upper = age_upper,
                                sex = 'Male',
                                race = 'Non_Hispanic_Black',
                                n = mgus_male_nhb[1,],
                                y = mgus_male_nhb[2,])

df_final_female_nhw <- data.frame(age_lower = age_lower,
                                  age_upper = age_upper,
                                  sex = 'Female',
                                  race = 'Non_Hispanic_White',
                                  n = mgus_female_nhw[1,],
                                  y = mgus_female_nhw[2,])

df_final_female_nhb <- data.frame(age_lower = age_lower,
                                  age_upper = age_upper,
                                  sex = 'Female',
                                  race = 'Non_Hispanic_Black',
                                  n = mgus_female_nhb[1,],
                                  y = mgus_female_nhb[2,])

# combine data 
df_final <- rbind(df_final_male_nhw,
                  df_final_male_nhb,
                  df_final_female_nhw,
                  df_final_female_nhb)

# write to file
write.csv(df_final, '../data/MGUS_Data_Reformatted.csv', row.names = F)
