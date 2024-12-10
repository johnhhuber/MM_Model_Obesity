# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(foreign)){install.packages('foreign'); library(foreign)}
if(!require(openxlsx)){install.packages('openxlsx'); library(openxlsx)}

# load bmi file 
nhanes_bmi_1999_2000 <- read.xport('../data/NHANES_BMI_1999_2000.XPT')
nhanes_bmi_2001_2002 <- read.xport('../data/NHANES_BMI_2001_2002.XPT')
nhanes_bmi_2003_2004 <- read.xport('../data/NHANES_BMI_2003_2004.XPT')

nhanes_bmi <- data.frame(SEQN = c(nhanes_bmi_1999_2000$SEQN,
                                  nhanes_bmi_2001_2002$SEQN,
                                  nhanes_bmi_2003_2004$SEQN),
                         BMXBMI = c(nhanes_bmi_1999_2000$BMXBMI,
                                    nhanes_bmi_2001_2002$BMXBMI,
                                    nhanes_bmi_2003_2004$BMXBMI))

# categorize according to BMI bin
# 0 = underweight (BMI < 18.5)
# 1 = normal weight (BMI >= 18.5 & BMI < 25)
# 2 = overweight (BMI >= 25 and BMI < 30)
# 3 = obese (BMI >= 30)
nhanes_bmi$BMI_CATEGORY <- NA
nhanes_bmi$BMI_CATEGORY[nhanes_bmi$BMXBMI < 18.5] <- 0
nhanes_bmi$BMI_CATEGORY[nhanes_bmi$BMXBMI >= 18.5 & nhanes_bmi$BMXBM < 25] <- 1
nhanes_bmi$BMI_CATEGORY[nhanes_bmi$BMXBMI >= 25 & nhanes_bmi$BMXBM < 30] <- 2
nhanes_bmi$BMI_CATEGORY[nhanes_bmi$BMXBMI >= 30] <- 3

# load the data 
df <- read.xlsx('../data/NHANES_MGUS_Raw_Data_1999_2004.xlsx')

# pull the bmi category into the df 
df$bmi <- nhanes_bmi$BMI_CATEGORY[match(df$seqn, nhanes_bmi$SEQN)]

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
df_male_nhw_uw <- subset(df, riagendr == 'male' & ridreth2 == 1 & bmi == 0)
df_male_nhw_nw <- subset(df, riagendr == 'male' & ridreth2 == 1 & bmi == 1)
df_male_nhw_ow <- subset(df, riagendr == 'male' & ridreth2 == 1 & bmi == 2)
df_male_nhw_ob <- subset(df, riagendr == 'male' & ridreth2 == 1 & bmi == 3)

mgus_male_nhw_uw <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhw_uw$ridageyr >= age_lower[a] & df_male_nhw_uw$ridageyr <= age_upper[a]), 
                                                           sum(df_male_nhw_uw$mgus[df_male_nhw_uw$ridageyr >= age_lower[a] & df_male_nhw_uw$ridageyr <= age_upper[a]]))})
mgus_male_nhw_nw <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhw_nw$ridageyr >= age_lower[a] & df_male_nhw_nw$ridageyr <= age_upper[a]), 
                                                              sum(df_male_nhw_nw$mgus[df_male_nhw_nw$ridageyr >= age_lower[a] & df_male_nhw_nw$ridageyr <= age_upper[a]]))})
mgus_male_nhw_ob <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhw_ob$ridageyr >= age_lower[a] & df_male_nhw_ob$ridageyr <= age_upper[a]), 
                                                              sum(df_male_nhw_ob$mgus[df_male_nhw_ob$ridageyr >= age_lower[a] & df_male_nhw_ob$ridageyr <= age_upper[a]]))})
mgus_male_nhw_ow <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhw_ow$ridageyr >= age_lower[a] & df_male_nhw_ow$ridageyr <= age_upper[a]), 
                                                              sum(df_male_nhw_ow$mgus[df_male_nhw_ow$ridageyr >= age_lower[a] & df_male_nhw_ow$ridageyr <= age_upper[a]]))})


df_female_nhw_uw <- subset(df, riagendr == 'female' & ridreth2 == 1 & bmi == 0)
df_female_nhw_nw <- subset(df, riagendr == 'female' & ridreth2 == 1 & bmi == 1)
df_female_nhw_ow <- subset(df, riagendr == 'female' & ridreth2 == 1 & bmi == 2)
df_female_nhw_ob <- subset(df, riagendr == 'female' & ridreth2 == 1 & bmi == 3)

mgus_female_nhw_uw <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhw_uw$ridageyr >= age_lower[a] & df_female_nhw_uw$ridageyr <= age_upper[a]), 
                                                              sum(df_female_nhw_uw$mgus[df_female_nhw_uw$ridageyr >= age_lower[a] & df_female_nhw_uw$ridageyr <= age_upper[a]]))})
mgus_female_nhw_nw <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhw_nw$ridageyr >= age_lower[a] & df_female_nhw_nw$ridageyr <= age_upper[a]), 
                                                              sum(df_female_nhw_nw$mgus[df_female_nhw_nw$ridageyr >= age_lower[a] & df_female_nhw_nw$ridageyr <= age_upper[a]]))})
mgus_female_nhw_ob <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhw_ob$ridageyr >= age_lower[a] & df_female_nhw_ob$ridageyr <= age_upper[a]), 
                                                              sum(df_female_nhw_ob$mgus[df_female_nhw_ob$ridageyr >= age_lower[a] & df_female_nhw_ob$ridageyr <= age_upper[a]]))})
mgus_female_nhw_ow <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhw_ow$ridageyr >= age_lower[a] & df_female_nhw_ow$ridageyr <= age_upper[a]), 
                                                              sum(df_female_nhw_ow$mgus[df_female_nhw_ow$ridageyr >= age_lower[a] & df_female_nhw_ow$ridageyr <= age_upper[a]]))})


df_male_nhb_uw <- subset(df, riagendr == 'male' & ridreth2 == 2 & bmi == 0)
df_male_nhb_nw <- subset(df, riagendr == 'male' & ridreth2 == 2 & bmi == 1)
df_male_nhb_ow <- subset(df, riagendr == 'male' & ridreth2 == 2 & bmi == 2)
df_male_nhb_ob <- subset(df, riagendr == 'male' & ridreth2 == 2 & bmi == 3)

mgus_male_nhb_uw <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhb_uw$ridageyr >= age_lower[a] & df_male_nhb_uw$ridageyr <= age_upper[a]), 
                                                              sum(df_male_nhb_uw$mgus[df_male_nhb_uw$ridageyr >= age_lower[a] & df_male_nhb_uw$ridageyr <= age_upper[a]]))})
mgus_male_nhb_nw <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhb_nw$ridageyr >= age_lower[a] & df_male_nhb_nw$ridageyr <= age_upper[a]), 
                                                              sum(df_male_nhb_nw$mgus[df_male_nhb_nw$ridageyr >= age_lower[a] & df_male_nhb_nw$ridageyr <= age_upper[a]]))})
mgus_male_nhb_ob <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhb_ob$ridageyr >= age_lower[a] & df_male_nhb_ob$ridageyr <= age_upper[a]), 
                                                              sum(df_male_nhb_ob$mgus[df_male_nhb_ob$ridageyr >= age_lower[a] & df_male_nhb_ob$ridageyr <= age_upper[a]]))})
mgus_male_nhb_ow <- sapply(1:length(age_lower), function(a){c(sum(df_male_nhb_ow$ridageyr >= age_lower[a] & df_male_nhb_ow$ridageyr <= age_upper[a]), 
                                                              sum(df_male_nhb_ow$mgus[df_male_nhb_ow$ridageyr >= age_lower[a] & df_male_nhb_ow$ridageyr <= age_upper[a]]))})

df_female_nhb_uw <- subset(df, riagendr == 'female' & ridreth2 == 2 & bmi == 0)
df_female_nhb_nw <- subset(df, riagendr == 'female' & ridreth2 == 2 & bmi == 1)
df_female_nhb_ow <- subset(df, riagendr == 'female' & ridreth2 == 2 & bmi == 2)
df_female_nhb_ob <- subset(df, riagendr == 'female' & ridreth2 == 2 & bmi == 3)

mgus_female_nhb_uw <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhb_uw$ridageyr >= age_lower[a] & df_female_nhb_uw$ridageyr <= age_upper[a]), 
                                                              sum(df_female_nhb_uw$mgus[df_female_nhb_uw$ridageyr >= age_lower[a] & df_female_nhb_uw$ridageyr <= age_upper[a]]))})
mgus_female_nhb_nw <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhb_nw$ridageyr >= age_lower[a] & df_female_nhb_nw$ridageyr <= age_upper[a]), 
                                                              sum(df_female_nhb_nw$mgus[df_female_nhb_nw$ridageyr >= age_lower[a] & df_female_nhb_nw$ridageyr <= age_upper[a]]))})
mgus_female_nhb_ob <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhb_ob$ridageyr >= age_lower[a] & df_female_nhb_ob$ridageyr <= age_upper[a]), 
                                                              sum(df_female_nhb_ob$mgus[df_female_nhb_ob$ridageyr >= age_lower[a] & df_female_nhb_ob$ridageyr <= age_upper[a]]))})
mgus_female_nhb_ow <- sapply(1:length(age_lower), function(a){c(sum(df_female_nhb_ow$ridageyr >= age_lower[a] & df_female_nhb_ow$ridageyr <= age_upper[a]), 
                                                              sum(df_female_nhb_ow$mgus[df_female_nhb_ow$ridageyr >= age_lower[a] & df_female_nhb_ow$ridageyr <= age_upper[a]]))})

# create the data frames 
df_final_male_nhw_uw <- data.frame(age_lower = age_lower,
                                age_upper = age_upper,
                                sex = 'Male',
                                race = 'Non_Hispanic_White',
                                bmi = 'uw',
                                n = mgus_male_nhw_uw[1,],
                                y = mgus_male_nhw_uw[2,])
df_final_male_nhw_nw <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Male',
                                   race = 'Non_Hispanic_White',
                                   bmi = 'nw',
                                   n = mgus_male_nhw_nw[1,],
                                   y = mgus_male_nhw_nw[2,])
df_final_male_nhw_ow <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Male',
                                   race = 'Non_Hispanic_White',
                                   bmi = 'ow',
                                   n = mgus_male_nhw_ow[1,],
                                   y = mgus_male_nhw_ow[2,])
df_final_male_nhw_ob <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Male',
                                   race = 'Non_Hispanic_White',
                                   bmi = 'ob',
                                   n = mgus_male_nhw_ob[1,],
                                   y = mgus_male_nhw_ob[2,])


df_final_female_nhw_uw <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Female',
                                   race = 'Non_Hispanic_White',
                                   bmi = 'uw',
                                   n = mgus_female_nhw_uw[1,],
                                   y = mgus_female_nhw_uw[2,])
df_final_female_nhw_nw <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Female',
                                   race = 'Non_Hispanic_White',
                                   bmi = 'nw',
                                   n = mgus_female_nhw_nw[1,],
                                   y = mgus_female_nhw_nw[2,])
df_final_female_nhw_ow <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Female',
                                   race = 'Non_Hispanic_White',
                                   bmi = 'ow',
                                   n = mgus_female_nhw_ow[1,],
                                   y = mgus_female_nhw_ow[2,])
df_final_female_nhw_ob <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Female',
                                   race = 'Non_Hispanic_White',
                                   bmi = 'ob',
                                   n = mgus_female_nhw_ob[1,],
                                   y = mgus_female_nhw_ob[2,])


df_final_male_nhb_uw <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Male',
                                   race = 'Non_Hispanic_Black',
                                   bmi = 'uw',
                                   n = mgus_male_nhb_uw[1,],
                                   y = mgus_male_nhb_uw[2,])
df_final_male_nhb_nw <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Male',
                                   race = 'Non_Hispanic_Black',
                                   bmi = 'nw',
                                   n = mgus_male_nhb_nw[1,],
                                   y = mgus_male_nhb_nw[2,])
df_final_male_nhb_ow <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Male',
                                   race = 'Non_Hispanic_Black',
                                   bmi = 'ow',
                                   n = mgus_male_nhb_ow[1,],
                                   y = mgus_male_nhb_ow[2,])
df_final_male_nhb_ob <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Male',
                                   race = 'Non_Hispanic_Black',
                                   bmi = 'ob',
                                   n = mgus_male_nhb_ob[1,],
                                   y = mgus_male_nhb_ob[2,])


df_final_female_nhb_uw <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Female',
                                   race = 'Non_Hispanic_Black',
                                   bmi = 'uw',
                                   n = mgus_female_nhb_uw[1,],
                                   y = mgus_female_nhb_uw[2,])
df_final_female_nhb_nw <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Female',
                                   race = 'Non_Hispanic_Black',
                                   bmi = 'nw',
                                   n = mgus_female_nhb_nw[1,],
                                   y = mgus_female_nhb_nw[2,])
df_final_female_nhb_ow <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Female',
                                   race = 'Non_Hispanic_Black',
                                   bmi = 'ow',
                                   n = mgus_female_nhb_ow[1,],
                                   y = mgus_female_nhb_ow[2,])
df_final_female_nhb_ob <- data.frame(age_lower = age_lower,
                                   age_upper = age_upper,
                                   sex = 'Female',
                                   race = 'Non_Hispanic_Black',
                                   bmi = 'ob',
                                   n = mgus_female_nhb_ob[1,],
                                   y = mgus_female_nhb_ob[2,])


df_final <- rbind(df_final_male_nhw_uw,
                  df_final_male_nhw_nw,
                  df_final_male_nhw_ow,
                  df_final_male_nhw_ob,
                  df_final_female_nhw_uw,
                  df_final_female_nhw_nw,
                  df_final_female_nhw_ow,
                  df_final_female_nhw_ob,
                  df_final_male_nhb_uw,
                  df_final_male_nhb_nw,
                  df_final_male_nhb_ow,
                  df_final_male_nhb_ob,
                  df_final_female_nhb_uw,
                  df_final_female_nhb_nw,
                  df_final_female_nhb_ow,
                  df_final_female_nhb_ob)

# write to file
write.csv(df_final, '../data/MGUS_Data_Reformatted_BMI.csv', row.names = F)
