# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(openxlsx)){install.packages('openxlsx'); library(openxlsx)}

# function to normalize vector
normalize <- function(x){x / sum(x)}

# load the bridged race population projections from 2010
pop_br <- read.table('../data/Bridged-Race Population Estimates 1990-2020-7.txt', sep = '\t', header = T)

# load the population projections from 2014. this will be used to disaggregate the 85+ years age category from the bridged race projections
pop_pp <- read.table('../data/CDC_Wonder_Population_Projection_2014.txt', sep = '\t', header = T)

# load the SEER 9 data to get the years for which we have MM incidence data
seer_data <- read.xlsx('../data/SEER9_MM_Trend.xlsx')
yrs <- unique(seer_data$Year)

# subset the data into the race/gender pairings
pop_br_male_nhw <- subset(pop_br, Gender == 'Male' & Race == 'White')
pop_pp_male_nhw <- subset(pop_pp, Gender == 'Male' & Race == 'White')

pop_br_female_nhw <- subset(pop_br, Gender == 'Female' & Race == 'White')
pop_pp_female_nhw <- subset(pop_pp, Gender == 'Female' & Race == 'White')

pop_br_male_nhb <- subset(pop_br, Gender == 'Male' & Race == 'Black or African American')
pop_pp_male_nhb <- subset(pop_pp, Gender == 'Male' & Race == 'Black or African American')

pop_br_female_nhb <- subset(pop_br, Gender == 'Female' & Race == 'Black or African American')
pop_pp_female_nhb <- subset(pop_pp, Gender == 'Female' & Race == 'Black or African American')

# create the data frame 
ages <- 0:99

df_male_nhw <- data.frame(age = rep(ages,length(yrs)),
                          year = rep(yrs, each = length(ages)),
                          gender = 'Male',
                          race = 'Non_Hispanic_White',
                          N = NA)

df_female_nhw <- data.frame(age = rep(ages,length(yrs)),
                            year = rep(yrs, each = length(ages)),
                            gender = 'Female',
                            race = 'Non_Hispanic_White',
                            N = NA)

df_male_nhb <- data.frame(age = rep(ages,length(yrs)),
                          year = rep(yrs, each = length(ages)),
                          gender = 'Male',
                          race = 'Non_Hispanic_Black',
                          N = NA)

df_female_nhb <- data.frame(age = rep(ages,length(yrs)),
                            year = rep(yrs, each = length(ages)),
                            gender = 'Female',
                            race = 'Non_Hispanic_Black',
                            N = NA)

# calculate the age distribution for individuals 85-100 based upon the population projections from 2014
dist_male_nhw <- normalize(pop_pp_male_nhw$Projected.Populations[pop_pp_male_nhw$Age.Code %in% 85:100])
dist_female_nhw <- normalize(pop_pp_female_nhw$Projected.Populations[pop_pp_female_nhw$Age.Code %in% 85:100])
dist_male_nhb <- normalize(pop_pp_male_nhb$Projected.Populations[pop_pp_male_nhb$Age.Code %in% 85:100])
dist_female_nhb <- normalize(pop_pp_female_nhb$Projected.Populations[pop_pp_female_nhb$Age.Code %in% 85:100])

# loop through each year and fill in the data 
yrs_data <- unique(pop_br$Yearly.July.1st.Estimates)
for(yy in 1:length(yrs_data))
{
  # fill in the ages 0:84 from the bridged race projections
  df_male_nhw$N[(df_male_nhw$age %in% 0:84) & df_male_nhw$year == yrs_data[yy]] <- pop_br_male_nhw$Population[(pop_br_male_nhw$Age.Code %in% 0:84) 
                                                                                                         & pop_br_male_nhw$Yearly.July.1st.Estimates == yrs_data[yy]]
  df_female_nhw$N[(df_female_nhw$age %in% 0:84) & df_female_nhw$year == yrs_data[yy]] <- pop_br_female_nhw$Population[(pop_br_female_nhw$Age.Code %in% 0:84) 
                                                                                                              & pop_br_female_nhw$Yearly.July.1st.Estimates == yrs_data[yy]]
  
  df_male_nhb$N[(df_male_nhb$age %in% 0:84) & df_male_nhb$year == yrs_data[yy]] <- pop_br_male_nhb$Population[(pop_br_male_nhb$Age.Code %in% 0:84) 
                                                                                                              & pop_br_male_nhb$Yearly.July.1st.Estimates == yrs_data[yy]]
  df_female_nhb$N[(df_female_nhb$age %in% 0:84) & df_female_nhb$year == yrs_data[yy]] <- pop_br_female_nhb$Population[(pop_br_female_nhb$Age.Code %in% 0:84) 
                                                                                                                      & pop_br_female_nhb$Yearly.July.1st.Estimates == yrs_data[yy]]
  
  # fill in the ages 85:99 by disaggregating the bridged race projections using the distribution calculated from the population projections
  df_male_nhw$N[(df_male_nhw$age %in% 85:99) & df_male_nhw$year == yrs_data[yy]] <- round(pop_br_male_nhw$Population[pop_br_male_nhw$Age.Code == 85 & pop_br_male_nhw$Yearly.July.1st.Estimates == yrs_data[yy]] * head(dist_male_nhw, n = -1))
  df_female_nhw$N[(df_female_nhw$age %in% 85:99) & df_female_nhw$year == yrs_data[yy]] <- round(pop_br_female_nhw$Population[pop_br_female_nhw$Age.Code == 85 & pop_br_female_nhw$Yearly.July.1st.Estimates == yrs_data[yy]] * head(dist_female_nhw, n = -1))
  
  df_male_nhb$N[(df_male_nhb$age %in% 85:99) & df_male_nhb$year == yrs_data[yy]] <- round(pop_br_male_nhb$Population[pop_br_male_nhb$Age.Code == 85 & pop_br_male_nhb$Yearly.July.1st.Estimates == yrs_data[yy]] * head(dist_male_nhb, n = -1))
  df_female_nhb$N[(df_female_nhb$age %in% 85:99) & df_female_nhb$year == yrs_data[yy]] <- round(pop_br_female_nhb$Population[pop_br_female_nhb$Age.Code == 85 & pop_br_female_nhb$Yearly.July.1st.Estimates == yrs_data[yy]] * head(dist_female_nhb, n = -1))
}

# loop through the data frame: if the year is less than 1990, set to 1990 data 
for(yy in 1:length(yrs))
{
  if(yrs[yy] < 1990)
  {
    df_male_nhw$N[df_male_nhw$year == yrs[yy]] <- df_male_nhw$N[df_male_nhw$year == 1990]
    df_female_nhw$N[df_female_nhw$year == yrs[yy]] <- df_female_nhw$N[df_female_nhw$year == 1990]
    
    df_male_nhb$N[df_male_nhb$year == yrs[yy]] <- df_male_nhb$N[df_male_nhb$year == 1990]
    df_female_nhb$N[df_female_nhb$year == yrs[yy]] <- df_female_nhb$N[df_female_nhb$year == 1990]
  }
}

# combine the data frames
df <- rbind(df_male_nhw,
            df_female_nhw,
            df_male_nhb,
            df_female_nhb)

# write data frame to file
write.csv(df,
          file = '../data/Population_Data_Disaggregated.csv',
          row.names = F)
