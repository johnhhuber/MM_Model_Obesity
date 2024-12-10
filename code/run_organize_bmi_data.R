# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# specify path to data files 
path_wm = '../data/BMI/WhiteMale/'
path_wf = '../data/BMI/WhiteFemale/'
path_bm = '../data/BMI/BlackMale/'
path_bf = '../data/BMI/BlackFemale/'

# list the data files
file_wm <- list.files(path = path_wm, full.names = F)
file_wf <- list.files(path = path_wf, full.names = F)
file_bm <- list.files(path = path_bm, full.names = F)
file_bf <- list.files(path = path_bf, full.names = F)

# get the years from the files and specify age and bmi vectors
years <- as.numeric(substring(file_wm, first = 1, last = 4))
ages <- 2:85
bmi <- 9:73

# create 3-d array (years x ages x bmi) and fill with data from files 
bmi_arr_wm <- list()
bmi_arr_wf <- list()
bmi_arr_bm <- list()
bmi_arr_bf <- list()

for(ff in 1:length(file_wm))
{
  dat <- read.csv(paste(path_wm, file_wm[ff], sep = ''))
  bmi_arr_wm[[years[ff]]] <- as.matrix(dat)
}
for(ff in 1:length(file_wf))
{
  dat <- read.csv(paste(path_wf, file_wf[ff], sep = ''))
  bmi_arr_wf[[years[ff]]] <- as.matrix(dat)
}

for(ff in 1:length(file_bm))
{
  dat <- read.csv(paste(path_bm, file_bm[ff], sep = ''))
  bmi_arr_bm[[years[ff]]] <- as.matrix(dat)
}
for(ff in 1:length(file_bf))
{
  dat <- read.csv(paste(path_bf, file_bf[ff], sep = ''))
  bmi_arr_bf[[years[ff]]] <- as.matrix(dat)
}

# get the indices for underweight, normal weight, overweight, and obese
ind_uw <- which(bmi <= 18)
ind_nw <- which(bmi > 18 & bmi <= 25)
ind_ow <- which(bmi > 25 & bmi <= 30)
ind_ob <- which(bmi > 30)

# construct a data frame to organize the data 
bmi_wm <- expand.grid(Year = years, Age = 0:99, Gender = 'Male', Race = 'Non_Hispanic_White',
                      Prev_Normal = NA, Prev_Under = NA, Prev_Over = NA, Prev_Obese = NA)
bmi_wf <- expand.grid(Year = years, Age = 0:99, Gender = 'Female', Race = 'Non_Hispanic_White',
                      Prev_Normal = NA, Prev_Under = NA, Prev_Over = NA, Prev_Obese = NA)
bmi_bm <- expand.grid(Year = years, Age = 0:99, Gender = 'Male', Race = 'Non_Hispanic_Black',
                      Prev_Normal = NA, Prev_Under = NA, Prev_Over = NA, Prev_Obese = NA)
bmi_bf <- expand.grid(Year = years, Age = 0:99, Gender = 'Female', Race = 'Non_Hispanic_Black',
                      Prev_Normal = NA, Prev_Under = NA, Prev_Over = NA, Prev_Obese = NA)

# fill data frame
for(ii in 1:nrow(bmi_wm))
{
  # get age and year
  yr <- bmi_wm$Year[ii]
  age <- bmi_wm$Age[ii]
  age <- ifelse(age < 2, 2,age)
  age <- ifelse(age > 85,85,age)
  age_row <- paste('Age_', age, sep = '')
  
  bmi_wm$Prev_Under[ii] <- sum(bmi_arr_wm[[yr]][age_row,ind_uw])
  bmi_wm$Prev_Normal[ii] <- sum(bmi_arr_wm[[yr]][age_row,ind_nw])
  bmi_wm$Prev_Over[ii] <- sum(bmi_arr_wm[[yr]][age_row,ind_ow])
  bmi_wm$Prev_Obese[ii] <- sum(bmi_arr_wm[[yr]][age_row,ind_ob])
  
  bmi_wf$Prev_Under[ii] <- sum(bmi_arr_wf[[yr]][age_row,ind_uw])
  bmi_wf$Prev_Normal[ii] <- sum(bmi_arr_wf[[yr]][age_row,ind_nw])
  bmi_wf$Prev_Over[ii] <- sum(bmi_arr_wf[[yr]][age_row,ind_ow])
  bmi_wf$Prev_Obese[ii] <- sum(bmi_arr_wf[[yr]][age_row,ind_ob])
  
  bmi_bm$Prev_Under[ii] <- sum(bmi_arr_bm[[yr]][age_row,ind_uw])
  bmi_bm$Prev_Normal[ii] <- sum(bmi_arr_bm[[yr]][age_row,ind_nw])
  bmi_bm$Prev_Over[ii] <- sum(bmi_arr_bm[[yr]][age_row,ind_ow])
  bmi_bm$Prev_Obese[ii] <- sum(bmi_arr_bm[[yr]][age_row,ind_ob])
  
  bmi_bf$Prev_Under[ii] <- sum(bmi_arr_bf[[yr]][age_row,ind_uw])
  bmi_bf$Prev_Normal[ii] <- sum(bmi_arr_bf[[yr]][age_row,ind_nw])
  bmi_bf$Prev_Over[ii] <- sum(bmi_arr_bf[[yr]][age_row,ind_ow])
  bmi_bf$Prev_Obese[ii] <- sum(bmi_arr_bf[[yr]][age_row,ind_ob])
}

# row bind into a data frame
bmi_df <- rbind(bmi_wm,
                bmi_wf,
                bmi_bm,
                bmi_bf)

# write to file
write.csv(bmi_df, '../data/BMI/BMI_aggregated.csv',row.names = F)
