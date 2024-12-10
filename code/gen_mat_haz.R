# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# load necessary data 
load('../output/data_organized_train_and_test.RData')

# specify years and ages 
years <- 1975:2018
ages <- 50:85

# generate matrix 
mat_haz_nhw_male <- matrix(NA, nrow = length(ages), ncol = length(years))
mat_haz_nhw_female <- matrix(NA, nrow = length(ages), ncol = length(years))
mat_haz_nhb_male <- matrix(NA, nrow = length(ages), ncol = length(years))
mat_haz_nhb_female <- matrix(NA, nrow = length(ages), ncol = length(years))

for(ii in 1:length(years))
{
  print(ii)
  mat_haz_nhw_male[,ii] = haz_trend_male_nhw_list[[years[ii]]]$haz_mm_bmi[ages+1]
  mat_haz_nhw_female[,ii] = haz_trend_female_nhw_list[[years[ii]]]$haz_mm_bmi[ages+1]
  mat_haz_nhb_male[,ii] = haz_trend_male_nhb_list[[years[ii]]]$haz_mm_bmi[ages+1]
  mat_haz_nhb_female[,ii] = haz_trend_female_nhb_list[[years[ii]]]$haz_mm_bmi[ages+1]
}

image(x = years, y = ages, t(mat_haz_nhw_male), las = 1, zlim = c(1,2))
abline(v = years - 0.5, col = 'white', lwd = 0.25)
abline(h = ages - 0.5, col = 'white', lwd = 0.25)

image(x = years, y = ages, t(mat_haz_nhw_female), las = 1, zlim = c(1,2))
abline(v = years - 0.5, col = 'white', lwd = 0.25)
abline(h = ages - 0.5, col = 'white', lwd = 0.25)

image(x = years, y = ages, t(mat_haz_nhb_male), las = 1, zlim = c(1,2))
abline(v = years - 0.5, col = 'white', lwd = 0.25)
abline(h = ages - 0.5, col = 'white', lwd = 0.25)

image(x = years, y = ages, t(mat_haz_nhb_male), las = 1, axes = F, xlab = '', ylab = '')
abline(v = years - 0.5, col = 'black', lwd = 0.5)
abline(h = ages - 0.5, col = 'black', lwd = 0.5)
axis(side = 1, at = years, labels = NA, tck = -0.0075)
axis(side = 1, las = 1, at = seq(from = 1975, to = 2020, by = 5))
axis(side = 2, las = 1, at = ages, labels= NA, tck = -0.0075)
axis(side = 2, las = 1, at = seq(from = 50, to = 85, by = 5))
box()
mtext(side = 1, line = 2.3, 'Year')
mtext(side = 2, line = 2.3, 'Age (yr)')