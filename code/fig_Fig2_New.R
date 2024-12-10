# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(ggthemes)){install.packages('ggthemes'); library(ggthemes)}
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load necessary data
load('../output/trend_analysis/bmi_spline_0915/preds_3_6.RData')
load('../output/data_organized.RData')

# specify the lower bound of the ages 
age_lower <- unique(data_list$mm_trend_incid_male_nhw$age_lower)

# specify palette
palette <- tableau_color_pal('Classic Cyclic')(length(age_lower))

min_year <- min(data_list_combined$mm_trend_incid_male_nhw$year) - max(age_lower)
max_year <- max(data_list_combined$mm_trend_incid_male_nhw$year) - min(age_lower)

# generate plot 
jpeg(filename = '../output/figs_new/Fig_2.jpg', width = 10, height = 6.5, units = 'in', res = 500)
layout(mat = matrix(1:4,nrow = 2, byrow = T))
par(mar = c(3.3,3.3,1.1,1.1))
plot(NA, NA, xlim = c(min_year - 2.5, max_year + 2.5), ylim = c(0,80), axes = F,
     xaxs = 'i', yaxs = 'i', bty = 'n', xlab = '', ylab = '')
for(ii in 1:length(age_lower))
{
  indices <- which(data_list_combined$mm_trend_incid_male_nhw$age_lower == age_lower[ii])
  birth_years <- data_list_combined$mm_trend_incid_male_nhw$year[indices] - data_list_combined$mm_trend_incid_male_nhw$age_lower[indices]
  incidence_correction = 1e5 / data_list_combined$mm_trend_incid_male_nhw$n[indices]
  polygon(x = c(birth_years, rev(birth_years)), y = c(preds_combined$x_male_nhw_ppi['2.5%',indices] * incidence_correction, rev(preds_combined$x_male_nhw_ppi['97.5%',indices] * incidence_correction)),
          col = col2alpha(palette[ii],alpha=0.375), border = NA)
  polygon(x = c(birth_years, rev(birth_years)), y = c(preds_combined$x_male_nhw_ppi['25%',indices] * incidence_correction, rev(preds_combined$x_male_nhw_ppi['75%',indices] * incidence_correction)),
          col = col2alpha(palette[ii],alpha=0.5), border = NA)
  lines(x = birth_years, y = preds_combined$x_male_nhw_ppi['50%',indices] * incidence_correction, lwd = 2, col = palette[ii])
  
  indices_train <- which(data_list$mm_trend_incid_male_nhw$age_lower == age_lower[ii])
  indices_test <- which(data_list_test$mm_trend_incid_male_nhw$age_lower == age_lower[ii])
  
  segments(x0 = data_list$mm_trend_incid_male_nhw$year[indices_train] - data_list$mm_trend_incid_male_nhw$age_lower[indices_train],
           y0 = data_list$mm_trend_incid_male_nhw$x_lower[indices_train] * 1e5,
           y1 = data_list$mm_trend_incid_male_nhw$x_upper[indices_train] * 1e5,
           lwd = 1, col = palette[ii])
  points(x = data_list$mm_trend_incid_male_nhw$year[indices_train] - data_list$mm_trend_incid_male_nhw$age_lower[indices_train],
         y = data_list$mm_trend_incid_male_nhw$x[indices_train] * 1e5,
         pch = 21, cex = 1, col = palette[ii], bg = 'white')
  
  #segments(x0 = data_list_test$mm_trend_incid_male_nhw$year[indices_test] - data_list_test$mm_trend_incid_male_nhw$age_lower[indices_test],
  #         y0 = data_list_test$mm_trend_incid_male_nhw$x_lower[indices_test] * 1e5,
  #         y1 = data_list_test$mm_trend_incid_male_nhw$x_upper[indices_test] * 1e5,
  #         lwd = 1, col = palette[ii])
  #points(x = data_list_test$mm_trend_incid_male_nhw$year[indices_test] - data_list_test$mm_trend_incid_male_nhw$age_lower[indices_test],
  #       y = data_list_test$mm_trend_incid_male_nhw$x[indices_test] * 1e5,
  #       pch = 21, cex = 1, bg = 'white', col = palette[ii])
}
box()
axis(side = 1)
axis(side = 2, las = 1)
mtext(side = 2, line = 2.3, 'MM Incidence (per 100,000)')
mtext(side = 3, line = 0, at = min_year-2.5, 'A', font = 2)
mtext(side = 3, line = 0, 'White Men')
legend(x = max_year - 10, y = 80, pch = 15, col = palette, legend = c('40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85+'),pt.cex = 1, cex = 0.7, bty = 'n', title = 'Age Group')
legend(x = max_year - 30, y = 80, pch = c(NA,15,15,21), lwd = c(2,NA,NA,NA), pt.cex = 1,
       col = c('#222222', col2alpha('#222222',0.5), col2alpha('#222222',0.375),'#222222'),
       pt.bg = c(NA,NA,NA,'white'), legend = c('Median', '50% PPI', '95% PPI', 'SEER Data'), bty = 'n', cex = 0.7)


plot(NA, NA, xlim = c(min_year - 2.5, max_year + 2.5), ylim = c(0,50), axes = F,
     xaxs = 'i', yaxs = 'i', bty = 'n', xlab = '', ylab = '')
for(ii in 1:length(age_lower))
{
  indices <- which(data_list_combined$mm_trend_incid_female_nhw$age_lower == age_lower[ii])
  birth_years <- data_list_combined$mm_trend_incid_female_nhw$year[indices] - data_list_combined$mm_trend_incid_female_nhw$age_lower[indices]
  incidence_correction = 1e5 / data_list_combined$mm_trend_incid_female_nhw$n[indices]
  polygon(x = c(birth_years, rev(birth_years)), y = c(preds_combined$x_female_nhw_ppi['2.5%',indices] * incidence_correction, rev(preds_combined$x_female_nhw_ppi['97.5%',indices] * incidence_correction)),
          col = col2alpha(palette[ii],alpha=0.375), border = NA)
  polygon(x = c(birth_years, rev(birth_years)), y = c(preds_combined$x_female_nhw_ppi['25%',indices] * incidence_correction, rev(preds_combined$x_female_nhw_ppi['75%',indices] * incidence_correction)),
          col = col2alpha(palette[ii],alpha=0.5), border = NA)
  lines(x = birth_years, y = preds_combined$x_female_nhw_ppi['50%',indices] * incidence_correction, lwd = 2, col = palette[ii])
  
  indices_train <- which(data_list$mm_trend_incid_female_nhw$age_lower == age_lower[ii])
  indices_test <- which(data_list_test$mm_trend_incid_female_nhw$age_lower == age_lower[ii])
  
  segments(x0 = data_list$mm_trend_incid_female_nhw$year[indices_train] - data_list$mm_trend_incid_female_nhw$age_lower[indices_train],
           y0 = data_list$mm_trend_incid_female_nhw$x_lower[indices_train] * 1e5,
           y1 = data_list$mm_trend_incid_female_nhw$x_upper[indices_train] * 1e5,
           lwd = 1, col = palette[ii])
  points(x = data_list$mm_trend_incid_female_nhw$year[indices_train] - data_list$mm_trend_incid_female_nhw$age_lower[indices_train],
         y = data_list$mm_trend_incid_female_nhw$x[indices_train] * 1e5,
         pch = 21, cex = 1, col = palette[ii], bg = 'white')
  
  #segments(x0 = data_list_test$mm_trend_incid_female_nhw$year[indices_test] - data_list_test$mm_trend_incid_female_nhw$age_lower[indices_test],
  #         y0 = data_list_test$mm_trend_incid_female_nhw$x_lower[indices_test] * 1e5,
  #         y1 = data_list_test$mm_trend_incid_female_nhw$x_upper[indices_test] * 1e5,
  #         lwd = 1, col = palette[ii])
  #points(x = data_list_test$mm_trend_incid_female_nhw$year[indices_test] - data_list_test$mm_trend_incid_female_nhw$age_lower[indices_test],
  #       y = data_list_test$mm_trend_incid_female_nhw$x[indices_test] * 1e5,
  #       pch = 21, cex = 1, bg = 'white', col = palette[ii])
}
box()
axis(side = 1)
axis(side = 2, las = 1)
mtext(side = 3, line = 0, at = min_year-2.5, 'B', font = 2)
mtext(side = 3, line = 0, 'White Women')


plot(NA, NA, xlim = c(min_year - 2.5, max_year + 2.5), ylim = c(0,200), axes = F,
     xaxs = 'i', yaxs = 'i', bty = 'n', xlab = '', ylab = '')
for(ii in 1:length(age_lower))
{
  indices <- which(data_list_combined$mm_trend_incid_male_nhb$age_lower == age_lower[ii])
  birth_years <- data_list_combined$mm_trend_incid_male_nhb$year[indices] - data_list_combined$mm_trend_incid_male_nhb$age_lower[indices]
  incidence_correction = 1e5 / data_list_combined$mm_trend_incid_male_nhb$n[indices]
  polygon(x = c(birth_years, rev(birth_years)), y = c(preds_combined$x_male_nhb_ppi['2.5%',indices] * incidence_correction, rev(preds_combined$x_male_nhb_ppi['97.5%',indices] * incidence_correction)),
          col = col2alpha(palette[ii],alpha=0.375), border = NA)
  polygon(x = c(birth_years, rev(birth_years)), y = c(preds_combined$x_male_nhb_ppi['25%',indices] * incidence_correction, rev(preds_combined$x_male_nhb_ppi['75%',indices] * incidence_correction)),
          col = col2alpha(palette[ii],alpha=0.5), border = NA)
  lines(x = birth_years, y = preds_combined$x_male_nhb_ppi['50%',indices] * incidence_correction, lwd = 2, col = palette[ii])
  
  indices_train <- which(data_list$mm_trend_incid_male_nhb$age_lower == age_lower[ii])
  indices_test <- which(data_list_test$mm_trend_incid_male_nhb$age_lower == age_lower[ii])
  
  segments(x0 = data_list$mm_trend_incid_male_nhb$year[indices_train] - data_list$mm_trend_incid_male_nhb$age_lower[indices_train],
           y0 = data_list$mm_trend_incid_male_nhb$x_lower[indices_train] * 1e5,
           y1 = data_list$mm_trend_incid_male_nhb$x_upper[indices_train] * 1e5,
           lwd = 1, col = palette[ii])
  points(x = data_list$mm_trend_incid_male_nhb$year[indices_train] - data_list$mm_trend_incid_male_nhb$age_lower[indices_train],
         y = data_list$mm_trend_incid_male_nhb$x[indices_train] * 1e5,
         pch = 21, cex = 1, col = palette[ii], bg = 'white')
  
  #segments(x0 = data_list_test$mm_trend_incid_male_nhb$year[indices_test] - data_list_test$mm_trend_incid_male_nhb$age_lower[indices_test],
  #         y0 = data_list_test$mm_trend_incid_male_nhb$x_lower[indices_test] * 1e5,
  #         y1 = data_list_test$mm_trend_incid_male_nhb$x_upper[indices_test] * 1e5,
  #         lwd = 1, col = palette[ii])
  #points(x = data_list_test$mm_trend_incid_male_nhb$year[indices_test] - data_list_test$mm_trend_incid_male_nhb$age_lower[indices_test],
  #       y = data_list_test$mm_trend_incid_male_nhb$x[indices_test] * 1e5,
  #       pch = 21, cex = 1, bg = 'white', col = palette[ii])
}
box()
axis(side = 1)
axis(side = 2, las = 1)
mtext(side = 1, line = 2.3, 'Birth Year')
mtext(side = 2, line = 2.3, 'MM Incidence (per 100,000)')
mtext(side = 3, line = 0, at = min_year-2.5, 'C', font = 2)
mtext(side = 3, line = 0, 'Black Men')


plot(NA, NA, xlim = c(min_year - 2.5, max_year + 2.5), ylim = c(0,150), axes = F,
     xaxs = 'i', yaxs = 'i', bty = 'n', xlab = '', ylab = '')
for(ii in 1:length(age_lower))
{
  indices <- which(data_list_combined$mm_trend_incid_female_nhb$age_lower == age_lower[ii])
  birth_years <- data_list_combined$mm_trend_incid_female_nhb$year[indices] - data_list_combined$mm_trend_incid_female_nhb$age_lower[indices]
  incidence_correction = 1e5 / data_list_combined$mm_trend_incid_female_nhb$n[indices]
  polygon(x = c(birth_years, rev(birth_years)), y = c(preds_combined$x_female_nhb_ppi['2.5%',indices] * incidence_correction, rev(preds_combined$x_female_nhb_ppi['97.5%',indices] * incidence_correction)),
          col = col2alpha(palette[ii],alpha=0.375), border = NA)
  polygon(x = c(birth_years, rev(birth_years)), y = c(preds_combined$x_female_nhb_ppi['25%',indices] * incidence_correction, rev(preds_combined$x_female_nhb_ppi['75%',indices] * incidence_correction)),
          col = col2alpha(palette[ii],alpha=0.5), border = NA)
  lines(x = birth_years, y = preds_combined$x_female_nhb_ppi['50%',indices] * incidence_correction, lwd = 2, col = palette[ii])
  
  indices_train <- which(data_list$mm_trend_incid_female_nhb$age_lower == age_lower[ii])
  indices_test <- which(data_list_test$mm_trend_incid_female_nhb$age_lower == age_lower[ii])
  
  segments(x0 = data_list$mm_trend_incid_female_nhb$year[indices_train] - data_list$mm_trend_incid_female_nhb$age_lower[indices_train],
           y0 = data_list$mm_trend_incid_female_nhb$x_lower[indices_train] * 1e5,
           y1 = data_list$mm_trend_incid_female_nhb$x_upper[indices_train] * 1e5,
           lwd = 1, col = palette[ii])
  points(x = data_list$mm_trend_incid_female_nhb$year[indices_train] - data_list$mm_trend_incid_female_nhb$age_lower[indices_train],
         y = data_list$mm_trend_incid_female_nhb$x[indices_train] * 1e5,
         pch = 21, cex = 1, col = palette[ii], bg = 'white')
  
  #segments(x0 = data_list_test$mm_trend_incid_female_nhb$year[indices_test] - data_list_test$mm_trend_incid_female_nhb$age_lower[indices_test],
  #         y0 = data_list_test$mm_trend_incid_female_nhb$x_lower[indices_test] * 1e5,
  #         y1 = data_list_test$mm_trend_incid_female_nhb$x_upper[indices_test] * 1e5,
  #         lwd = 1, col = palette[ii])
  #points(x = data_list_test$mm_trend_incid_female_nhb$year[indices_test] - data_list_test$mm_trend_incid_female_nhb$age_lower[indices_test],
  #       y = data_list_test$mm_trend_incid_female_nhb$x[indices_test] * 1e5,
  #       pch = 21, cex = 1, bg = 'white', col = palette[ii])
}
box()
axis(side = 1)
axis(side = 2, las = 1)
mtext(side = 1, line = 2.3, 'Birth Year')
mtext(side = 3, line = 0, at = min_year-2.5, 'D', font = 2)
mtext(side = 3, line = 0, 'Black Women')
dev.off()