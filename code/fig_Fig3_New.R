# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(fields)){install.packages('fields'); library(fields)}
if(!require(RColorBrewer)){install.packages('RColorBrewer'); library(RColorBrewer)}
if(!require(plotrix)){install.packages('plotrix'); library(plotrix)}
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# specify palette
palette = c('#158644', '#ba4e41', '#34a2cc', '#e5bc20')
palette_ramp <- colorRampPalette(c(palette[4], 'white',palette[1]))(25)

# load necessary data
load('../output/trend_analysis/bmi_spline_0915/preds_3_6.RData')
load('../output/data_organized.RData')

# specify knots for spline and reference years
data_reference_year = 2004
period_reference_year = 2000
cohort_reference_year = 1920
diagnosis_reference_year = 2013

knots_x_period <- seq(from = min(data_list$mm_trend_incid_male_nhw$year) - period_reference_year, 
                      to = max(data_list$mm_trend_incid_male_nhw$year) - period_reference_year,
                      length.out = 3)
knots_x_period[which.min(abs(knots_x_period))] <- 0
knots_x_period <- knots_x_period + period_reference_year

knots_x_cohort <- seq(from = min(data_list$mm_trend_incid_male_nhw$year) - max(data_list$mm_trend_incid_male_nhw$age_upper) - cohort_reference_year,
                      to = max(data_list$mm_trend_incid_male_nhw$year) - min(data_list$mm_trend_incid_male_nhw$age_lower) - cohort_reference_year,
                      length.out = 6)
knots_x_cohort[which.min(abs(knots_x_cohort))] <- 0
knots_x_cohort <- knots_x_cohort + cohort_reference_year

years_period <- min(knots_x_period):max(knots_x_period)
years_cohort <- min(knots_x_cohort):max(knots_x_cohort)

# specify years and ages 
years <- 1975:2018
ages <- 40:85

# generate matrix 
mat_haz_nhw_male <- matrix(NA, nrow = length(ages), ncol = length(years))
mat_haz_nhw_female <- matrix(NA, nrow = length(ages), ncol = length(years))
mat_haz_nhb_male <- matrix(NA, nrow = length(ages), ncol = length(years))
mat_haz_nhb_female <- matrix(NA, nrow = length(ages), ncol = length(years))

for(ii in 1:length(years))
{
  mat_haz_nhw_male[,ii] = haz_trend_male_nhw_list[[years[ii]]]$haz_mm_bmi[ages+1]
  mat_haz_nhw_female[,ii] = haz_trend_female_nhw_list[[years[ii]]]$haz_mm_bmi[ages+1]
  mat_haz_nhb_male[,ii] = haz_trend_male_nhb_list[[years[ii]]]$haz_mm_bmi[ages+1]
  mat_haz_nhb_female[,ii] = haz_trend_female_nhb_list[[years[ii]]]$haz_mm_bmi[ages+1]
}

signif(mat_haz_nhw_male[which(ages == 40), which(years == 1975)], digits = 3)
signif(mat_haz_nhw_male[which(ages == 80), which(years == 1975)], digits = 3)
signif(mat_haz_nhw_male[which(ages == 40), which(years == 2018)], digits = 3)
signif(mat_haz_nhw_male[which(ages == 80), which(years == 2018)], digits = 3)

signif(max(mat_haz_nhw_male), digits = 3)
signif(max(mat_haz_nhw_female), digits = 3)
signif(max(mat_haz_nhb_male), digits = 3)
signif(max(mat_haz_nhb_female), digits = 3)

# generate plot 
jpeg(filename = '../output/figs_new/Fig_3.jpg', width = 8, height = 4, units = 'in', res = 500)
layout(mat = matrix(c(1,1,2,4,1,1,3,5),nrow = 2, byrow = T))
par(mar = c(3.3,3.5,1.1,1.1))
plot(NA,NA,xlim = c(min(years_cohort)-2, 2020), ylim = c(0.4,2.1), xaxs = 'i', yaxs = 'i',
     axes = F, xlab = '', ylab = '')
abline(h = 1, lwd = 1, lty = 2)

#polygon(x = c(years_period, rev(years_period)),
#        y = c(preds_no_bmi$period_effect_CI['2.5%',], rev(preds_no_bmi$period_effect_CI['97.5%',])),
#        col = col2alpha(palette[3], alpha = 0.375/2), border = NA)
#lines(years_period, preds_no_bmi$period_effect_CI['50%',], col = col2alpha(palette[3],0.5), lwd = 2, lty = 3)

polygon(x = c(years_period, rev(years_period)),
        y = c(preds$period_effect_CI['2.5%',], rev(preds$period_effect_CI['97.5%',])),
        col = col2alpha(palette[3], alpha = 0.375), border = NA)
lines(years_period, preds$period_effect_CI['50%',], col = palette[3], lwd = 2)
points(period_reference_year, 1, pch = 21, bg = palette[3], col = 'white', cex = 2)

#polygon(x = c(years_cohort, rev(years_cohort)),
#        y = c(preds_no_bmi$cohort_effect_CI['2.5%',], rev(preds_no_bmi$cohort_effect_CI['97.5%',])),
#        col = col2alpha(palette[2], alpha = 0.375/2), border = NA)
#lines(years_cohort, preds_no_bmi$cohort_effect_CI['50%',], col = col2alpha(palette[2],0.5), lwd = 2, lty = 3)
polygon(x = c(years_cohort, rev(years_cohort)),
        y = c(preds$cohort_effect_CI['2.5%',], rev(preds$cohort_effect_CI['97.5%',])),
        col = col2alpha(palette[2], alpha = 0.375), border = NA)
lines(years_cohort, preds$cohort_effect_CI['50%',], col = palette[2], lwd = 2)
points(cohort_reference_year, 1, pch = 21, bg = palette[2], col = 'white', cex = 2)
box()
axis(side = 1)
axis(side = 2, las = 1)
mtext(side = 1, line = 2.3, 'Year')
mtext(side = 2, line = 2.3, 'Multiplier')
mtext(side = 3, line = 0, 'A', font = 2, at = min(years_cohort))
#legend('topleft', lwd = c(2,NA,NA,NA,NA,NA,NA), pch = c(NA,15,16,15,15,15,15), col = c('#222222', col2alpha('#222222', alpha = 0.375), '#222222', palette[3], col2alpha(palette[3]), palette[2], col2alpha(palette[2])),
#       legend = c('Median', '95% CI', 'Reference Year', 'Period Effect with BMI', 'Period Effect without BMI', 'Cohort Effect with BMI', 'Cohort Effect without BMI'), bty = 'n', pt.cex = 2)
legend('topleft', lwd = c(2,NA,NA,NA,NA), pch = c(NA,15,16,15,15), col = c('#222222', col2alpha('#222222', alpha = 0.375), '#222222', palette[3], palette[2]),
       legend = c('Median', '95% CI', 'Reference Year', 'Period Effect', 'Cohort Effect'), bty = 'n', pt.cex = 2)

par(mar = c(3.3, 2.9, 1.1, 1.0))
image(x = years, y = ages, t(mat_haz_nhw_male), zlim = c(1,1.75), axes = F, xlab = '', ylab = '')
abline(v = years - 0.5, col = 'black', lwd = 0.25)
abline(h = ages - 0.5, col = 'black', lwd = 0.25)
axis(side = 1, at = years, labels = NA, tck = -0.0125)
axis(side = 1, las = 1, at = seq(from = 1975, to = 2020, by = 10))
axis(side = 2, las = 1, at = ages, labels= NA, tck = -0.0125)
axis(side = 2, las = 1, at = seq(from = min(ages), to = max(ages), by = 5))
box()
mtext(side = 2, line = 2.3, 'Age (yr)')
mtext(side = 3, line = 0, at = min(years), 'B', font = 2)
mtext(side = 3, line = 0, cex = 1, 'White Men')

par(mar = c(3.3, 2.9, 1.1, 1.0))
image(x = years, y = ages, t(mat_haz_nhw_female), zlim = c(1,1.75), axes = F, xlab = '', ylab = '', add = F)
abline(v = years - 0.5, col = 'black', lwd = 0.25)
abline(h = ages - 0.5, col = 'black', lwd = 0.25)
axis(side = 1, at = years, labels = NA, tck = -0.0125)
axis(side = 1, las = 1, at = seq(from = 1975, to = 2020, by = 10))
axis(side = 2, las = 1, at = ages, labels= NA, tck = -0.0125)
axis(side = 2, las = 1, at = seq(from = min(ages), to = max(ages), by = 5))
box()
mtext(side = 2, line = 2.3, 'Age (yr)')
mtext(side = 1, line = 2.3, 'Year')
mtext(side = 3, line = 0, at = min(years), 'D', font = 2)
mtext(side = 3, line = 0, cex = 1, 'White Women')

par(mar = c(3.3, 1.5, 1.1, 2.4))
image(x = years, y = ages, t(mat_haz_nhb_male), zlim = c(1,1.75), axes = F, xlab = '', ylab = '', add = F)
color.legend(xl = max(years + 2), xr = max(years + 4), yb = min(ages), yt = max(ages), rect.col = hcl.colors(12, "YlOrRd", rev = TRUE), gradient = 'y',
             legend = c(1,rep('',10),1.75), align = 'rb', cex = 0.5)
mtext(side = 4, line = 1.3, 'BMI Multiplier', cex = 0.5)
abline(v = years - 0.5, col = 'black', lwd = 0.25)
abline(h = ages - 0.5, col = 'black', lwd = 0.25)
axis(side = 1, at = years, labels = NA, tck = -0.0125)
axis(side = 1, las = 1, at = seq(from = 1975, to = 2020, by = 10))
axis(side = 2, las = 1, at = ages, labels= NA, tck = -0.0125)
axis(side = 2, las = 1, at = seq(from = min(ages), to = max(ages), by = 5))
box()
mtext(side = 3, line = 0, at = min(years), 'C', font = 2)
mtext(side = 3, line = 0, cex = 1, 'Black Men')

par(mar = c(3.3, 1.5, 1.1, 2.4))
image(x = years, y = ages, t(mat_haz_nhb_female), zlim = c(1,1.75), axes = F, xlab = '', ylab = '', add = F)
color.legend(xl = max(years + 2), xr = max(years + 4), yb = min(ages), yt = max(ages), rect.col = hcl.colors(12, "YlOrRd", rev = TRUE), gradient = 'y',
             legend = c(1,rep('',10),1.75), align = 'rb', cex = 0.5)
mtext(side = 4, line = 1.3, 'BMI Multiplier', cex = 0.5)
abline(v = years - 0.5, col = 'black', lwd = 0.25)
abline(h = ages - 0.5, col = 'black', lwd = 0.25)
axis(side = 1, at = years, labels = NA, tck = -0.0125)
axis(side = 1, las = 1, at = seq(from = 1975, to = 2020, by = 10))
axis(side = 2, las = 1, at = ages, labels= NA, tck = -0.0125)
axis(side = 2, las = 1, at = seq(from = min(ages), to = max(ages), by = 5))
box()
mtext(side = 1, line = 2.3, 'Year')
mtext(side = 3, line = 0, at = min(years), 'E', font = 2)
mtext(side = 3, line = 0, cex = 1, 'Black Women')

dev.off()

