# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load necessary data
load('../output/fig_1.RData')

# specify palette
palette <- rcartocolor::carto_pal(n = 12, name = 'Prism')[c(2,3,7,6)]
#col.axis.labs <- '#7A7B7B'
col.axis.labs <- '#222222'
cex = 1.5
xlim_offset = 2

# generate plot
jpeg(filename  = '../output/figs_new/fig_1.jpg', width = 10, height = 5, units = 'in', res = 500)
layout(mat = matrix(1:8, nrow = 2, ncol = 4, byrow = T))
par(mar = c(4.7,3.5,1.3,0.3))

plot(NA, NA, xlim = c(min(data_list$mgus_prev_male_nhw$age_lower)-xlim_offset,max(data_list$mgus_prev_male_nhw$age_lower)+xlim_offset), ylim = c(0,0.35),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mgus_prev_male_nhw$age_lower, rev(data_list$mgus_prev_male_nhw$age_lower)), y = c(preds$y_male_nhw_ppi['2.5%',] / data_list$mgus_prev_male_nhw$n, rev(preds$y_male_nhw_ppi['97.5%',] / data_list$mgus_prev_male_nhw$n)),
        col = col2alpha(palette[1],alpha = 0.25), border = NA)
polygon(x = c(data_list$mgus_prev_male_nhw$age_lower, rev(data_list$mgus_prev_male_nhw$age_lower)), y = c(preds$y_male_nhw_ppi['25%',] / data_list$mgus_prev_male_nhw$n, rev(preds$y_male_nhw_ppi['75%',] / data_list$mgus_prev_male_nhw$n)),
        col = col2alpha(palette[1],alpha = 0.5), border = NA)
lines(x = data_list$mgus_prev_male_nhw$age_lower, y = preds$y_male_nhw_ppi['50%',] / data_list$mgus_prev_male_nhw$n, col = palette[1], lwd = 2)
segments(x0 = data_list$mgus_prev_male_nhw$age_lower, y0 = p_male_nhw_lower, y1 = p_male_nhw_upper, lwd = cex, col = '#222222', lty = 1)
points(data_list$mgus_prev_male_nhw$age_lower, data_list$mgus_prev_male_nhw$y / data_list$mgus_prev_male_nhw$n, pch = 21, cex = cex, bg = 'white', col = '#222222')
axis(side = 1, at = seq(from = 45, to = 85, by = 5), labels = c(NA,'50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'), las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, at = seq(from = 0, to = 0.35, by = 0.05), labels = seq(from = 0, to = 35, by = 5), col.axis = col.axis.labs)
mtext(side = 2, line = 2.3, 'MGUS Prevalence (%)', col = '#222222', cex = 1)
mtext(side = 3, line = 0, 'White Men', font = 1, col = palette[1], cex = 1)
mtext(side = 3, line = 0, at = min(data_list$mgus_prev_male_nhw$age_lower)-xlim_offset, 'A', font = 2)

legend('topleft',
       pch = c(21,NA,15,15,NA), lwd = c(NA,2,NA,NA,2),
       col = c('#222222', '#222222', col2alpha(palette[1],0.25),col2alpha(palette[1],0.5),palette[1]),
       pt.bg = c('white',NA,NA,NA,NA),
       pt.cex = 1.25, cex = 0.75, c('NHANES Mean', 'NHANES 95% CI', '95% PPI', '50% PPI', 'Median'), bty = 'n')

plot(NA, NA, xlim = c(min(data_list$mgus_prev_female_nhw$age_lower)-xlim_offset,max(data_list$mgus_prev_female_nhw$age_lower)+xlim_offset), ylim = c(0,0.35),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mgus_prev_female_nhw$age_lower, rev(data_list$mgus_prev_female_nhw$age_lower)), y = c(preds$y_female_nhw_ppi['2.5%',] / data_list$mgus_prev_female_nhw$n, rev(preds$y_female_nhw_ppi['97.5%',] / data_list$mgus_prev_female_nhw$n)),
        col = col2alpha(palette[2],alpha = 0.25), border = NA)
polygon(x = c(data_list$mgus_prev_female_nhw$age_lower, rev(data_list$mgus_prev_female_nhw$age_lower)), y = c(preds$y_female_nhw_ppi['25%',] / data_list$mgus_prev_female_nhw$n, rev(preds$y_female_nhw_ppi['75%',] / data_list$mgus_prev_female_nhw$n)),
        col = col2alpha(palette[2],alpha = 0.5), border = NA)
lines(x = data_list$mgus_prev_female_nhw$age_lower, y = preds$y_female_nhw_ppi['50%',] / data_list$mgus_prev_female_nhw$n, col = palette[2], lwd = 2)
segments(x0 = data_list$mgus_prev_female_nhw$age_lower, y0 = p_female_nhw_lower, y1 = p_female_nhw_upper, lwd = cex, col = '#222222', lty = 1)
points(data_list$mgus_prev_female_nhw$age_lower, data_list$mgus_prev_female_nhw$y / data_list$mgus_prev_female_nhw$n, pch = 21, cex = cex, bg = 'white', col = '#222222')
axis(side = 1, at = seq(from = 45, to = 85, by = 5), labels = c(NA,'50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'), las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, at = seq(from = 0, to = 0.35, by = 0.05), labels = seq(from = 0, to = 35, by = 5), col.axis = col.axis.labs)
mtext(side = 3, line = 0, 'White Women', font = 1, col = palette[2], cex = 1)
mtext(side = 3, line = 0, at = min(data_list$mgus_prev_female_nhw$age_lower)-xlim_offset, 'B', font = 2)

plot(NA, NA, xlim = c(min(data_list$mgus_prev_male_nhb$age_lower)-xlim_offset,max(data_list$mgus_prev_male_nhb$age_lower)+xlim_offset), ylim = c(0,0.35),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mgus_prev_male_nhb$age_lower, rev(data_list$mgus_prev_male_nhb$age_lower)), y = c(preds$y_male_nhb_ppi['2.5%',] / data_list$mgus_prev_male_nhb$n, rev(preds$y_male_nhb_ppi['97.5%',] / data_list$mgus_prev_male_nhb$n)),
        col = col2alpha(palette[3],alpha = 0.25), border = NA)
polygon(x = c(data_list$mgus_prev_male_nhb$age_lower, rev(data_list$mgus_prev_male_nhb$age_lower)), y = c(preds$y_male_nhb_ppi['25%',] / data_list$mgus_prev_male_nhb$n, rev(preds$y_male_nhb_ppi['75%',] / data_list$mgus_prev_male_nhb$n)),
        col = col2alpha(palette[3],alpha = 0.5), border = NA)
lines(x = data_list$mgus_prev_male_nhb$age_lower, y = preds$y_male_nhb_ppi['50%',] / data_list$mgus_prev_male_nhb$n, col = palette[3], lwd = 2)
segments(x0 = data_list$mgus_prev_male_nhb$age_lower, y0 = p_male_nhb_lower, y1 = p_male_nhb_upper, lwd = cex, col = '#222222', lty = 1)
points(data_list$mgus_prev_male_nhb$age_lower, data_list$mgus_prev_male_nhb$y / data_list$mgus_prev_male_nhb$n, pch = 21, cex = cex, bg = 'white', col = '#222222')
axis(side = 1, at = seq(from = 45, to = 85, by = 5), labels = c(NA,'50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'), las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, at = seq(from = 0, to = 0.35, by = 0.05), labels = seq(from = 0, to = 35, by = 5), col.axis = col.axis.labs)
mtext(side = 3, line = 0, 'Black Men', font = 1, col = palette[3], cex = 1)
mtext(side = 3, line = 0, at = min(data_list$mgus_prev_male_nhb$age_lower)-xlim_offset, 'C', font = 2)

plot(NA, NA, xlim = c(min(data_list$mgus_prev_female_nhb$age_lower)-xlim_offset,max(data_list$mgus_prev_female_nhb$age_lower)+xlim_offset), ylim = c(0,0.35),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mgus_prev_female_nhb$age_lower, rev(data_list$mgus_prev_female_nhb$age_lower)), y = c(preds$y_female_nhb_ppi['2.5%',] / data_list$mgus_prev_female_nhb$n, rev(preds$y_female_nhb_ppi['97.5%',] / data_list$mgus_prev_female_nhb$n)),
        col = col2alpha(palette[4],alpha = 0.25), border = NA)
polygon(x = c(data_list$mgus_prev_female_nhb$age_lower, rev(data_list$mgus_prev_female_nhb$age_lower)), y = c(preds$y_female_nhb_ppi['25%',] / data_list$mgus_prev_female_nhb$n, rev(preds$y_female_nhb_ppi['75%',] / data_list$mgus_prev_female_nhb$n)),
        col = col2alpha(palette[4],alpha = 0.5), border = NA)
lines(x = data_list$mgus_prev_female_nhb$age_lower, y = preds$y_female_nhb_ppi['50%',] / data_list$mgus_prev_female_nhb$n, col = palette[4], lwd = 2)
segments(x0 = data_list$mgus_prev_female_nhb$age_lower, y0 = p_female_nhb_lower, y1 = p_female_nhb_upper, lwd = cex, col = '#222222', lty = 1)
points(data_list$mgus_prev_female_nhb$age_lower, data_list$mgus_prev_female_nhb$y / data_list$mgus_prev_female_nhb$n, pch = 21, cex = cex, bg = 'white', col = '#222222')
axis(side = 1, at = seq(from = 45, to = 85, by = 5), labels = c(NA,'50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'), las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, at = seq(from = 0, to = 0.35, by = 0.05), labels = seq(from = 0, to = 35, by = 5), col.axis = col.axis.labs)
mtext(side = 3, line = 0, 'Black Women', font = 1, col = palette[4], cex = 1)
mtext(side = 3, line = 0, at = min(data_list$mgus_prev_female_nhb$age_lower)-xlim_offset, 'D', font = 2)

plot(NA, NA, xlim = c(min(data_list$mm_incid_male_nhw$age_lower)-xlim_offset, max(data_list$mm_incid_male_nhw$age_lower)+xlim_offset), ylim = c(0,200),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mm_incid_male_nhw$age_lower, rev(data_list$mm_incid_male_nhw$age_lower)), y = c(preds$x_male_nhw_ppi['2.5%',] * 1e5, rev(preds$x_male_nhw_ppi['97.5%',] * 1e5)),
        col = col2alpha(palette[1],alpha = 0.25), border = NA)
polygon(x = c(data_list$mm_incid_male_nhw$age_lower, rev(data_list$mm_incid_male_nhw$age_lower)), y = c(preds$x_male_nhw_ppi['25%',] * 1e5, rev(preds$x_male_nhw_ppi['75%',] * 1e5)),
        col = col2alpha(palette[1], alpha = 0.5), border = NA)
lines(x = data_list$mm_incid_male_nhw$age_lower, y = preds$x_male_nhw_ppi['50%',] * 1e5, col = palette[1], lwd = 2)
segments(x0 = data_list$mm_incid_male_nhw$age_lower, y0 = data_list$mm_incid_male_nhw$x_lower * 1e5, y1 = data_list$mm_incid_male_nhw$x_upper * 1e5, lwd = cex, col = '#222222')
points(data_list$mm_incid_male_nhw$age_lower, data_list$mm_incid_male_nhw$x * 1e5, pch = 21, cex = cex, bg = 'white', col = '#222222')
axis(side = 1, at = seq(from = 0, to = 85, by = 5), labels = c('0', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                                                               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'),
     las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 3.5, 'Age (yr)', col = '#222222', cex = 1)
mtext(side = 2, line = 2.3, 'MM Incidence (per 100,000)', col = '#222222', cex = 1)
mtext(side = 3, line = 0, at = min(data_list$mm_incid_male_nhw$age_lower)-xlim_offset, 'E', font = 2)

legend('topleft',
       pch = c(21,NA,15,15,NA), lwd = c(NA,2,NA,NA,2),
       col = c('#222222', '#222222', col2alpha(palette[1],0.25),col2alpha(palette[1],0.5),palette[1]),
       pt.bg = c('white',NA,NA,NA,NA),
       pt.cex = 1.25, cex = 0.75, c('SEER 2004 Mean', 'SEER 2004 95% CI', '50% PPI', '95% PPI', 'Median'), bty = 'n')

plot(NA, NA, xlim = c(min(data_list$mm_incid_female_nhw$age_lower)-xlim_offset, max(data_list$mm_incid_female_nhw$age_lower)+xlim_offset), ylim = c(0,200),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mm_incid_female_nhw$age_lower, rev(data_list$mm_incid_female_nhw$age_lower)), y = c(preds$x_female_nhw_ppi['2.5%',] * 1e5, rev(preds$x_female_nhw_ppi['97.5%',] * 1e5)),
        col = col2alpha(palette[2],alpha = 0.25), border = NA)
polygon(x = c(data_list$mm_incid_female_nhw$age_lower, rev(data_list$mm_incid_female_nhw$age_lower)), y = c(preds$x_female_nhw_ppi['25%',] * 1e5, rev(preds$x_female_nhw_ppi['75%',] * 1e5)),
        col = col2alpha(palette[2], alpha = 0.5), border = NA)
lines(x = data_list$mm_incid_female_nhw$age_lower, y = preds$x_female_nhw_ppi['50%',] * 1e5, col = palette[2], lwd = 2)
segments(x0 = data_list$mm_incid_female_nhw$age_lower, y0 = data_list$mm_incid_female_nhw$x_lower * 1e5, y1 = data_list$mm_incid_female_nhw$x_upper * 1e5, lwd = cex, col = '#222222')
points(data_list$mm_incid_female_nhw$age_lower, data_list$mm_incid_female_nhw$x * 1e5, pch = 21, cex = cex, bg = 'white', col = '#222222')
axis(side = 1, at = seq(from = 0, to = 85, by = 5), labels = c('0', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                                                               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'),
     las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 3.5, 'Age (yr)', col = '#222222', cex = 1)
mtext(side = 3, line = 0, at = min(data_list$mm_incid_female_nhw$age_lower)-xlim_offset, 'F', font = 2)

plot(NA, NA, xlim = c(min(data_list$mm_incid_male_nhb$age_lower)-xlim_offset, max(data_list$mm_incid_male_nhb$age_lower)+xlim_offset), ylim = c(0,200),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mm_incid_male_nhb$age_lower, rev(data_list$mm_incid_male_nhb$age_lower)), y = c(preds$x_male_nhb_ppi['2.5%',] * 1e5, rev(preds$x_male_nhb_ppi['97.5%',] * 1e5)),
        col = col2alpha(palette[3],alpha = 0.25), border = NA)
polygon(x = c(data_list$mm_incid_male_nhb$age_lower, rev(data_list$mm_incid_male_nhb$age_lower)), y = c(preds$x_male_nhb_ppi['25%',] * 1e5, rev(preds$x_male_nhb_ppi['75%',] * 1e5)),
        col = col2alpha(palette[3], alpha = 0.5), border = NA)
lines(x = data_list$mm_incid_male_nhb$age_lower, y = preds$x_male_nhb_ppi['50%',] * 1e5, col = palette[3], lwd = 2)
segments(x0 = data_list$mm_incid_male_nhb$age_lower, y0 = data_list$mm_incid_male_nhb$x_lower * 1e5, y1 = data_list$mm_incid_male_nhb$x_upper * 1e5, lwd = cex, col = '#222222')
points(data_list$mm_incid_male_nhb$age_lower, data_list$mm_incid_male_nhb$x * 1e5, pch = 21, cex = cex, bg = 'white', col = '#222222')
axis(side = 1, at = seq(from = 0, to = 85, by = 5), labels = c('0', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                                                               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'),
     las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 3.5, 'Age (yr)', col = '#222222', cex = 1)
mtext(side = 3, line = 0, at = min(data_list$mm_incid_male_nhb$age_lower)-xlim_offset, 'G', font = 2)

plot(NA, NA, xlim = c(min(data_list$mm_incid_female_nhb$age_lower)-xlim_offset, max(data_list$mm_incid_female_nhb$age_lower)+xlim_offset), ylim = c(0,200),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mm_incid_female_nhb$age_lower, rev(data_list$mm_incid_female_nhb$age_lower)), y = c(preds$x_female_nhb_ppi['2.5%',] * 1e5, rev(preds$x_female_nhb_ppi['97.5%',] * 1e5)),
        col = col2alpha(palette[4],alpha = 0.25), border = NA)
polygon(x = c(data_list$mm_incid_female_nhb$age_lower, rev(data_list$mm_incid_female_nhb$age_lower)), y = c(preds$x_female_nhb_ppi['25%',] * 1e5, rev(preds$x_female_nhb_ppi['75%',] * 1e5)),
        col = col2alpha(palette[4], alpha = 0.5), border = NA)
lines(x = data_list$mm_incid_female_nhb$age_lower, y = preds$x_female_nhb_ppi['50%',] * 1e5, col = palette[4], lwd = 2)
segments(x0 = data_list$mm_incid_female_nhb$age_lower, y0 = data_list$mm_incid_female_nhb$x_lower * 1e5, y1 = data_list$mm_incid_female_nhb$x_upper * 1e5, lwd = cex, col = '#222222')
points(data_list$mm_incid_female_nhb$age_lower, data_list$mm_incid_female_nhb$x * 1e5, pch = 21, cex = cex, bg = 'white', col = '#222222')
axis(side = 1, at = seq(from = 0, to = 85, by = 5), labels = c('0', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                                                               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'),
     las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 3.5, 'Age (yr)', col = '#222222', cex = 1)
mtext(side = 3, line = 0, at = min(data_list$mm_incid_female_nhb$age_lower)-xlim_offset, 'H', font = 2)
dev.off()
