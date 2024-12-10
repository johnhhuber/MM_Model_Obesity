# set working directory 
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(RColorBrewer)){install.packages('RColorBrewer'); library(RColorBrewer)}
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load data 
load('../output/data_organized_bmi.RData')
load('../output/fig_S2.RData')

# calculate point estimates for total prevalence 
y_uw <- sum(data_list$mgus_prev_male_nhw_uw$y) + sum(data_list$mgus_prev_female_nhw_uw$y) + sum(data_list$mgus_prev_male_nhb_uw$y) + sum(data_list$mgus_prev_female_nhb_uw$y)
y_nw <- sum(data_list$mgus_prev_male_nhw_nw$y) + sum(data_list$mgus_prev_female_nhw_nw$y) + sum(data_list$mgus_prev_male_nhb_nw$y) + sum(data_list$mgus_prev_female_nhb_nw$y)
y_ow <- sum(data_list$mgus_prev_male_nhw_ow$y) + sum(data_list$mgus_prev_female_nhw_ow$y) + sum(data_list$mgus_prev_male_nhb_ow$y) + sum(data_list$mgus_prev_female_nhb_ow$y)
y_ob <- sum(data_list$mgus_prev_male_nhw_ob$y) + sum(data_list$mgus_prev_female_nhw_ob$y) + sum(data_list$mgus_prev_male_nhb_ob$y) + sum(data_list$mgus_prev_female_nhb_ob$y)

n_uw <- sum(data_list$mgus_prev_male_nhw_uw$n) + sum(data_list$mgus_prev_female_nhw_uw$n) + sum(data_list$mgus_prev_male_nhb_uw$n) + sum(data_list$mgus_prev_female_nhb_uw$n)
n_nw <- sum(data_list$mgus_prev_male_nhw_nw$n) + sum(data_list$mgus_prev_female_nhw_nw$n) + sum(data_list$mgus_prev_male_nhb_nw$n) + sum(data_list$mgus_prev_female_nhb_nw$n)
n_ow <- sum(data_list$mgus_prev_male_nhw_ow$n) + sum(data_list$mgus_prev_female_nhw_ow$n) + sum(data_list$mgus_prev_male_nhb_ow$n) + sum(data_list$mgus_prev_female_nhb_ow$n)
n_ob <- sum(data_list$mgus_prev_male_nhw_ob$n) + sum(data_list$mgus_prev_female_nhw_ob$n) + sum(data_list$mgus_prev_male_nhb_ob$n) + sum(data_list$mgus_prev_female_nhb_ob$n)

p_uw <- y_uw / n_uw
p_nw <- y_nw / n_nw
p_ow <- y_ow / n_ow
p_ob <- y_ob / n_ob

# plotting parameter 
rw <- 0.15
palette <- rev(brewer.pal(name = 'Spectral', n = 11)[c(2,3,4,5)])
col.axis.labs <- '#7A7B7B'
#offset <- c(-0.3, -0.1, 0.1, 0.3)
offset <- c(-0.3, 0, 0.3)
lwd = 1.5
col = '#222222'
pch = 21

# generate plot 
jpeg(filename = '../output/figs_new/fig_S2.jpg', width = 7, height = 5, units = 'in', res = 500)
par(mar = c(2.3,3.3,1.3,0.3))
plot(NA, NA, xlim = c(0.5,5.5), ylim = c(0,0.12), axes = F, xaxs = 'i', yaxs = 'i',
     xlab = '', ylab = '')
box()
abline(v = seq(from = 1.5, to = 4.5, by = 1))
rect(xleft = 1 + offset[1] - rw, xright = 1 + offset[1] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_male_nhw_nw$y) / sum(data_list$mgus_prev_male_nhw_nw$n),
     col = col2alpha(palette[2], alpha = 0.5), border = palette[2])
segments(x0 = 1 + offset[1], y0 = y_male_nhw_nw_ppi['2.5%'], y1 = y_male_nhw_nw_ppi['97.5%'], lwd = lwd, col = col)
points(x = 1 + offset[1], y = y_male_nhw_nw_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 1 + offset[2] - rw, xright = 1 + offset[2] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_male_nhw_ow$y) / sum(data_list$mgus_prev_male_nhw_ow$n),
     col = col2alpha(palette[3], alpha = 0.5), border = palette[3])
segments(x0 = 1 + offset[2], y0 = y_male_nhw_ow_ppi['2.5%'], y1 = y_male_nhw_ow_ppi['97.5%'], lwd = lwd, col = col)
points(x = 1 + offset[2], y = y_male_nhw_ow_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 1 + offset[3] - rw, xright = 1 + offset[3] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_male_nhw_ob$y) / sum(data_list$mgus_prev_male_nhw_ob$n),
     col = col2alpha(palette[4], alpha = 0.5), border = palette[4])
segments(x0 = 1 + offset[3], y0 = y_male_nhw_ob_ppi['2.5%'], y1 = y_male_nhw_ob_ppi['97.5%'], lwd = lwd, col = col)
points(x = 1 + offset[3], y = y_male_nhw_ob_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')


rect(xleft = 2 + offset[1] - rw, xright = 2 + offset[1] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_female_nhw_nw$y) / sum(data_list$mgus_prev_female_nhw_nw$n),
     col = col2alpha(palette[2], alpha = 0.5), border = palette[2])
segments(x0 = 2 + offset[1], y0 = y_female_nhw_nw_ppi['2.5%'], y1 = y_female_nhw_nw_ppi['97.5%'], lwd = lwd, col = col)
points(x = 2 + offset[1], y = y_female_nhw_nw_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 2 + offset[2] - rw, xright = 2 + offset[2] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_female_nhw_ow$y) / sum(data_list$mgus_prev_female_nhw_ow$n),
     col = col2alpha(palette[3], alpha = 0.5), border = palette[3])
segments(x0 = 2 + offset[2], y0 = y_female_nhw_ow_ppi['2.5%'], y1 = y_female_nhw_ow_ppi['97.5%'], lwd = lwd, col = col)
points(x = 2 + offset[2], y = y_female_nhw_ow_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 2 + offset[3] - rw, xright = 2 + offset[3] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_female_nhw_ob$y) / sum(data_list$mgus_prev_female_nhw_ob$n),
     col = col2alpha(palette[4], alpha = 0.5), border = palette[4])
segments(x0 = 2 + offset[3], y0 = y_female_nhw_ob_ppi['2.5%'], y1 = y_female_nhw_ob_ppi['97.5%'], lwd = lwd, col = col)
points(x = 2 + offset[3], y = y_female_nhw_ob_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')


rect(xleft = 3 + offset[1] - rw, xright = 3 + offset[1] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_male_nhb_nw$y) / sum(data_list$mgus_prev_male_nhb_nw$n),
     col = col2alpha(palette[2], alpha = 0.5), border = palette[2])
segments(x0 = 3 + offset[1], y0 = y_male_nhb_nw_ppi['2.5%'], y1 = y_male_nhb_nw_ppi['97.5%'], lwd = lwd, col = col)
points(x = 3 + offset[1], y = y_male_nhb_nw_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 3 + offset[2] - rw, xright = 3 + offset[2] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_male_nhb_ow$y) / sum(data_list$mgus_prev_male_nhb_ow$n),
     col = col2alpha(palette[3], alpha = 0.5), border = palette[3])
segments(x0 = 3 + offset[2], y0 = y_male_nhb_ow_ppi['2.5%'], y1 = y_male_nhb_ow_ppi['97.5%'], lwd = lwd, col = col)
points(x = 3 + offset[2], y = y_male_nhb_ow_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 3 + offset[3] - rw, xright = 3 + offset[3] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_male_nhb_ob$y) / sum(data_list$mgus_prev_male_nhb_ob$n),
     col = col2alpha(palette[4], alpha = 0.5), border = palette[4])
segments(x0 = 3 + offset[3], y0 = y_male_nhb_ob_ppi['2.5%'], y1 = y_male_nhb_ob_ppi['97.5%'], lwd = lwd, col = col)
points(x = 3 + offset[3], y = y_male_nhb_ob_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')


rect(xleft = 4 + offset[1] - rw, xright = 4 + offset[1] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_female_nhb_nw$y) / sum(data_list$mgus_prev_female_nhb_nw$n),
     col = col2alpha(palette[2], alpha = 0.5), border = palette[2])
segments(x0 = 4 + offset[1], y0 = y_female_nhb_nw_ppi['2.5%'], y1 = y_female_nhb_nw_ppi['97.5%'], lwd = lwd, col = col)
points(x = 4 + offset[1], y = y_female_nhb_nw_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 4 + offset[2] - rw, xright = 4 + offset[2] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_female_nhb_ow$y) / sum(data_list$mgus_prev_female_nhb_ow$n),
     col = col2alpha(palette[3], alpha = 0.5), border = palette[3])
segments(x0 = 4 + offset[2], y0 = y_female_nhb_ow_ppi['2.5%'], y1 = y_female_nhb_ow_ppi['97.5%'], lwd = lwd, col = col)
points(x = 4 + offset[2], y = y_female_nhb_ow_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 4 + offset[3] - rw, xright = 4 + offset[3] + rw, 
     ybottom = 0, ytop = sum(data_list$mgus_prev_female_nhb_ob$y) / sum(data_list$mgus_prev_female_nhb_ob$n),
     col = col2alpha(palette[4], alpha = 0.5), border = palette[4])
segments(x0 = 4 + offset[3], y0 = y_female_nhb_ob_ppi['2.5%'], y1 = y_female_nhb_ob_ppi['97.5%'], lwd = lwd, col = col)
points(x = 4 + offset[3], y = y_female_nhb_ob_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')

rect(xleft = 5 + offset[1] - rw, xright = 5 + offset[1] + rw, 
     ybottom = 0, ytop = p_nw,
     col = col2alpha(palette[2], alpha = 0.5), border = palette[2])
segments(x0 = 5 + offset[1], y0 = y_nw_ppi['2.5%'], y1 = y_nw_ppi['97.5%'], lwd = lwd, col = col)
points(x = 5 + offset[1], y = y_nw_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 5 + offset[2] - rw, xright = 5 + offset[2] + rw, 
     ybottom = 0, ytop = p_ow,
     col = col2alpha(palette[3], alpha = 0.5), border = palette[3])
segments(x0 = 5 + offset[2], y0 = y_ow_ppi['2.5%'], y1 = y_ow_ppi['97.5%'], lwd = lwd, col = col)
points(x = 5 + offset[2], y = y_ow_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')
rect(xleft = 5 + offset[3] - rw, xright = 5 + offset[3] + rw, 
     ybottom = 0, ytop = p_ob,
     col = col2alpha(palette[4], alpha = 0.5), border = palette[4])
segments(x0 = 5 + offset[3], y0 = y_ob_ppi['2.5%'], y1 = y_ob_ppi['97.5%'], lwd = lwd, col = col)
points(x = 5 + offset[3], y = y_ob_ppi['50%'], pch = pch, cex = lwd, bg = col, col = 'white')

axis(side = 1, at = 1:5, tick = F,
     labels = c('White Men', 'White Women', 'Black Men', 'Black Women', 'Overall'))
axis(side = 2, las = 1, at = seq(from = 0, to = 0.12, by = 0.03), labels = seq(from = 0, to = 12, by = 3))
mtext(side = 2, line = 2.3, 'NHANES MGUS Prevalence (%)')

legend(x = 3, y = 0.125, pch = c(0,16,NA,22,22,22), lwd = c(NA,NA,2,NA,NA,NA), pt.cex = 1.5, cex = 0.75,
       col = c('#222222', '#222222', '#222222',palette[2:4]), pt.bg = c(NA,NA,NA,col2alpha(palette[2]), col2alpha(palette[3]), col2alpha(palette[4])),
       legend = c('NHANES 1999-2004', 'Median', '95% CI','BMI: 18.5-24.9', 'BMI: 25-29.9',paste0('BMI: ',intToUtf8(8805), "30")), bty = 'n', horiz = T, xpd = T, xjust = 0.5, yjust = 0.5,
       text.width = NA)
dev.off()
