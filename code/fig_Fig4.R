# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# load necessary data
load('../output/fig_4.RData')

# convert mi to proportions
mi_nhw_male <- mi_nhw_male / 100
mi_nhw_female <- mi_nhw_female / 100
mi_nhb_male <- mi_nhb_male / 100
mi_nhb_female <- mi_nhb_female / 100

# multiply the relative components by the absolute predictive value 
mi_nhw_male[-1] <- mi_nhw_male[-1] * mi_nhw_male[1]
mi_nhw_female[-1] <- mi_nhw_female[-1] * mi_nhw_female[1]
mi_nhb_male[-1] <- mi_nhb_male[-1] * mi_nhb_male[1]
mi_nhb_female[-1] <- mi_nhb_female[-1] * mi_nhb_female[1]

# specify color palette
palette = c('#158644', '#ba4e41', '#34a2cc', '#e5bc20')

# plotting parameters
bar_width <- 0.7
border_col = NA

# generate plot 
jpeg(filename = '../output/figs_new/Fig_4.jpg', width = 6.5, height = 5, units = 'in', res = 500)
par(mar = c(2.3,3.3,1.1,1.1))
plot(NA,NA, xlim = c(0.5, 4.5), ylim = c(0,1), xaxs = 'i', yaxs = 'i', las = 1, xlab = '', ylab = '', bty = 'n', axes = F)
rect(xleft = 1 - 0.5 * bar_width,
     xright = 1 + 0.5 * bar_width,
     ybottom = 0, ytop = mi_nhw_male[2], col = palette[1], border = border_col)
text(x = 1, y = mean(c(0, mi_nhw_male[2])), signif(mi_nhw_male[2] * 100, digits = 3), col = 'white')
rect(xleft = 1 - 0.5 * bar_width,
     xright = 1 + 0.5 * bar_width,
     ybottom = mi_nhw_male[2], ytop = sum(mi_nhw_male[2:3]), col = palette[3], border = border_col)
text(x = 1, y = mean(c(mi_nhw_male[2], sum(mi_nhw_male[2:3]))), signif(mi_nhw_male[3] * 100, digits = 3), col = 'white')
rect(xleft = 1 - 0.5 * bar_width,
     xright = 1 + 0.5 * bar_width,
     ybottom = sum(mi_nhw_male[2:3]), ytop = sum(mi_nhw_male[2:4]), col = palette[2], border = border_col)
text(x = 1, y = mean(c(sum(mi_nhw_male[2:3]), sum(mi_nhw_male[2:4]))), signif(mi_nhw_male[4] * 100, digits = 3), col = 'white')
rect(xleft = 1 - 0.5 * bar_width,
     xright = 1 + 0.5 * bar_width,
     ybottom = sum(mi_nhw_male[2:4]), ytop = sum(mi_nhw_male[2:5]), col = palette[4], border = border_col)
text(x = 1, y = mean(c(sum(mi_nhw_male[2:4]), sum(mi_nhw_male[2:5]))), signif(mi_nhw_male[5] * 100, digits = 3), col = 'white')
rect(xleft = 1 - 0.5 * bar_width,
     xright = 1 + 0.5 * bar_width,
     ybottom = 0, ytop = sum(mi_nhw_male[2:5]), col = NA, border = '#222222')
text(x = 1, y = 1.05 * mi_nhw_male[1], signif(mi_nhw_male[1] * 100, digits = 3), col = '#222222')

rect(xleft = 2 - 0.5 * bar_width,
     xright = 2 + 0.5 * bar_width,
     ybottom = 0, ytop = mi_nhw_female[2], col = palette[1], border = border_col)
text(x = 2, y = mean(c(0, mi_nhw_female[2])), signif(mi_nhw_female[2] * 100, digits = 3), col = 'white')
rect(xleft = 2 - 0.5 * bar_width,
     xright = 2 + 0.5 * bar_width,
     ybottom = mi_nhw_female[2], ytop = sum(mi_nhw_female[2:3]), col = palette[3], border = border_col)
text(x = 2, y = mean(c(mi_nhw_female[2], sum(mi_nhw_female[2:3]))), signif(mi_nhw_female[3] * 100, digits = 3), col = 'white')
rect(xleft = 2 - 0.5 * bar_width,
     xright = 2 + 0.5 * bar_width,
     ybottom = sum(mi_nhw_female[2:3]), ytop = sum(mi_nhw_female[2:4]), col = palette[2], border = border_col)
text(x = 2, y = mean(c(sum(mi_nhw_female[2:3]), sum(mi_nhw_female[2:4]))), signif(mi_nhw_female[4] * 100, digits = 3), col = 'white')
rect(xleft = 2 - 0.5 * bar_width,
     xright = 2 + 0.5 * bar_width,
     ybottom = sum(mi_nhw_female[2:4]), ytop = sum(mi_nhw_female[2:5]), col = palette[4], border = border_col)
text(x = 2, y = mean(c(sum(mi_nhw_female[2:4]), sum(mi_nhw_female[2:5]))), signif(mi_nhw_female[5] * 100, digits = 3), col = 'white')
rect(xleft = 2 - 0.5 * bar_width,
     xright = 2 + 0.5 * bar_width,
     ybottom = 0, ytop = sum(mi_nhw_female[2:5]), col = NA, border = '#222222')
text(x = 2, y = 1.05 * mi_nhw_female[1], signif(mi_nhw_female[1] * 100, digits = 3), col = '#222222')

rect(xleft = 3 - 0.5 * bar_width,
     xright = 3 + 0.5 * bar_width,
     ybottom = 0, ytop = mi_nhb_male[2], col = palette[1], border = border_col)
text(x = 3, y = mean(c(0, mi_nhb_male[2])), signif(mi_nhb_male[2] * 100, digits = 3), col = 'white')
rect(xleft = 3 - 0.5 * bar_width,
     xright = 3 + 0.5 * bar_width,
     ybottom = mi_nhb_male[2], ytop = sum(mi_nhb_male[2:3]), col = palette[3], border = border_col)
text(x = 3, y = mean(c(mi_nhb_male[2], sum(mi_nhb_male[2:3]))), signif(mi_nhb_male[3] * 100, digits = 3), col = 'white')
rect(xleft = 3 - 0.5 * bar_width,
     xright = 3 + 0.5 * bar_width,
     ybottom = sum(mi_nhb_male[2:3]), ytop = sum(mi_nhb_male[2:4]), col = palette[2], border = border_col)
text(x = 3, y = mean(c(sum(mi_nhb_male[2:3]), sum(mi_nhb_male[2:4]))), signif(mi_nhb_male[4] * 100, digits = 3), col = 'white')
rect(xleft = 3 - 0.5 * bar_width,
     xright = 3 + 0.5 * bar_width,
     ybottom = sum(mi_nhb_male[2:4]), ytop = sum(mi_nhb_male[2:5]), col = palette[4], border = border_col)
text(x = 3, y = mean(c(sum(mi_nhb_male[2:4]), sum(mi_nhb_male[2:5]))), signif(mi_nhb_male[5] * 100, digits = 3), col = 'white')
rect(xleft = 3 - 0.5 * bar_width,
     xright = 3 + 0.5 * bar_width,
     ybottom = 0, ytop = sum(mi_nhb_male[2:5]), col = NA, border = '#222222')
text(x = 3, y = 1.05 * mi_nhb_male[1], signif(mi_nhb_male[1] * 100, digits = 3), col = '#222222')

rect(xleft = 4 - 0.5 * bar_width,
     xright = 4 + 0.5 * bar_width,
     ybottom = 0, ytop = mi_nhb_female[2], col = palette[1], border = border_col)
text(x = 4, y = mean(c(0, mi_nhb_female[2])), signif(mi_nhb_female[2] * 100, digits = 3), col = 'white')
rect(xleft = 4 - 0.5 * bar_width,
     xright = 4 + 0.5 * bar_width,
     ybottom = mi_nhb_female[2], ytop = sum(mi_nhb_female[2:3]), col = palette[3], border = border_col)
text(x = 4, y = mean(c(mi_nhb_female[2], sum(mi_nhb_female[2:3]))), signif(mi_nhb_female[3] * 100, digits = 3), col = 'white')
rect(xleft = 4 - 0.5 * bar_width,
     xright = 4 + 0.5 * bar_width,
     ybottom = sum(mi_nhb_female[2:3]), ytop = sum(mi_nhb_female[2:4]), col = palette[2], border = border_col)
text(x = 4, y = mean(c(sum(mi_nhb_female[2:3]), sum(mi_nhb_female[2:4]))), signif(mi_nhb_female[4] * 100, digits = 3), col = 'white')
rect(xleft = 4 - 0.5 * bar_width,
     xright = 4 + 0.5 * bar_width,
     ybottom = sum(mi_nhb_female[2:4]), ytop = sum(mi_nhb_female[2:5]), col = palette[4], border = border_col)
text(x = 4, y = mean(c(sum(mi_nhb_female[2:4]), sum(mi_nhb_female[2:5]))), signif(mi_nhb_female[5] * 100, digits = 3), col = 'white')
rect(xleft = 4 - 0.5 * bar_width,
     xright = 4 + 0.5 * bar_width,
     ybottom = 0, ytop = sum(mi_nhb_female[2:5]), col = NA, border = '#222222')
text(x = 4, y = 1.05 * mi_nhb_female[1], signif(mi_nhb_female[1] * 100, digits = 3), col = '#222222')

box()
axis(side = 2, las = 1, at = seq(from = 0, to = 1, by = 0.2), labels = seq(from = 0, to = 100, by = 20))
axis(side = 1, at = 1:4, labels = c('White Men', 'White Women', 'Black Men', 'Black Women'), tick = F)
mtext(side = 2, line = 2.3, 'Contribution to Multiple Myeloma Incidence (%)')

legend(x = 2.5, y = 1.075, horiz = T, pch = 15, col = palette[c(1,3,2,4)], legend = c('Age Effect', 'Period Effect', 'Cohort Effect', 'BMI Effect'), bty = 'n',
       xjust = 0.5, xpd = T, pt.cex = 1.5)

dev.off()