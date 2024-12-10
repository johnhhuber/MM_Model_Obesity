# set working directory
setwd('~/Dropbox/MM_Model_Trend/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(nord)){install.packages('nord'); library(nord)}

# list the necessary files
post.files <- list.files(path = '../output/single_year_analysis/bmi_mgus_mm/', pattern = 'posterior_.*bz2', full.names = T)

# load the posteriors
posts <- lapply(post.files, read.csv)

# specify burn in and thin factor
burn.in = 5e5
num.samples = nrow(posts[[1]])
thin.factor = 50

indices <- seq(from = burn.in + 1, to = num.samples, by = thin.factor)

# generate palette
palette <- nord(palette = 'aurora', n = 5)

# vector of parameter names
params.names <- c(expression(gamma['MGUS']),
                  expression(beta['MGUS,a']),
                  expression(beta['MGUS,s']),
                  expression(beta['MGUS,r']),
                  expression(gamma['MM']),
                  expression(beta['MM,a']),
                  expression(beta['MM,'*'a'^2]),
                  expression(beta['MM,s']),
                  expression(beta['MM,r']),
                  expression(tau^2))

# generate plot 
jpeg(filename = '../output/figs_new/fig_S1.jpg', width = 6, height = 6, units = 'in', res = 500)
par(mar = c(3.3,5.0,1.6,1.3))
layout(mat = matrix(1:9, nrow = 3, ncol = 3, byrow = T))
for(ii in 1:ncol(posts[[1]]))
{
  plot(posts[[1]][indices,ii], type = 'n', axes = 'F', xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  for(jj in 1:length(posts))
  {
    lines(posts[[jj]][indices,ii], col = palette[jj])
  }
  box()
  axis(side = 1)
  axis(side = 2, las = 1)
  mtext(side = 3, line = 0, params.names[ii])
  if(ii > 6){mtext(side = 1, line = 2.3, 'Sample')} 
  if(ii %% 3 == 1){mtext(side = 2, line = 3.7, 'Value')}
}
dev.off()