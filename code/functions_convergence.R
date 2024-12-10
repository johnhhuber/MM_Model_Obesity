# install necessary packages
if(!require(coda)){install.packages('coda'); library(coda)}

# function to return convergence statistics
get_convergence_stats <- function(path.in,
                                  file.pattern,
                                  burn.in,
                                  thin.factor)
{
  # list the necessary files 
  post.files <- list.files(path = path.in, pattern = file.pattern, full.names = T)
  
  # load the necessary posteriors 
  posts <- lapply(post.files, read.csv)
  
  # apply the burn in and thin
  posts <- lapply(1:length(posts), function(i){posts[[i]][seq(from = burn.in + 1,
                                                              to = nrow(posts[[i]]),
                                                              by = thin.factor),]})
  
  # convert to mcmc list 
  posts_coda <- as.mcmc.list(lapply(1:length(posts), function(i){as.mcmc(posts[[i]])}))
  
  # generate gelman rubin statistics
  print(gelman.diag(posts_coda))
}

# function to return convergence statistics
get_convergence_stats_files <- function(path.in,
                                  post.files,
                                  burn.in,
                                  thin.factor)
{
  # load the necessary posteriors 
  posts <- lapply(post.files, read.csv)
  
  # apply the burn in and thin
  posts <- lapply(1:length(posts), function(i){posts[[i]][seq(from = burn.in + 1,
                                                              to = nrow(posts[[i]]),
                                                              by = thin.factor),]})
  
  # convert to mcmc list 
  posts_coda <- as.mcmc.list(lapply(1:length(posts), function(i){as.mcmc(posts[[i]])}))
  
  # generate gelman rubin statistics
  gelman.diag(posts_coda)
}

# function to pool chains
pool_chain <- function(path.in,
                       file.pattern,
                       burn.in,
                       thin.factor,
                       file.out)
{
  # list the necessary files 
  post.files <- list.files(path = path.in, pattern = file.pattern, full.names = T)
  
  # load the necessary posteriors 
  posts <- lapply(post.files, read.csv)
  
  # apply the burn in and thin
  posts <- lapply(1:length(posts), function(i){posts[[i]][seq(from = burn.in + 1,
                                                              to = nrow(posts[[i]]),
                                                              by = thin.factor),]})
  
  # row bind the chains
  post_pooled <- do.call(rbind, posts)
  
  # write pool post to file 
  write.csv(post_pooled, file = file.out, row.names = F)
  
  # return quantiles of parameters
  signif(apply(post_pooled, 2, quantile, probs = c(0.025, 0.50, 0.975)), digits = 2)
}

# function to pool chains
pool_chain_files <- function(path.in,
                       post.files,
                       burn.in,
                       thin.factor,
                       file.out)
{
  # load the necessary posteriors 
  posts <- lapply(post.files, read.csv)
  
  # apply the burn in and thin
  posts <- lapply(1:length(posts), function(i){posts[[i]][seq(from = burn.in + 1,
                                                              to = nrow(posts[[i]]),
                                                              by = thin.factor),]})
  
  # row bind the chains
  post_pooled <- do.call(rbind, posts)
  
  # write pool post to file 
  write.csv(post_pooled, file = file.out, row.names = F)
  
  # return quantiles of parameters
  signif(apply(post_pooled, 2, quantile, probs = c(0.025, 0.50, 0.975)), digits = 2)
}

# function to calculate DIC 
DIC <- function(post,
                likelihood,
                num.samps = NA)
{
  
  # calculate likelihood 
  if(!is.na(num.samps)){samps <- sample(1:nrow(post), size = num.samps, replace = F)}else{
    samps <- 1:nrow(post)
  }
  ll <- sapply(samps, function(i){likelihood(as.numeric(post[i,]))})
  D = -2 * ll
  
  # calculate and return DIC 
  DIC <- 0.5 * var(D) + mean(D)
  return(DIC)
}

# function to calculate BIC 
BIC <- function(post,
                likelihood,
                num.samps = NA,
                num.data)
{
  
  # calculate likelihood 
  if(!is.na(num.samps)){samps <- sample(1:nrow(post), size = num.samps, replace = F)}else{
    samps <- 1:nrow(post)
  }
  ll <- sapply(samps, function(i){likelihood(as.numeric(post[i,]))})
  num.params <- ncol(post)
  
  # calculate and return DIC 
  BIC <- num.params * log(num.data) - 2 * max(ll)
  return(BIC)
}
