# make_winbugs.R
# make winbugs code; spike-and-slab for mean and inverse-variance
# used by 4_model_[...].R
# May 2021

# add statistic continuous/percent as an independent variable?

# write the code
bfile = 'bugs_model.txt'
bugs = file(bfile, 'w')
cat('model
{
  for(i in 1:N){
      mdiff[i] ~ dt(mu[study[i]], tau[i], df[i])
      tau[i] <- sem2[i] * inv.var[study[i]] # precision
  }
  for(j in 1:N_studies){
  # spike-slab for mean
      mu[j] <- mu.beta[j, pick[j]]
      pick[j] <- mean.flag[j] + 1
      mean.flag[j]  ~ dbern(p_mean) # 
      mu.beta[j,1] <- 0 # spike at zero (no difference in groups)
      mu.beta[j,2] ~ dnorm(0, eps) # "slab": precision eps is small; two-sided
  # spike-slab for inverse-variance
      log(inv.var[j]) <- equals(var.flag[j], 1) * mu.var[j, 1] + equals(var.flag[j], 2)* mu.var[j, 2] + equals(var.flag[j], 3)* mu.var[j, 3]
      var.flag[j] ~ dcat(p_precision[1:3]) # 
      mu.var[j,1] ~ dnorm(0, 0.1)I(,0) # "slab": one-sided and negative; had to keep precision relatively small
      mu.var[j,2] <- 0 # spike at zero (no change in precision)
      mu.var[j,3] ~ dnorm(0, 0.1)I(0,) # "slab": one-sided and positive
  }
  # probabilities
  p_mean ~ dbeta(1, 1)
  p_precision[1:3] ~ ddirch(ones[1:3])
}', file=bugs)
close(bugs)

# without mean difference
bfile = 'bugs_model.txt'
bugs = file(bfile, 'w')
cat('model
{
  for(i in 1:N){
      mdiff[i] ~ dt(0, tau[i], df[i])
      tau[i] <- sem2[i] * inv.var[study[i]] # precision
  }
  for(j in 1:N_studies){
  # spike-slab for inverse-variance
      log(inv.var[j]) <- equals(var.flag[j], 1) * mu.var[j, 1] + equals(var.flag[j], 2)* mu.var[j, 2] + equals(var.flag[j], 3)* mu.var[j, 3]
      var.flag[j] ~ dcat(p_precision[1:3]) # 
      mu.var[j,1] ~ dnorm(0, 0.1)I(,0) # "slab": one-sided and negative; had to keep precision relatively small
      mu.var[j,2] <- 0 # spike at zero (no change in precision)
      mu.var[j,3] ~ dnorm(0, 0.1)I(0,) # "slab": one-sided and positive
  }
  # probabilities
  p_precision[1:3] ~ ddirch(ones[1:3])
}', file=bugs)
close(bugs)


# without mean difference and simpler spike-slab
bfile = 'bugs_model.txt'
bugs = file(bfile, 'w')
cat('model
{
  for(i in 1:N){
      mdiff[i] ~ dt(0, tau[i], df[i])
      tau[i] <- inv.sem2[i] * inv.var[study[i]] # precision
  }
  for(j in 1:N_studies){
  # spike-slab for inverse-variance
      log(inv.var[j]) <- mu.var[j, pick[j]]
      pick[j] <- var.flag[j] + 1
      var.flag[j] ~ dbern(p_precision) # 
      mu.var[j,1] <- 0 # spike at zero (no change in precision)
      mu.var[j,2] ~ dnorm(0, 0.1) # "slab"
  }
  # probabilities
  p_precision ~ dbeta(1,1)
}', file=bugs)
close(bugs)

# as previous, but without hyper parameter for theta
bfile_no_hyper = 'bugs_model_no_hyper.txt'
bugs = file(bfile_no_hyper, 'w')
cat('model
{
  for(i in 1:N){
      mdiff[i] ~ dt(0, tau[i], df[i])
      tau[i] <- inv.sem2[i] * inv.var[study[i]] # precision
  }
  for(j in 1:N_studies){
  # spike-slab for inverse-variance
      log(inv.var[j]) <- mu.var[j, pick[j]]
      pick[j] <- var.flag[j] + 1
      var.flag[j] ~ dbern(', prior, ') # 
      mu.var[j,1] <- 0 # spike at zero (no change in precision)
      mu.var[j,2] ~ dnorm(0, 0.1) # "slab"
  }
}', sep='', file=bugs)
close(bugs)

# as previous, but for a single study (no vectors)
bfile_no_hyper_single = 'bugs_model_no_hyper_single_study.txt'
bugs = file(bfile_no_hyper_single, 'w')
cat('model
{
  for(i in 1:N){
      mdiff[i] ~ dt(0, tau[i], df[i])
      tau[i] <- inv.sem2[i] * inv.var # precision
  }
  # spike-slab for inverse-variance
  log(inv.var) <- mu.var[pick]
  pick <- var.flag + 1
  var.flag ~ dbern(', prior, ') # 
  mu.var[1] <- 0 # spike at zero (no change in precision)
  mu.var[2] ~ dnorm(0, 0.1) # "slab"
}', sep='', file=bugs)
close(bugs)

# version for multiple studies with study-specific probability; results show no big difference compared with fixed var.flag[j] ~ dbern(0.5)
bfile_study_specific = 'bugs_model_study_specific.txt'
bugs = file(bfile_study_specific, 'w')
cat('model
{
  for(i in 1:N){
      mdiff[i] ~ dt(0, tau[i], df[i])
      tau[i] <- inv.sem2[i] * inv.var[study[i]] # precision
  }
  for(j in 1:N_studies){
  # spike-slab for inverse-variance
      log(inv.var[j]) <- mu.var[j, pick[j]]
      pick[j] <- var.flag[j] + 1
      var.flag[j] ~ dbern(theta[j]) # 
      theta[j] ~ dbeta(1,1)
      mu.var[j,1] <- 0 # spike at zero (no change in precision)
      mu.var[j,2] ~ dnorm(0, 0.1) # "slab"
  }
}', sep='', file=bugs)
close(bugs)
