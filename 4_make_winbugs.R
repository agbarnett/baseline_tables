# make_winbugs.R
# make winbugs code; spike-and-slab for mean and inverse-variance
# used by 4_model_[...].R
# May 2021

# add statistic continuous/percent?

# write code
bfile = 'bugs_model.txt'
bugs = file(bfile, 'w')
cat('model
{
  for(i in 1:N){
      mdiff[i] ~ dt(mu[study[i]], tau[i], df[i])
      log(tau[i]) <- log(sem2[i]) + inv.var[study[i]] 
  }
  for(j in 1:N_studies){
  # spike-slab for mean
      mu[j] <- mu.beta[j, pick[j]]
      pick[j] <- mean.flag[j] + 1
      mean.flag[j]  ~ dbern(p_issue) # 
      mu.beta[j,1] <- 0 # spike at zero
      mu.beta[j,2] ~ dnorm(0, eps) # "slab": precision eps is small; two-sided
  # spike-slab for inverse-variance
      inv.var[j] <- equals(var.flag[j],1)* mu.var[j, 1] + equals(var.flag[j],2)* mu.var[j, 2] + equals(var.flag[j],3)* mu.var[j, 3]
      var.flag[j] ~ dcat(p_invvar[1:3]) # 
      mu.var[j,1] ~ dnorm(0, eps)I(,c) # negative slab
      mu.var[j,2] <- 0 # spike at zero
      mu.var[j,3] ~ dnorm(0, eps)I(c,) # "slab": precision eps is small; one-sided, c > 0
  }
}', file=bugs)
close(bugs)
