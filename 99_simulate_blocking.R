# 99_simulate_blocking.R
# does block randomisation help to balance participant characteristics?
# March 2022
library(ggplot2)
library(dplyr)
library(blockrand)
library(tidyr)

## key numbers
N_sim = 1000 # number of simulations
sample_size = 40 # sample size per group
N = sample_size*2 # total sample size
treat = c(rep(0, sample_size), rep(1, sample_size)) # simple vector of treatments
rho = 0.5 # similarity of participants

# AR(1) random data, from https://campus.datacamp.com/courses/optimizing-r-code-with-rcpp/case-studies-4?ex=11
ar1 <- function(n, constant, phi, eps) {
  p <- length(phi)
  x <- numeric(n)
  for(i in seq(p + 1, n)) {
    value <- rnorm(1, constant, eps)
    for(j in seq_len(p)) {
      value <- value + phi[j] * x[i - j]
    }
    x[i] <- value
  }
  x
}

# simulate participants that are correlated or not
simulate_participants = function(N=N, correlated = FALSE, rho=0.3){
  #
  if(correlated == FALSE){
    sim = data.frame(gender = rbinom(n = N, size=1, prob = 0.5),
             age = round(rnorm(n = N, mean = 45, sd = 10)))
  }
  #
  if(correlated == TRUE){
    age = round(45 + ar1(n = N+1, constant=0, phi = rho, eps=5))
    age = age[-1] # remove starting value
    sim = data.frame(gender = rbinom(n = N, size=1, prob = 0.5), # gender not yet correlated
                     age = age)
  }
  return(sim)
}


# big loop
overall = NULL
for (s in 1:N_sim){
  
  # simulate participants
  participants = simulate_participants(N=N, correlated = TRUE, rho=0.1) # turn off or on here; tried rho of 0.1 and 0.9
  
  ## now simulate simple and block randomisation
  # simple
  simple = mutate(participants, group = sample(treat, size=N, replace=FALSE))
  # block
  block = blockrand(n = N, block.sizes = c(4,6))
  blocked = mutate(participants, group = as.numeric(block$treatment[1:N]=='A'))
  
  ## check for between group differences
  # age in simple
  x = filter(simple, group==0) %>% pull(age)
  y = filter(simple, group==1) %>% pull(age)
  test_simple = t.test(x = x, y = y, alternative='two.sided', paired = FALSE)
  # age in blocked
  x = filter(blocked, group==0) %>% pull(age)
  y = filter(blocked, group==1) %>% pull(age)
  test_blocked = t.test(x = x, y = y, alternative='two.sided', paired = FALSE)
  # store results
  results = data.frame(sim = s, simple = test_simple$statistic, blocked = test_blocked$statistic)
  overall = bind_rows(overall, results)
  
} # end of loop
row.names(overall) = NULL

## compare
# versus standard normal
long = pivot_longer(overall, cols=c('simple','blocked'))
hplot = ggplot(data=long, aes(sample=value))+
  stat_qq()+
  stat_qq_line()+
  theme_bw()+
  facet_wrap(~name)
hplot

# summary statistics
group_by(long, name) %>%
  summarise(n=n(), mean=mean(value), sd=sd(value))

# answer
# block randomisation does help, but only if people are strongly correlated over time
# difference clear when rho = 0.9
# difference is barely detectable when rho = 0.1
