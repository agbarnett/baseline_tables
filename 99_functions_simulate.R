# 99_functions_simulate.R
# functions for simulating baseline table data
# November 2021

### simulate baseline table data from one paper in the same format as extracted data
simulate_table1 = function(
  loop = NA, # for numbering studies
  rho = 0, # correlation between groups
  prop_continuous = 0.33, # what proportion of statistics are continuous (remainder are percent)
  gamma_table = c(2.2, 0.15), # gamma shape and rate for the number of table rows (based on real data)
  gamma_sample_size = c(10.7, 2.84), # gamma shape and rate for the log-transformed sample size (based on real data)
  exp_sample_size = TRUE, # exponentiate sample size (if parameters above are on log-scale)
  min_sample_size = 4, # minimum sample size to avoid errors
  add_skew = FALSE, # add skew to continuous variables
  dp = 1, # decimal places for rounding
  issue = 'none' # issue with the data, `none`, `too variable` for groups are too different, `too precise` for groups are too similar
){
  
  sim_data = NULL
  
  # set up rho
  if(rho < 0){cat('Correlation must be positive.\n')}
  if(issue=='none'){rho = 0}
  if(issue=='too variable'){rho = rho * -1} # negative correlation to simulate groups that are too variable
  
  # generate number of rows per baseline table 
  n_rows = round(rgamma(n=1, shape = gamma_table[1], rate=gamma_table[2]))
  if(n_rows <= 2){n_rows = 3} # prevent missing or very small tables
  
  # generate statistic for each row
  statistic = rbinom(n=n_rows, size=1, prob=prop_continuous) 
  statistic = c('percent','continuous')[statistic+1]
  
  # randomly generate sample size per groups using gamma (assume the same sample size in both columns)
  sample_size = rgamma(n = 1, shape = gamma_sample_size[1], rate=gamma_sample_size[2])
  if(exp_sample_size == TRUE){sample_size = exp(sample_size)}
  sample_size = max(round(sample_size), min_sample_size)
  
  ## for percents - not updated after using correlation approach
  n_percent = sum(statistic == 'percent')
  if(n_percent > 0){
    p_1 = runif(n=n_percent, min=0, max=1) # random p's from 0 to 1
    r_1 = rbinom(n=n_percent, size=sample_size, prob=p_1) # Numbers in group 1
    p_2 = p_1 # assume no difference to start
    if(issue == 'too variable'){
      p_2 = p_1 * diff # simulate difference between groups
    }
    r_2 = rbinom(n = n_percent, size=sample_size, prob=p_2) # Numbers in group 2 (no difference)
    if(issue == 'too precise'){
      r_2 = r_1 + sample(x=c(-2,-1,0,1,2), replace=TRUE, size=n_percent) # Just add 1 to 2 either side
      # avoid impossible numbers:
      r_2 = pmax(r_2, 0)
      r_2 = pmin(r_2, sample_size)
    }
    # create data with structure to match automatically extracted data
    p_frame_1 = data.frame(statistic='percent', row=1:n_percent, column=1, stat1=r_1, sample_size = sample_size)
    p_frame_2 = data.frame(statistic='percent', row=1:n_percent, column=2, stat1=r_2, sample_size = sample_size)
    ## concatenate data
    sim_data = bind_rows(sim_data, p_frame_1, p_frame_2)
  }
  
  ## for continuous
  n_continuous = sum(statistic == 'continuous')
  if(n_continuous > 0){
    mean1 = rnorm(n=n_continuous, mean=50, sd=100) # mean can be pretty much anywhere
    sd = rgamma(n=n_continuous, shape=5, rate=1)
    # use standard normal if using skew (avoid exponentiating large means)
    if(add_skew == TRUE){
      mean1 = rep(0, n_continuous)
      sd = rep(1, n_continuous)
    }
    m_1 = m_2 = sd_1 = sd_2 = NULL
    for (k in 1:n_continuous){ # simulate individual observations; have to loop through rows
      Sigma = matrix(c(1, rho, rho, 1), ncol=2) * sd[k]^2 # assume same sd
      m = mvrnorm(n = sample_size, 
                  mu = rep(mean1[k], 2),  # assume same mean in two groups
                  Sigma=Sigma) # use multivariate normal to create correlated groups
      if(add_skew == TRUE){
        m = exp(m) # to create skew
      }
      m_1 = c(m_1, mean(m[,1]))
      m_2 = c(m_2, mean(m[,2]))
      sd_1 = c(sd_1, sd(m[,1]))
      sd_2 = c(sd_2, sd(m[,2]))
    }
    # round to mimic journal presentation
    m_1 = round(m_1, dp)
    m_2 = round(m_2, dp)
    sd_1 = round(sd_1, dp+1)
    sd_2 = round(sd_2, dp+1)
    # create data with structure to match automatically extracted data
    m_frame_1 = data.frame(statistic='continuous', row=n_percent + (1:n_continuous), column=1, stat1=m_1, stat2=sd_1, sample_size = sample_size)
    m_frame_2 = data.frame(statistic='continuous', row=n_percent + (1:n_continuous), column=2, stat1=m_2, stat2=sd_2, sample_size = sample_size)
    ## concatenate data
    sim_data = bind_rows(sim_data, m_frame_1, m_frame_2)
  }
  
  # create dummy pmcid that includes the issue
  #pmcid = paste(issue, paste(sample(0:9, size=6, replace=TRUE), sep='', collapse = ''), sep='')
  pmcid = paste(issue, loop, sep='', collapse = '') # using the loop number
  pmcid = gsub(pattern=' ', replacement = '', pmcid) # remove spaces
  sim_data$issue = issue
  sim_data$pmcid = pmcid
  return(sim_data)
  
} # end of function

### alternative that creates problem tables by adding numbers to first column
simulate_table1_alternative = function(
  loop = NA, # for numbering studies
  prop_continuous = 0.33, # what proportion of statistics are continuous (remainder are percent)
  gamma_table = c(2.2, 0.15), # gamma shape and rate for the number of table rows (based on real data)
  gamma_sample_size = c(10.7, 2.84), # gamma shape and rate for the log-transformed sample size (based on real data)
  exp_sample_size = TRUE, # exponentiate sample size (if parameters above are on log-scale)
  min_sample_size = 4, # minimum sample size to avoid errors
  dp = 1, # decimal places for rounding
  issue = 'none' # issue with the data, `none`, `too variable` for groups are too different, `too precise` for groups are too similar
){
  
  sim_data = NULL
  
  # generate number of rows per baseline table 
  n_rows = round(rgamma(n=1, shape = gamma_table[1], rate=gamma_table[2]))
  if(n_rows <= 2){n_rows = 3} # prevent missing or very small tables
  
  # generate statistic for each row
  statistic = rbinom(n=n_rows, size=1, prob=prop_continuous) 
  statistic = c('percent','continuous')[statistic+1]
  
  # randomly generate sample size per groups using gamma (assume the same sample size in both columns)
  sample_size = rgamma(n = 1, shape = gamma_sample_size[1], rate=gamma_sample_size[2])
  if(exp_sample_size == TRUE){sample_size = exp(sample_size)}
  sample_size = max(round(sample_size), min_sample_size)
  
  ## for percents - not updated after using correlation approach
  n_percent = sum(statistic == 'percent')
  if(n_percent > 0){
    p_1 = runif(n=n_percent, min=0, max=1) # random p's from 0 to 1
    r_1 = rbinom(n=n_percent, size=sample_size, prob=p_1) # Numbers in group 1
    r_2 = rbinom(n=n_percent, size=sample_size, prob=p_1) # Numbers in group 2 (start as no issue)
    # then create numbers in group 2 ... (centred on count for group 1)
    if(issue == 'too variable'){ # add a big number
      r_2 = r_1 + sample(x=c(-sample_size/2,0,sample_size/2), replace=TRUE, size=n_percent) # add relatively large number to either side
    }
    if(issue == 'too precise'){ # add a small number
      r_2 = r_1 + sample(x=c(-2,-1,0,1,2), replace=TRUE, size=n_percent) # Just add 1 to 2 either side
    }
    # avoid impossible numbers:
    r_2 = pmax(r_2, 0)
    r_2 = pmin(r_2, sample_size)
    # create data with structure to match automatically extracted data
    p_frame_1 = data.frame(statistic='percent', row=1:n_percent, column=1, stat1=r_1, sample_size = sample_size)
    p_frame_2 = data.frame(statistic='percent', row=1:n_percent, column=2, stat1=r_2, sample_size = sample_size)
    ## concatenate data
    sim_data = bind_rows(sim_data, p_frame_1, p_frame_2)
  }
  
  ## for continuous
  n_continuous = sum(statistic == 'continuous')
  if(n_continuous > 0){
    mean1 = rnorm(n=n_continuous, mean=50, sd=60) # mean can be pretty much anywhere, more often positive
    sd = rgamma(n=n_continuous, shape=5, rate=1) # must be positive
    m_1 = m_2 = sd_1 = sd_2 = NULL
    for (k in 1:n_continuous){ # simulate individual observations; have to loop through rows
      m1 = rnorm(n = sample_size, 
                mean = mean1[k], # 
                sd = sd[k]) # 
      m2 = rnorm(n = sample_size, 
                 mean = mean1[k],  # assume same mean in two groups
                 sd = sd[k]) # use multivariate normal to create correlated groups
      m_1 = c(m_1, mean(m1))
      sd_1 = c(sd_1, sd(m1))
      sd_2 = c(sd_2, sd(m2))
      mean2 = mean(m2)
      # then create statistics in group 2 (if there's an issue) ... (only change mean, not SD)
      # addition scaled to the SD; centred on mean for group 1
      if(issue == 'too variable'){ # add a big number
        scale = 1
        mean2 = mean1 + sample(x=c(-sd[k]/scale, 0, sd[k]/scale), size=1) # add relatively large number
      }
      if(issue == 'too precise'){ # add a small number
        scale = 10
        mean2 = mean1 + sample(x=c(-sd[k]/scale, 0, sd[k]/scale), size=1) # add small number
      }
      m_2 = c(m_2, mean2)
    }
    # round to mimic journal presentation
    m_1 = round(m_1, dp)
    m_2 = round(m_2, dp)
    sd_1 = round(sd_1, dp+1)
    sd_2 = round(sd_2, dp+1)
    # create data with structure to match automatically extracted data
    m_frame_1 = data.frame(statistic='continuous', row=n_percent + (1:n_continuous), column=1, stat1=m_1, stat2=sd_1, sample_size = sample_size)
    m_frame_2 = data.frame(statistic='continuous', row=n_percent + (1:n_continuous), column=2, stat1=m_2, stat2=sd_2, sample_size = sample_size)
    ## concatenate data
    sim_data = bind_rows(sim_data, m_frame_1, m_frame_2)
  }
  
  # create dummy pmcid that includes the issue
  pmcid = paste(issue, loop, sep='', collapse = '') # using the loop number
  pmcid = gsub(pattern=' ', replacement = '', pmcid) # remove spaces
  sim_data$issue = issue
  sim_data$pmcid = pmcid
  return(sim_data)
  
} # end of function
