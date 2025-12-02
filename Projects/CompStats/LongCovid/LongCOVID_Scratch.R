library(dplyr)
library(ggplot2)
library(purrr)

long_covid <- data.frame(
  symptoms = c(rep(1, 23), rep(0, 106), rep(1, 81), rep(0, 214)),
  # symptoms = 1 if symptoms remain after 90 days, 0 if symptoms are gone within 90 days
  vax = c(rep("treatment", 129), rep("control", 295))
  # treatment = vaccinated, control = unvaccinated
)

##---PART 1-----

#like in class from estimating alphas
perm_test <- function(df, R = 999, alternative = "t") {
#df: data
  #column 1: response (0 = no long covid and 1 = long covid) (symptoms)
  #column 2: group (treatment or control) (vax)
#R: number of perm resamples to generate
  # alternative: type of test (l = left, r = right, t = two-sided)
  
#check the alt argument is valid
  assertthat::assert_that(
    alternative %in% c("l", "r", "t"),
    msg = 'Alternative must be "l", "r", or "t" '
  )
  
#get the response variable (long COVID yes/no)
  response <- df[, 1, drop = TRUE]
  
#get and convert the group variable to a factor (to separate treatment/control)
  group <- as.factor(df[, 2, drop = TRUE])
  
#storing group names
  group1 <- levels(group)[1]
  group2 <- levels(group)[2]
  

#STEP 1: Compute obs diff in props
  #mean of 0 and 1 values is sample prop
  p1_obs <- mean(response[group == group1])  # props of long COVID in group1
  p2_obs <- mean(response[group == group2])  # props of long COVID in group2
  
  #observed t.stat: diff in props
  diff_obs <- p1_obs - p2_obs
  

#STEP 2: Simulate permutation null dist

  diff_samp_prop <- function(ind, y) {
  # ind: indices assigned to treatment group
  #y: vector containing the response vals
    #mean(y[ind]): simulated treatment group proportion
    #mean(y[-ind]): simulated control group proportion
    
    diff_prop <- mean(y[ind]) -  mean(y[-ind]) #diff in proportions
    
    return(diff_prop)
  }
  
  #under the null hypothesis: group assignment doesnt matter
  #so rand move around who is in g1 vs g2
  index_sim <- replicate(
    R,
    {
      # randomly assign which patients are in the treatment group
      sample(seq_along(response), sum(group == group1), replace = FALSE)
    }
  )
  
  # --- part 3: compute simulated differences for each permutation ---
  diff_props_sim <- apply(
    index_sim,   # matrix of simulated treatment indices
    2,           # apply function to each column
    diff_samp_prop, # custom helper function
    y = response
  )

#STEP 3: Combine sim and obs stats
  
  diff_all <- c(diff_props_sim, diff_obs)

#STEP 4: Compute permutation pvals
 #left: probability of seeing a value <= obs
  p_left <- mean(diff_all <= diff_obs)
  
  #right: probability of seeing a value >= observed
  p_right <- mean(diff_all >= diff_obs)
  
  #two: smallest tail × 2
  p_two_sided <- min(p_left, p_right) * 2
  
#return results
  invisible(
    list(
      obs_test_stat = diff_obs,   # observed difference in proportions
      all_test_stat = diff_all,   # vector of simulated + observed stats
      p.val = dplyr::case_when(   # pick correct p-value type
        alternative == "l" ~ p_left,
        alternative == "r" ~ p_right,
        alternative == "t" ~ p_two_sided
      )
    )
  )
}


#testing:
results <- perm_test(long_covid, R = 999, alternative = "t")

#obs diff and p-value
results$obs_test_stat   #observed diff
results$p.val           #pval

##----PART 2----
  #GOAL 2:
p_values <- replicate(
  1000, #simulate 1000 times
  {
    #null: both groups have same long COVID rate 20%
    group1_response <- rbinom(100, size = 1, prob = 0.20) #use binom
    group2_response <- rbinom(100, size = 1, prob = 0.20)
    
    #combine into df for perm_test
    fake_data <- data.frame(
      response = c(group1_response, group2_response),
      group = c(rep("treatment", 100), rep("control", 100))
    )
    
    # run permutation test and get pval
    perm_test_sim<- perm_test(fake_data, R = 999, alternative = "t") |>
      purrr::pluck("p.val")
  }
)

#create empirical cdf of the pvals and plot it
plot.ecdf(p_values)
abline(a=0, b=1, col="magenta") #1-1 line
#x-axis represents signifcance level slpha
#y-axis represents p(pval <= alpha)
#since the ecdf doesnt line up with the 1-1 line then the pvals do not can from true null

  #GOAL 3:
p_values_reduc <- replicate(
  1000, 
  { 
    #generate responses
    group1_response_reduc <- rbinom(100, size = 1, prob = 0.20)  #20% long COVID rate
    group2_response_reduc <- rbinom(100, size = 1, prob = 0.15)  #15% long COVID rate (25% reducton since 0.20/.75= 0.15)
    
    # combine into df
    fake_data <- data.frame(
      response = c(group1_response_reduc, group2_response_reduc),
      group = c(rep("treatment", 100), rep("control", 100))
    )
    
    # run permutation test and extract p-value
    perm_test_simR <- perm_test(df = fake_data, R = 999, alternative = "t") |>
      purrr::pluck("p.val")
  }
)
alpha <- 0.05
#estimated power
mean(p_values_reduc <= alpha)




##----PART 3----
n <- 100
#if changing sample size instead use to give 10 conditions:
#sample_sizes <- seq(20, 200, length.out = 10)
alpha <- 0.05

#changing relative risk
RR <- 1  # fixed at 1 for now

#control probabilities
p_control_vals <- seq(0.10, 0.25, length.out = 10)
#prob_control <- 0.20 if changing treatment

#treatment probabilities
#p_treatment_vals <- seq(0.10, 0.25, length.out = 10) if changing treatment
p_treatment <- 0.20  #fixed otherwise

get_pval <- function(p_control) { #change to p_treat if changing treatment
  p_values <- replicate(
    1000,
    {
      group_treat <- rbinom(n, 1, p_treatment)
      group_ctrl  <- rbinom(n, 1, p_control)
      
      tbl <- table(
        response = c(group_treat, group_ctrl),
        group    = c(rep("treatment", n), rep("control", n))
      )
      fisher.test(tbl)$p.value
    }
  )
  #estimated power
  mean(p_values <= alpha)
}

#apply the function to all treatment probabilities
  #change "p_control_vals to p_treatment if changing that
power_estimates <- sapply(p_control_vals, get_pval)
  #sapply applies my function to the vectors and outputs a vector (??) of numbers

# Plot with help of chat to understand what the heck im looking at
plot(p_control_vals, power_estimates, type = "b", pch = 16, col = "magenta",
     xlab = "Treatment probability of Long COVID",
     ylab = "Estimated power",
     main = "Sensitivity Analysis: Power vs Treatment Probability")
abline(h = alpha, col = "grey", lty = 2)
