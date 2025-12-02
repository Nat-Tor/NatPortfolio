#packages
library(dplyr)
library(readr)
library(ggplot2)

#reading in data
judge <- read_csv("Project_3/judge.csv")

#SECTION 1:
  #GOAL1:
#general idea of what we want to get
#y1: need mu1 and sigma
  #mu1: t1, sigma, kappa0
    #sigma: alpha0, lambda0
#WLOG y2
#set t1, t1, alpha0, lambda0, kappa0

#set seed
set.seed(1222)

#messed around with parameters until i got not too much more then 120 mph
  #cause Gigi told me 120 is like... humanly impossible ._. so i listened
sim_prior_pred <- function(R = 1000,
                            t1 = 99,    # expected mean pre-injury
                            t2 = 90,     # expected mean post-injury
                            kappa0 = 5,    # strength of prior on means
                            alpha0 = 30,    # gamma shape for precision
                            lambda0 = 1000  # gamma rate for precision
) {
  # Step 1: Draw precision v from Gamma(alpha0/2, lambda0/2) to then get sigma
  v2 <- rgamma(R, shape = alpha0/2, rate = lambda0/2)
  sigma <- 1 / sqrt(v2)
  
  # Step 2: Draw mu1, mu2 given sigma and kappa0
  mu1 <- rnorm(R, mean = t1, sd = sigma / sqrt(kappa0))
  mu2 <- rnorm(R, mean = t2, sd = sigma / sqrt(kappa0))
  
  # step 3: Draw y1,y2 based on mu1/2 and sigma
  y1 <- rnorm(R, mean = mu1, sd = sigma) #pre-injury
  y2 <- rnorm(R, mean = mu2, sd = sigma) #post-injury
  
  #put it into a list to then make visuals for
  list(sigma = sigma, mu1 = mu1, mu2 = mu2, y1 = y1, y2 = y2)
}

#run it
#putting it into histograms
res <- sim_prior_pred()
par(mfrow = c(2,1))
hist(res$y1)
hist(res$y2)

#SECTION 2:
#GOAL 1:
#summarize data from the csv

launch_summary <- judge |>
  select(injury, launch_speed) |>
  group_by(injury) |>
  summarise(
    mean = mean(launch_speed),
    median = median(launch_speed),
    sd = sd(launch_speed),
    min = min(launch_speed),
    max = max(launch_speed)
  )

#mayve do a boxplot instead??
#did it
#setting the order for visual purposes
judge$injury <- factor(judge$injury, levels = c("Pre", "Post"))

ggplot(judge, aes(x = injury, y = launch_speed, fill = injury)) +
  geom_boxplot() +
  labs(
    title = "Launch Speed Pre vs Post Injury",
    y = "Launch Speed (mph)"
  ) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_light() 
#ummmm why does he look more consistent afterwards?

#SECTION 3:
#GOAL 1:

set.seed(1222)
#parameters from your prior predictive function
t1 <- 99
t2 <- 90
kappa0 <- 5
alpha0 <- 30
lambda0 <- 1000

#Data:
y1 <- judge$launch_speed[judge$injury == "Pre"]
y2 <- judge$launch_speed[judge$injury == "Post"]
n1 <- length(y1)
n2 <- length(y2)

# Storage matrix
param <- matrix(0, nrow = 5000, ncol = 3)


# Initial values from data
mu1 <- mean(y1)
mu2 <- mean(y2)
sigma <- sd(c(y1,y2))

# Gibbs Sampelr

for (i in 1:5000){
  
  #sample mu1 given mu 2and sigma
  mu_p1 <- (sum(y1) + kappa0*t1) / (n1 + kappa0)
  sd_p1 <- sigma / sqrt(n1 + kappa0)
  mu1 <- rnorm(1, mean = mu_p1, sd = sd_p1) #updated mu1
  
  #same for mu2 given mu 1 sigma
  mu_p2 <- (sum(y2) + kappa0*t2) / (n2 + kappa0)
  sd_p2 <- sigma / sqrt(n2 + kappa0)
  mu2 <- rnorm(1, mean = mu_p2, sd = sd_p2) #updated mu2
  
  #Sample sigma given mu1 and mu2
  #putting it in parts cauuuuse i caaan
  # precision v = 1/sigma^2
  
  #post shape based off of formula shape
  alpha_p <- alpha0 + n1 + n2
  
  #doing all this shabam for lambda_p based on formula sheet
  # Compute SSE + prior terms
  sum1 <- sum((y1 - mu1)^2)
  sum2 <- sum((y2 - mu2)^2)
  kappa_segment <- kappa0*((mu1 - t1)^2 + (mu2 - t2)^2)
  
  lambda_p <- lambda0 + sum1 + sum2 + kappa_segment
  
  #sample v
  v <- rgamma(1, shape = alpha_p/2, rate = lambda_p/2)
  sigma <- 1/sqrt(v)
  
  #store results
  param[i,] <- c(mu1, mu2, sigma)
}

#burn in removal
param_converged <- param[1001:5000,]

#plots
par(mfrow=c(2,2))
plot(param_converged[,1], type="l", main="mu1")
plot(param_converged[,2], type="l", main="mu2")
plot(param_converged[,3], type="l", main="sigma")

#section 4:
#goal 1:
#get last 4000 draws
mu1_post <- param_converged[,1]
mu2_post <- param_converged[,2]

# Compute posterior difference
diff_post <- mu1_post - mu2_post

#plot
par(mfrow = c(1,1)) #change it back to 1 plot
hist(diff_post,
     breaks = 40,
     col = "skyblue",
     main = "Posterior Distribution of μ1 − μ2",
     xlab = "μ1 − μ2")

#90% credible interval
quantile(diff_post, c(0.05,0.95))


