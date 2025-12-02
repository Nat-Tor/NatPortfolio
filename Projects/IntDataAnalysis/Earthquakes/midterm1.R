library(readr)
library(ggplot2)
midterm1 <- read_csv("midterm1.csv")
# Log transformation
x <- midterm1$mainshock
y <- midterm1$aftershock
#transforming y
trans.y = function(lambda){
  gy = (y^lambda - 1)/lambda
  SSR = sum((lm(x~gy)$residuals)^2)
  SSR
}

lam = optim(1,trans.y)$par
gy = (y^lam)/lam

# Fit linear model
model <- lm(gy ~ x)

par(mfrow=c(2,2))
plot(model)
# Residual plots
par(mfrow=c(2,2))
plot(model)
# Model summary
summary(model)
#part b
newdata <- data.frame(x = 5)
pred_gy <- predict(model, newdata = newdata, interval = "confidence")  # for mean
pred_gy_pred <- predict(model, newdata = newdata, interval = "prediction")  # for individual
pred_gy
pred_gy_pred

# Back-transform to get expected mean aftershock
lam <- lam  # your Box-Cox lambda
conf_int <- (lam * pred_gy)^(1/lam)
pred_int <- (lam * pred_gy_pred)^(1/lam)

conf_int
pred_int
library(ggplot2)

# Predicted values for plotting regression line
pred_gy <- predict(model)
pred_y <- (lam * pred_gy)^(1/lam)

# Predicted aftershock for mainshock 5
mainshock_5 <- 5
pred_gy_5 <- predict(model, newdata = data.frame(x = mainshock_5))
pred_y_5 <- (lam * pred_gy_5)^(1/lam)

# Confidence interval (from predict with interval="confidence")
pred_ci <- predict(model, newdata = data.frame(x = mainshock_5), interval="confidence")
pred_ci_y <- (lam * pred_ci)^(1/lam)

ggplot(midterm1, aes(x = mainshock, y = aftershock)) +
  geom_point() +
  geom_line(aes(y = pred_y), color = "red", size = 1) +
  geom_point(aes(x = mainshock_5, y = pred_y_5), color = "forestgreen", size = 3) +
  geom_errorbar(aes(x = mainshock_5, ymin = pred_ci_y[,"lwr"], ymax = pred_ci_y[,"upr"]), width = 0.1, color = "green") +
  labs(x = "Mainshock Magnitude", y = "Aftershock Magnitude",
       title = "Expected Aftershock Magnitude for Mainshock = 5.0") + theme_minimal()

# Assume you already have:
# x = mainshock
# y = aftershock
# lam = Box-Cox lambda
# model_log = lm(gy ~ x), where gy = (y^lam)/lam

set.seed(123)  # for reproducibility
n <- length(x)
B <- 10000  # number of bootstrap iterations

# Store bootstrap predictions
BS.ystar <- numeric(B)

# Original model residuals on transformed scale
orig.res <- residuals(model)

for(i in 1:B){
  
  # Step 1: Resample x and residuals
  BS.x <- sample(x, n, replace = TRUE)
  BS.res <- sample(orig.res, n, replace = TRUE)
  
  # Step 2: Predicted gy for bootstrap x
  BS.yhat <- predict(model, newdata = data.frame(x = BS.x))
  
  # Step 3: Generate BS transformed y
  BS.y <- BS.yhat + BS.res
  
  # Step 4: Fit bootstrap model
  BS.model <- lm(BS.y ~ BS.x)
  
  # Step 5: Predict transformed y at mainshock = 5 and add a residual
  BS.ystar[i] <- predict(BS.model, newdata = data.frame(BS.x = 5)) + sample(orig.res, 1)
  
}

# Step 6: Back-transform to original scale
BS.y_original <- (lam * BS.ystar)^(1/lam)

# Step 7: Compute 95% prediction interval
PI <- quantile(BS.y_original, c(0.025, 0.975))
PI

# Step 8: Compute probability aftershock > 6.0
prob_gt6 <- mean(BS.y_original > 6)
prob_gt6

dev.off()
hist(BS.y_original, breaks = 20, col = "#C4A484",
     main = "Bootstrap Distribution of Aftershock Magnitudes\n(Mainshock = 5.0)",
     xlab = "Aftershock Magnitude")
abline(v = 6, col = "red", lwd = 2, lty = 2)  # highlight threshold
