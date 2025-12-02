#read in data
library(readr)
Cars <- read_csv("CaseStudy2/Cars.csv")
View(Cars)

summary(Cars)

#making linear model based on ALL the variables
model <- lm(SuggestedRetailPrice ~ 
            Hybrid + EngineSize + Cylinders + Horsepower + HighwayMPG + Weight + WheelBase, 
            data = Cars)
summary(model)

#plots
par(mfrow=c(2,2))
plot(model)

AIC(model) #how significance the varaibles are
anova(model) #measuring variance

#renaming got it from Ashley
x1 = Cars$Hybrid
x2 = Cars$EngineSize
x3 = Cars$Cylinders
x4 = Cars$Horsepower
x5 = Cars$HighwayMPG
x6 = Cars$Weight
x7 = Cars$WheelBase
y = Cars$SuggestedRetailPrice
#remvoing outlier
  #removed because it was only electric so it was expensive
  #only engine with 3 cylinders
data1 = Cars[-67,] 


X = data.frame(x1,x2,x3,x4,x5,x6,x7)
Y = y
YX = data.frame(y,x1,x2,x3,x4,x5,x6,x7)

power.trans = function(lambda){
  Y.trans = (Y^lambda - 1)/(lambda)
  Y.hat = lm(YX)$fit	
  RSS.lambda = sum(lm(Y.hat~Y.trans)$residuals^2)
  RSS.lambda
}


optim(1,power.trans)
lambda = optim(1,power.trans)$par
lambda



# checking transformation

Y.trans = (Y^lambda - 1)/(lambda)
Y.hat = lm(YX)$fit	
plot(Y.trans,Y.hat)

model3 = lm(Y.trans~x1+x2+x3+x4+x5+x6+x7)
#par(mfrow=c(2,2))
#plot(model3)
AIC(model3)
coef(model3)
anova(model3)

#put transformed model into step function
  #to remove any variables that arent significant
model_new = step(model3, direction = "backward")
AIC(model_new)
coef(model_new)
anova(model_new)
par(mfrow=c(2,2))
plot(model_new)


#PART A PREDICTION
predict_price_a <- function(hybrid = 0,
                          engineSize = 2.9,
                          cylinders = 6,
                          horsepower = 200,
                          highwayMPG = 29,
                          weight = 3300,
                          wheelBase = 107) {
  # Create a new data frame for prediction
  new_car <- data.frame(
    x1 = hybrid,
    x2 = engineSize,
    x3 = cylinders,
    x4 = horsepower,
    x5 = highwayMPG,
    x6 = weight,
    x7 = wheelBase
  )
  
  # Predict on the transformed scale (same as model3’s Y)
  predicted_trans <- predict(model_new, newdata = new_car)
  
  # Back-transform using your power transformation lambda
  if (lambda != 0) {
    predicted_price <- ((predicted_trans * lambda) + 1)^(1 / lambda)
  } else {
    predicted_price <- exp(predicted_trans)  # special case if λ ≈ 0
  }
  
  # Print a clean output meow
  cat("Estimated Suggested Retail Price: $", round(predicted_price, 2), "\n")
}


results <- data.frame(
  model = c("Honda Civic", "Toyota Camry", "BMW M340i", "Toyota Supra 3.0"),
  actual = c(25000, 29000, 57000, 58000),
  predicted = c(
    predict_price_a(0, 2.0, 4, 158, 38, 2900, 106),
    predict_price_a(0, 2.5, 4, 203, 33, 3300, 111),
    predict_price_a(0, 3.0, 6, 382, 30, 3820, 112),
    predict_price_a(0, 3.0, 6, 382, 29, 3389, 97.2)
  )
)

results$diff <- results$actual - results$predicted
results




  
#part b stuff
new_car <- data.frame(
  x2 = 3.6,
  x3 = 6,
  x4 = 225,
  x5 = 24,
  x6 = 3237
)

#bootstrap for normality

car.m3 <- data.frame(
  x1 = x1,
  x2 = x2,
  x3 = x3,
  x4 = x4,
  x5 = x5,
  x6 = x6,
  x7 = x7
)

#Step 2, BS sample x and residuals (BSx, BSres)
set.seed(123)  # for reproducibility
n <- nrow(car.m3)
B <- 10000  # number of bootstrap iterations

# Store bootstrap predictions
BS.ystar <- numeric(B)

# Original model residuals on transformed scale
orig.res <- residuals(model_new)

for(i in 1:B){ 
  # Step 1: Resample x and residuals 
  BS.index=sample(1:n,n,replace =TRUE) 
  BB.x=car.m3[BS.index,] 
  BS.res <- sample(orig.res, n, replace = TRUE) 
  # Step 2: Predicted gy for bootstrap x 
  BS.yhat <- predict(model_new, newdata = data.frame(x = BS.x)) 
  # Step 3: Generate BS transformed y
  BS.y <- BS.yhat + BS.res
  
  BS.data = Data1
  BS.data[,3] = BS.y
  BS.data[,c(2,4,5,6,7,8,9)] = BS.x
  Y.trans = BS.y
  BS_new_car <- data.frame(
    x2 = x2,
    x3 = x3,
    x4 = x4,
    x5 = x5,
    x6 = x6,
    Y.trans = Y.trans)
  
  # Step 4: Fit bootstrap model
  BS.model3 = lm(Y.trans~x2+x3+x4+x5+x6,data=BS_new_car)
  
  # Step 5: Predict transformed y for 80th percentile
  BS.ystar[i] <- predict(BS.model3, newdata = new_car) + sample(orig.res, 1)
  
}

pred_trans <- quantile(BS.ystar, 0.80)
  pred_price <- ((pred_trans * lambda) + 1)^(1 / lambda)

hist(BS.ystar, breaks = 50, main="Bootstrap Distribution of Prices", xlab="Transformed Price")
  
  
# Display result
cat(.80*100, "th percentile Suggested Retail Price: $", round(pred_trans, 2), "\n")

# Histogram of bootstrap predictions
hist(BS.ystar,
     breaks = 50,
     main = "Bootstrap Distribution of Predicted Cadillac Prices",
     xlab = "Transformed Price",
     col = "lightblue")

# Add a vertical line for the 80th percentile
abline(v = pred_trans, col = "red", lwd = 2, lty = 2)
