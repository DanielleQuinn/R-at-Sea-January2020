# ---- Introduction to Linear Regression ----

# ---- Load Packages ----
library(Ecdat)
library(ggplot2)
library(dplyr)

# ---- A "Perfect" Example ----
# We'll start by generating some data that a linear model will fit well

# Generate Data
## Predictor: 1000 values from 0 to 10, uniformly distributed
predictor <- runif(1000, 0, 10)

## Reponse: a function of the predictor, plus some error that follows a normal distribution
## y = mx + b + e
response <- 7 * predictor + 1 + rnorm(1000, 0, 1)

## Store in data frame
demo_data <- data.frame(response, predictor)
head(demo_data)

# Visualize raw data
ggplot(demo_data) +
  geom_point(aes(x = predictor, y = response)) +
  theme_bw(16)

# Model
demo_model <- lm(response ~ predictor, data = demo_data)

# Coefficients
demo_model
demo_model$coefficients
coef(demo_model)

# The value of y is predicted to be 1.03 plus
# 6.99 per unit increase in x
# y = 1.03 + (6.99 * x)

# Predictions
demo_data <- demo_data %>%
  mutate(predicted = predict(demo_model, demo_data))

# Visualize model
ggplot(demo_data) +
  geom_point(aes(x = predictor, y = response)) +
  geom_line(aes(x = predictor, y = predicted),
            col = "red", size = 1.5) +
  theme_bw(16)

# Model fit: loss function (sum of square residuals)
demo_ssr <- sum(resid(demo_model)^2)

# Model fit: adjusted r square value
summary(demo_model)
summary(demo_model)$adj.r.squared

# Model diagnostics: residuals vs fitted values
demo_diag <- data.frame(fitted = fitted(demo_model),
                        residuals = resid(demo_model))
ggplot(demo_diag) +
  geom_point(aes(x = fitted, y = residuals)) +
  geom_hline(yintercept = 0,
             col = "red", size = 2, linetype = "dashed") +
  theme_bw(16)

# ---- Data: Housing ----
# Access (built-in data set in {Ecdat} package)
Housing <- Ecdat::Housing

# Goal: Predict the price of a house based on
# lot size, number of bedrooms, etc

# ---- Naive Model ----
# What if we have no information other than price?

# Model
naive_model <- lm(price ~ 1, Housing)
summary(naive_model)

# Coefficients
naive_model$coefficients
# With no other information, we would predict that a
# house will sell for $68,121.60
mean(Housing$price)

# Residuals
# How far off is the prediction from the actual price of each house?
Housing$price - 68121.6

# There are several other ways to find the residuals:
# Use the function resid() to extract residuals from naive_model
resid(naive_model)
# Extract the residuals that are stored in the object called naive_model
naive_model$residuals

# Model fit: loss function
naive_ssr <- sum(naive_model$residuals^2)
naive_ssr

# ---- Univariate Model ----
# Can we use lot size as a predictor of price?

# Model
model1 <- lm(price ~ lotsize, data = Housing)
summary(model1)

# Coefficients
model1$coefficients
# The predicted price of a house is $34,136.19 plus 
# $6.60 per 1 unit of lot size
# price = 34136.19 + (6.60 * lotsize)

# Predictions
## Extract predictions from the model object
model1$fitted.values

## Generate predictions using predict()
predict(model1, Housing)

## $fitted.values is used when you are generating predictions
## on the same data frame as the one used to build the model

## predict() is used when you are generating predictions
## on the same data OR on different data than what was used to build the model

## Add these predictions as a new column in the data
Housing <- Housing %>% 
  mutate(predicted_m1 = predict(model1, Housing))

# Visualize the model
ggplot(Housing) +
  # Raw data
  geom_point(aes(x = lotsize, y = price)) +
  # Model 1 predictions
  geom_line(aes(x = lotsize, y = predicted_m1), col = "red", size = 2) +
  # Naive model predictions
  geom_hline(yintercept = mean(Housing$price), col = "blue", size = 2) +
  theme_bw(16)

# Model fit: loss function
model1_ssr <- sum((Housing$price - Housing$predicted_m1)^2)
model1_ssr # This is an improvement

# Model fit: adjusted r squared value
summary(model1)$adj.r.squared
# The model explains 29 % of the variability of the
# response variable (price) around its mean

# Model diagnostics: {ggplot2}
model1_diag <- data.frame(fitted = fitted(model1),
                          residuals = resid(model1))

ggplot(model1_diag) +
  geom_point(aes(x = fitted, y = residuals)) +
  geom_hline(yintercept = 1,
             col = "red", size = 2, linetype = "dashed") +
  theme_bw(16)

# Model diagnostics: plot()
plot(model1)

# Our model is better at making predictions
# at low values of x; "heterogeneity" in our residuals
# The model should perform equally well across values of x

# ---- Comparing Models ----
# Can we use lot size as a predictor of price?
model1 <- lm(price ~ lotsize, data = Housing) # above

# Can we use the number of bedrooms as a predictor of price?

# Model
model2 <- lm(price ~ bedrooms, data = Housing)

# Model fit: loss function
model1_ssr
model2_ssr <- sum((model2$residuals)^2)
model2_ssr # Does not improve the model

# Model fit: adjusted r squared
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared

# ---- Multivariate Model ----
# Can we use lot size and number of stories as predictors of price?

# Model
model3 <- lm(price ~ lotsize + stories, data = Housing)
summary(model3)

# Coefficients
model3$coefficients
# The predicted price of a house is $15,075.57 plus 
# $6.21 per 1 unit of lot size plus
# $11,656.97 per story

# price = 15075.57 + (6.21 * lotsize) + (11656.97 * stories)

# How much do we predict a two story house
# with a lot size of 5500 will sell for?
15075.57 + (6.21 * 5500) + (11656.97 * 2) 

# Predictions
Housing <- Housing %>% 
  mutate(predicted_m3 = predict(model3, Housing))

# Confidence intervals
model3$coefficients
confint(model3)

# Visualize the model
ggplot(Housing) +
  # Raw data
  geom_point(aes(x = lotsize, y = price)) +
  # Naive model predictions
  geom_hline(yintercept = mean(Housing$price), col = "blue", size = 2) +
  # Model 1 predictions (lot size)
  geom_line(aes(x = lotsize, y = predicted_m1), col = "red", size = 2) +
  # Model 3 predictions (lot size + stories)
  geom_line(aes(x = lotsize, y = predicted_m3), col = "green", size = 2) +
  # Need to facet it to see the differences between stories
  facet_wrap(~stories) +
  theme_bw(16)

# Model fit: loss function
model3_ssr <- sum((model3$residuals)^2)
model1_ssr
model3_ssr # An improvement

# Comparing models
## AIC (Akaike Information Criterion): used to compare nested models
# Nested models: The parameters in Model A are a subset
# of the parameters in Model B and use the same data

model3$terms[[3]]
model2$terms[[3]] # not nested

model3$terms[[3]]
model1$terms[[3]] # nested

# AIC
AIC(model3, model1)

# ---- Categorical Predictors ----
# Model
model4 <- lm(price ~ lotsize + driveway, data = Housing)
summary(model4)

# Coefficients
model4$coefficients

# Predictions
## What is the predicted price of a house with a 
## lot size of 3400 with no driveway?
# price = 26750.94 + (6.05 * lotsize) + (11911.57 * driveway)
26750.94 + (6.05 * 3400) + (11911.57 * 0)

## What is the predicted price of a house with a 
## lot size of 3400 with a driveway?
26750.94 + (6.05 * 3400) + (11911.57 * 1)

# ---- Interactions ----
# Model
model5 <- lm(price ~ lotsize * driveway, data = Housing)
summary(model5)

# Coefficients
model5$coefficients

## lot size of 5000
## no driveway
38731.067485 + (5000*2.724212)

## What is the predicted price of a house with a 
## lot size of 3400 with no driveway?
# price = 38731.07 + (2.72 * lotsize) - (922.56 * driveway) + (3.48 * lotsize * driveway)
38731.07 + (2.72 * 3400) - (922.56 * 0) + (3.48 * 3400 * 0)

## What is the predicted price of a house with a 
## lot size of 3400 with no driveway?
38731.07 + (2.72 * 3400) - (922.56 * 1) + (3.48 * 3400 * 1)

# Here, a driveway reduces the predicted price by $922.56
# but increases it by $3.48 per unit lot size

# ---- Full Model ----
# Rather than adding one piece at a time, let's
# start with all of the variables in the model

# Clean data
names(Housing)
Housing <- Housing %>%
  select(-starts_with("predicted"))

# Model
full_model <- lm(price ~ ., data = Housing)
summary(full_model)

# Model formula
full_model$call

# Predictors
full_model$terms[[3]]

# Coefficients
full_model$coefficients

# Confidence intervals
confint(full_model)

# Generate predictions
Housing <- Housing %>%
  mutate(predicted_fm = predict(full_model, Housing))

# Model fit: loss function
fullmodel_ssr <- sum((full_model$residuals)^2)
model3_ssr
fullmodel_ssr # An improvement

# Compare models
model3$terms[[3]]
full_model$terms[[3]] # nested

AIC(model3, full_model) # The full model is better

# Predictions
## How much would a house cost with:
### lot size of 5500
### 3 bedrooms
### 2 bathrooms
### 2 stories
### a driveway
### a full basement
### a 1 car garage
myHouse <- data.frame(lotsize = 5500,
                      bedrooms = 3,
                      bathrms = 2,
                      stories = 2,
                      driveway = "yes",
                      recroom = "no",
                      fullbase = "yes",
                      gashw = "no",
                      airco = "no",
                      garagepl = 1,
                      prefarea = "no")
myHouse
predict(full_model, myHouse)

# ---- Updating a Model ----
summary(full_model)

# Remove bedrooms from the model
smaller_model <- update(full_model, . ~ . - bedrooms)
summary(smaller_model)

# Model fit: loss function
fullmodel_ssr
smallermodel_ssr <- sum(smaller_model$residuals^2)
smallermodel_ssr

# AIC
AIC(full_model, smaller_model)