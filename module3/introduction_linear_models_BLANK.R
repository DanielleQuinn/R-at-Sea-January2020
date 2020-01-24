# ---- Introduction to Linear Regression ----

# ---- Load Packages ----

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

# Goal: Predict the price of a house based on
# lot size, number of bedrooms, etc

# ---- Naive Model ----
# What if we have no information other than price?

# Model

# Coefficients


# Residuals
# How far off is the prediction from the actual price of each house?

# There are several other ways to find the residuals:
# Use the function resid() to extract residuals from naive_model

# Extract the residuals that are stored in the object called naive_model


# Model fit: loss function


# ---- Univariate Model ----
# Can we use lot size as a predictor of price?

# Model


# Coefficients

# The predicted price of a house is $34,136.19 plus 
# $6.60 per 1 unit of lot size
# price = 34136.19 + (6.60 * lotsize)

# Predictions
## Extract predictions from the model object


## Generate predictions using predict()


## $fitted.values is used when you are generating predictions
## on the same data frame as the one used to build the model

## predict() is used when you are generating predictions
## on the same data OR on different data than what was used to build the model

## Add these predictions as a new column in the data


# Visualize the model

  # Raw data

  # Model 1 predictions

  # Naive model predictions



# Model fit: loss function


# Model fit: adjusted r squared value

# The model explains 29 % of the variability of the
# response variable (price) around its mean

# Model diagnostics: {ggplot2}




# Model diagnostics: plot()


# Our model is better at making predictions
# at low values of x; "heterogeneity" in our residuals
# The model should perform equally well across values of x

# ---- Comparing Models ----
# Can we use lot size as a predictor of price?


# Can we use the number of bedrooms as a predictor of price?

# Model


# Model fit: loss function



# Model fit: adjusted r squared

# ---- Multivariate Model ----
# Can we use lot size and number of stories as predictors of price?

# Model


# Coefficients

# The predicted price of a house is $15,075.57 plus 
# $6.21 per 1 unit of lot size plus
# $11,656.97 per story

# price = 15075.57 + (6.21 * lotsize) + (11656.97 * stories)

# How much do we predict a two story house
# with a lot size of 5500 will sell for?


# Predictions


# Confidence intervals


# Visualize the model

  # Raw data

  # Naive model predictions

  # Model 1 predictions (lot size)

  # Model 3 predictions (lot size + stories)

  # Need to facet it to see the differences between stories


# Model fit: loss function


# Comparing models
## AIC (Akaike Information Criterion): used to compare nested models
# Nested models: The parameters in Model A are a subset
# of the parameters in Model B and use the same data

model3$terms[[3]]
model2$terms[[3]] # not nested


# AIC


# ---- Categorical Predictors ----
# Model


# Coefficients


# Predictions
## What is the predicted price of a house with a 
## lot size of 3400 with no driveway?
# price = 26750.94 + (6.05 * lotsize) + (11911.57 * driveway)


## What is the predicted price of a house with a 
## lot size of 3400 with a driveway?


# ---- Interactions ----
# Model


# Coefficients


## lot size of 5000
## no driveway


## What is the predicted price of a house with a 
## lot size of 3400 with no driveway?
# price = 38731.07 + (2.72 * lotsize) - (922.56 * driveway) + (3.48 * lotsize * driveway)


## What is the predicted price of a house with a 
## lot size of 3400 with no driveway?


# Here, a driveway reduces the predicted price by $922.56
# but increases it by $3.48 per unit lot size

# ---- Full Model ----
# Rather than adding one piece at a time, let's
# start with all of the variables in the model

# Clean data


# Model


# Model formula


# Predictors


# Coefficients


# Confidence intervals


# Generate predictions


# Model fit: loss function


# Compare models




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


# ---- Updating a Model ----


# Remove bedrooms from the model


# Model fit: loss function


# AIC

