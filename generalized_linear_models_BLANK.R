# ---- Generalized Linear Models in R ----

# ---- Load Packages ----
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Source Functions ----

# ---- Import Data ----

# ---- Explore Data ----

# ---- Before You Start Modelling ----
# 1. Identify the research question
## How does stream velocity influence the 
## number of dragonflies present?

# 2. Identify the response variable
## What are we trying to explain? (y-axis)
## abundance

# 3. Identify a possible predictor variable
## What do we think will influence our response variable? (x-axis)
## stream_flow

# 4. What do the raw data look like?



# ---- Naive Model ----
# Model


# Predictions


# Model fit: loss function


# ---- Univariate Model ----
# Can we use stream flow as a predictor of abundance?
# Model


# Coefficients

# The predicted abundance of dragonflies is 81.56 plus 
# -16.44 per 1 unit of stream flow

# Predictions


# Visualize the model


# Model fit: loss function


# Model fit: adjusted r squared

# The model explains ___ % of the variability of the
# response variable (price) around its mean

# Model diagnostics



# What does this tell us about our model?
# What other issues does this model have?
# Should we use this model to answer the research question?

# Conclusions:
# lower loss function
# heterogeneity in the residuals
# low r squared value
# poor fitted line, and 
# unreasonable potential predictions

# ---- Generalized Linear Models (GLM) ----
# flexible generalization
# allows for non normal error distribution
# various families; each uses a different link function

# ---- Poisson GLM ----
# often applied to count data that is non normal


# uses the log link function
# linear (gaussian)
# y = intercept + (slope * x)

# possion
# log(y) = intercept + (slope * x) 
# y = e ^ (intercept + (slope * x))

# Model



# Coefficients


# Predictions
# What is the predicted abundance when stream_flow is 2?
# log(abundance) = intercept + (slope * stream_flow)


# Transform using the link function
# abundance = e^(intercept + (slope * stream_flow))


# Predict function
# Non-transformed values


# Transformed values



# Add transformed predictions to data frame


# Visualize the model


# Model diagnostics



# Model diagnostics: overdispersion
## the ratio between residual deviance and 
## the degrees of freedom should be 1


# This means that there is more variability in the 
# data then what would be expected by the
# model or the error structure

# Visualize overdispersion
data.frame(stream_flow = seq(from = 0.5, to = 5.5, length = 25)) %>%
  mutate(temp = 2) %>%
  group_by(stream_flow) %>%
  tidyr::expand(temp = seq(1:temp)) %>%
  dplyr::select(-temp) %>%
  mutate(estimates = rpois(n = n(), lambda = exp(coef(model2)[1] + (stream_flow * coef(model2)[2])))) %>%
  ggplot() +
  geom_point(aes(x = stream_flow, y = abundance), data = dragonflies) +
  geom_line(aes(x = stream_flow, y = predicted_m2), col = "blue", data = dragonflies) +
  geom_point(aes(x = stream_flow, y = estimates), col = 'red', alpha = 0.5) +
  theme_bw()

# We see that the potential estimates based on this model do not
# adequately represent our real data; our real data are overdispersed

# Conclusions:
# heterogeneity in the residuals
# overdispersion

# ---- Additional Factors ----
# It's possible that the issues we're seeing
# in our models are a result of a factor that is
# influencing the data but is not being included
# in the model.

# How might season influence the data?



# We want to see residuals associated with each time
# are normally distributed about 0.
# This plot shows us that this isn't true for
# residuals associated with night.
# This suggets that we should include time as
# a factor in subsequent models.

# ---- Adding a Factor and Interaction ----
# Model


# Predictions


# Visualize the model


# Model diagnostics



# Model diagnostics


# Conclusions:
# overdispersion
# minor heterogeniety in residuals

# ---- Adding an Offset ----
# Review the data set


# The area sampled at each sampling event is not equal!
# Options:
# (1) Convert abundance to density
# (2) Add an offset


# Standarize area


# Model


# Model diagnostics


# ---- Negative Binomial GLMs ----
# Extra parameter theta relaxes the assumptions of
# equality between the mean and the variance
# Helps greatly with overdispersion
# Uses the same link function as Poisson

# Model


# Theta


# Dispersion


# ---- Assessing Terms ----
# Assess the significance of each term


# Update model


# Assess


# Try same model, without offset
# important: update() will not do this!


# ---- Model Selection ----
# Consider:
# Common Sense / Biological Knowledge
# Model Diagnostics
# Overall Patterns

# Compare: AIC


# Why is this happening?
# Visualize time and larea


# Compare: Residuals
model5_diag <- data.frame(fitted = fitted(model5),
                          residuals = resid(model5),
                          model = "Model5")

model6_diag <- data.frame(fitted = fitted(model6),
                          residuals = resid(model6),
                          model = "Model6")

model7_diag <- data.frame(fitted = fitted(model7),
                          residuals = resid(model7),
                          model = "Model7")

multi_diag <- bind_rows(model5_diag,
                        model6_diag,
                        model7_diag)



# Compare: dispersion

# Compare: theta

