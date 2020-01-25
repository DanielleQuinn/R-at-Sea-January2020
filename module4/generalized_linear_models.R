# ---- Generalized Linear Models in R ----

# ---- Load Packages ----
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Source Functions ----
source("more_functions.R")

# ---- Import Data ----
dragonflies <- read.csv("new_dragonflies.csv")

# ---- Explore Data ----
head(dragonflies)
summary(dragonflies)
glimpse(dragonflies)

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
ggplot(dragonflies) +
  geom_histogram(aes(x = abundance)) +
  theme_bw()

ggplot(dragonflies) +
  geom_point(aes(x = stream_flow, y = abundance)) +
  theme_bw()

# ---- Naive Model ----
# Model
naive_model <- lm(abundance ~ 1, data = dragonflies)
naive_model

# Predictions
predict(naive_model, dragonflies)
mean(dragonflies$abundance)

# Model fit: loss function
naive_ssr <- sum(resid(naive_model)^2)
naive_ssr

# ---- Univariate Model ----
# Can we use stream flow as a predictor of abundance?
# Model
model1 <- lm(abundance ~ stream_flow, data = dragonflies)
summary(model1)

# Coefficients
model1$coefficients
# The predicted abundance of dragonflies is 81.56 plus 
# -16.44 per 1 unit of stream flow

# Predictions
dragonflies <- dragonflies %>% 
  mutate(predicted_m1 = predict(model1, dragonflies))

# Visualize the model
ggplot(dragonflies) +
  # Data points
  geom_point(aes(x = stream_flow, y = abundance)) +
  # Naive model predictions
  geom_hline(yintercept = mean(dragonflies$abundance), col = "chocolate", size = 1.5) +
  # Model 1 predictions
  geom_line(aes(x = stream_flow, y = predicted_m1), col = "blue", size = 1.5) +
  theme_bw()

# Model fit: loss function
model1_ssr <- sum(resid(model1)^2)
model1_ssr # This is an improvement

# Model fit: adjusted r squared
summary(model1)$adj.r.squared
# The model explains 29.5 % of the variability of the
# response variable (price) around its mean

# Model diagnostics
model1_diag <- data.frame(fitted = fitted(model1),
                          residuals = resid(model1))

ggplot(model1_diag) + 
  geom_point(aes(x = fitted, y = residuals)) + 
  geom_hline(yintercept = 0,
             col = "red", linetype = "dashed") + 
  theme_bw()

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
ggplot(dragonflies) +
  geom_histogram(aes(x = abundance)) +
  theme_bw()

# uses the log link function
# linear (gaussian)
# y = intercept + (slope * x)

# possion
# log(y) = intercept + (slope * x) 
# y = e ^ (intercept + (slope * x))

# Model
model2 <- glm(abundance ~ stream_flow, data = dragonflies,
                   family = "poisson")
model2
summary(model2)

# Coefficients
model2$coefficients

# Predictions
# What is the predicted abundance when stream_flow is 2?
# log(abundance) = intercept + (slope * stream_flow)
4.8 + (-0.523 * 2)

# Transform using the link function
# abundance = e^(intercept + (slope * stream_flow))
exp(4.8 + (-0.523 * 2))

# Predict function
# Non-transformed values
predict(model2, dragonflies)

# Transformed values
predict(model2, dragonflies,
        type = "response")

# Add transformed predictions to data frame
dragonflies <- dragonflies %>%
  mutate(predicted_m2 = predict(model2, dragonflies, type = "response"))

# Visualize the model
ggplot(dragonflies) +
  # Data points
  geom_point(aes(x = stream_flow, y = abundance)) +
  # Model 2 predictions
  geom_line(aes(x = stream_flow, y = predicted_m2), col = "blue", size = 1.5) +
  theme_bw()

# Model diagnostics
model2_diag <- data.frame(fitted = fitted(model2),
                          residuals = resid(model2))

ggplot(model2_diag) +
  geom_point(aes(x = fitted, y = residuals)) +
  geom_hline(yintercept = 0,
             col = "red", linetype = "dashed") +
  theme_bw()

# Model diagnostics: overdispersion
## the ratio between residual deviance and 
## the degrees of freedom should be 1
dispersion(model2, modeltype = "p")

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
ggplot(dragonflies) +
  geom_boxplot(aes(x = time, y = abundance - predicted_m2)) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  theme_bw()

# We want to see residuals associated with each time
# are normally distributed about 0.
# This plot shows us that this isn't true for
# residuals associated with night.
# This suggets that we should include time as
# a factor in subsequent models.

# ---- Adding a Factor and Interaction ----
# Model
model3 <- glm(abundance ~ stream_flow * time,
              data = dragonflies,
              family = "poisson")

summary(model3)

# Predictions
dragonflies <- dragonflies %>%
  mutate(predicted_m3 = predict(model3, dragonflies, type = "response"))

# Visualize the model
ggplot(dragonflies) +
  # Data points
  geom_point(aes(x = stream_flow, y = abundance)) +
  # Model 3 predictions
  geom_line(aes(x = stream_flow, y = predicted_m3, col = time), size = 1.5) +
  theme_bw()

# Model diagnostics
model3_diag <- data.frame(fitted = fitted(model3),
                          residuals = resid(model3))

ggplot(model3_diag) +
  geom_point(aes(x = fitted, y = residuals)) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  theme_bw()

# Model diagnostics
dispersion(model3, modeltype = "p")

# Conclusions:
# overdispersion
# minor heterogeniety in residuals

# ---- Adding an Offset ----
# Review the data set
head(dragonflies, n = 2)

# The area sampled at each sampling event is not equal!
# Options:
# (1) Convert abundance to density
# (2) Add an offset

ggplot(dragonflies) +
  geom_point(aes(x = area, y = abundance)) +
  theme_bw()

# Standarize area
dragonflies <- dragonflies %>%
  mutate(larea = log(area))

# Model
model4 <- glm(abundance ~ stream_flow * time + offset(larea),
              data = dragonflies,
              family = "poisson")
summary(model4)

# Model diagnostics
dispersion(model4, modeltype = "p")

# ---- Negative Binomial GLMs ----
# Extra parameter theta relaxes the assumptions of
# equality between the mean and the variance
# Helps greatly with overdispersion
# Uses the same link function as Poisson

# Model
model5 <- MASS::glm.nb(abundance ~ stream_flow * time + offset(larea),
                 data = dragonflies)

summary(model5)

# Theta
model5$theta

# Dispersion
dispersion(model5, modeltype = "nb")

# ---- Assessing Terms ----
# Assess the significance of each term
drop1(model5, test = "Chisq")

# Update model
model6 <- update(model5, . ~ . - stream_flow:time)
summary(model6)

# Assess
drop1(model6, test = "Chisq")

# Try same model, without offset
# important: update() will not do this!
model6$terms[[3]]
model7 <- glm.nb(abundance ~ stream_flow + time, 
                 data = dragonflies)
summary(model7)

# ---- Model Selection ----
# Consider:
# Common Sense / Biological Knowledge
# Model Diagnostics
# Overall Patterns

# Compare: AIC
AIC(model5, model6, model7)

# Why is this happening?
# Visualize time and larea
ggplot(dragonflies) +
  geom_boxplot(aes(x = time, y = larea))

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

ggplot(multi_diag) +
  geom_point(aes(x = fitted, y = residuals)) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  facet_wrap(~model, ncol = 1) +
  theme_bw()

# Compare: dispersion
dispersion(model5, modeltype = "nb")
dispersion(model6, modeltype = "nb")
dispersion(model7, modeltype = "nb")

# Compare: theta
model5$theta
model6$theta
model7$theta

# ---- Visualize Model7 ----
summary(model7)

dragonflies <- dragonflies %>%
  mutate(predicted_m7 = predict(model7, dragonflies, type = "response"),
         error = predict(model7, dragonflies, se = TRUE, type = "response")$se)

ggplot(dragonflies) +
  geom_point(aes(x = stream_flow, y = abundance)) + 
  geom_line(aes(x = stream_flow, y = predicted_m7, col = time)) +
  geom_line(aes(x = stream_flow, y = predicted_m7 - error, col = time),
            linetype = "dashed") +
  geom_line(aes(x = stream_flow, y = predicted_m7 + error,  col = time),
                linetype = "dashed") +
  theme_bw()
