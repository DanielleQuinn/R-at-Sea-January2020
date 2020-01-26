# ---- Mixed Effect Models ----

# ---- Load Packages ----
library(nlme)
library(dplyr)
library(ggplot2)

# ---- Import Data ----


# ---- Explore Data ----


# ---- Identify Research Question ----
# How does tree_age influence the richness of
# orchids?

# ---- Visualize Data ----


# ---- Grouped Data ----
# We have data from multiple sites


# ---- Gaussian GLM ----
# Fixed Effect Model
# an unknown constant that is estimated from the data


# Coefficients


# Requires sufficient data from each group
# Parameters are expensive!

# ---- Contextualizing the Research Question ----
# What are we *actually* trying to do?
# Is our question about comparing specific sites?

# We're concerned with the wider population and 
# trends, not site-specific differences

# ---- Random Intercept Model ----
# Estimates a distribution of the random
# effect of site on the intercept coefficient

# "The relationship (slope) will be the same, but
# the magnitude of the response variable (intercept)
# may differ"

# Model



# Coefficients
     # fixed & random effects
     # random effect

# Fixed (population level) predictions


# Visualize population level model


# Random effect predictions


# Visualize random effects level model


# ---- Comparing GLM to MEM ----
# Did using a random effect model "help"?

# When comparing models, you need to be aware of 
# the underlying methods being used to fit the 
# models and whether these methods generate models 
# that can be directly compared. While models can 
# be compared that don't use the same fit methods, 
# they need to have underlying similarities in their 
# formulation.

# E.g. Was it worth moving from the GLM to
# a random intercept model?

gaussian_glm # fit using Iteratively Reweighted Least Squares

random_int # fit using Restricted Maximum Likelihood

# These can't be compared directly!
# Instead, you need to refit the glm using
# generalized least square (GLS) using the gls() function



# The models must also have the *same* fixed effects


# Apply a likelihood ratio test


# ---- Random Intercept and Slope Model ----
# What if we wanted to look at how not only 
# intercept but also slope is effected by the 
# random influence of site?

# GLM: add an interatction
# Mixed Effect: Random intercept and slope model

# Model
# to include a link between this random effect and 
# our predictor variable, tree_age, we add tree_age
# to the left hand side of the vertical bar


# Fixed (population level) predictions


# Visualize population level model


# Random effect level predictions


# Visualize random effect model


# ---- Correlation Parameter ----
# correlation between the random intercepts and the 
# random slopes


# the correlation between out intercepts is 0.485

# ---- Variance ----
# calculate the variances of the random intercepts
# and slopes by squaring the standard deviations of
# the corresponding parameters

# Random intercept model

   # intercept

# Random intercept and slope model

   # intercept
   # slope

# ---- Model Selection ----
# Compare


# what happens to degrees of freedom? (# of parameters)
# what happens to AIC?
# what is the p-value?
