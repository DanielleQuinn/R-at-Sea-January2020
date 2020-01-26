# ---- Mixed Effect Models ----

# ---- Load Packages ----
library(nlme)
library(dplyr)
library(ggplot2)

# ---- Import Data ----
orchids <- read.csv("orchids.csv")

# ---- Explore Data ----
dim(orchids)
head(orchids)
glimpse(orchids)

# ---- Identify Research Question ----
# How does tree_age influence the richness of
# orchids?

# ---- Visualize Data ----
ggplot(orchids) +
  geom_jitter(aes(x = tree_age, y = richness)) +
  theme_bw()

# ---- Grouped Data ----
# We have data from multiple sites
ggplot(orchids) +
  geom_jitter(aes(x = tree_age, y = richness,
                 col = site)) +
  theme_bw()

# ---- Gaussian GLM ----
# Fixed Effect Model
# an unknown constant that is estimated from the data
gaussian_glm <- glm(richness ~ tree_age + site,
                    data = orchids,
                    family = "gaussian")
gaussian_glm

# Coefficients
coef(gaussian_glm)

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
random_int <- lme(richness ~ tree_age,
                  random = ~1 | site,
                  data = orchids)
random_int

# Coefficients
random_int$coefficients # fixed & random effects
coef(random_int) # random effect

# Fixed (population level) predictions
orchids <- orchids %>%
  mutate(predicted_ri_pop = predict(random_int,
                                    orchids,
                                    level = 0))

# Visualize population level model
ggplot(orchids) + 
  geom_jitter(aes(x = tree_age, y = richness,
                 col = site)) +
  geom_line(aes(x = tree_age, y = predicted_ri_pop),
            size = 2) +
  theme_bw()

# Random effect predictions
orchids <- orchids %>%
  mutate(predicted_ri_re = predict(random_int,
                                    orchids,
                                    level = 1))

# Visualize random effects level model
ggplot(orchids) + 
  geom_jitter(aes(x = tree_age, y = richness,
                 col = site)) +
  geom_line(aes(x = tree_age, y = predicted_ri_pop),
            size = 2) +
  geom_line(aes(tree_age, y = predicted_ri_re,
                col = site)) +
  theme_bw()

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

gaussian_gls <- gls(richness ~ tree_age,
                    data = orchids)

# The models must also have the *same* fixed effects
gaussian_gls$call
random_int$call

# Apply a likelihood ratio test
anova(gaussian_gls, random_int)

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
random_intslo <- lme(richness ~ tree_age,
                     random = ~1 + tree_age | site,
                     data = orchids)

# Fixed (population level) predictions
orchids <- orchids %>%
  mutate(predicted_risl_pop = predict(random_intslo,
                                      orchids,
                                      level = 0))

# Visualize population level model
ggplot(orchids) +
  geom_jitter(aes(x = tree_age, y = richness,
                 col = site)) +
  geom_line(aes(x = tree_age, y = predicted_risl_pop),
            size = 2) +
  theme_bw()

# Random effect level predictions
orchids <- orchids %>%
  mutate(predicted_risl_re = predict(random_intslo,
                                     orchids,
                                     level = 1))

# Visualize random effect model
ggplot(orchids) +
  geom_jitter(aes(x = tree_age, y = richness,
                 col = site)) +
  geom_line(aes(x = tree_age, y = predicted_risl_pop),
            size = 2) +
  geom_line(aes(x = tree_age, y = predicted_risl_re,
                col = site)) +
  theme_bw()

# ---- Correlation Parameter ----
# correlation between the random intercepts and the 
# random slopes

random_intslo
# the correlation between out intercepts is 0.485

# ---- Variance ----
# calculate the variances of the random intercepts
# and slopes by squaring the standard deviations of
# the corresponding parameters

# Random intercept model
random_int
1.066118^2 # intercept

# Random intercept and slope model
random_intslo
0.74364541^2 # intercept
0.04433737^2 # slope

# ---- Model Selection ----
# Compare
anova(random_int, random_intslo)

# what happens to degrees of freedom? (# of parameters)
# what happens to AIC?
# what is the p-value?