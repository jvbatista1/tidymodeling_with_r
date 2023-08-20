##### 3.1 AN EXAMPLE #####
library(tidyverse)

data(crickets, package = "modeldata")
names(crickets)

# Plot the temperature on the x-axis, the chirp rate on the y-axis. The plot
# elements will be colored differently for each species:
ggplot(crickets, 
       aes(x = temp, y = rate, color = species, pch = species, lty = species)) + 
  # Plot points for each data point and color by species
  geom_point(size = 2) + 
  # Show a simple linear model fit created separately for each species:
  geom_smooth(method = lm, se = FALSE, alpha = 0.5) + 
  scale_color_brewer(palette = "Paired") +
  labs(x = "Temperature (C)", y = "Chirp Rate (per minute)")

rate ~ temp
rate ~ temp + time
rate ~ temp + species

rate ~ temp + species + temp:species

# A shortcut can be used to expand all interactions containing
# interactions with two variables:
rate ~ (temp + species)^2

# Another shortcut to expand factors to include all possible
# interactions (equivalent for this example):
rate ~ temp * species

interaction_fit <-  lm(rate ~ (temp + species)^2, data = crickets) 

# To print a short summary of the model:
interaction_fit

# Place two plots next to one another:
par(mfrow = c(1, 2))

# Show residuals vs predicted values:
plot(interaction_fit, which = 1)

# A normal quantile plot on the residuals:
plot(interaction_fit, which = 2)

# Fit a reduced model:
main_effect_fit <-  lm(rate ~ temp + species, data = crickets) 

# Compare the two:
anova(main_effect_fit, interaction_fit)

summary(main_effect_fit)

new_values <- data.frame(species = "O. exclamationis", temp = 15:20)
predict(main_effect_fit, new_values)

##### 3.2 WHAT DOES THE R FORMULA DO? #####

##### 3.3 WHY TIDINESS IS IMPORTANT FOR MODELING #####

plot(plot_data$x, plot_data$y)

library(lattice)
xyplot(y ~ x, data = plot_data)

library(ggplot2)
ggplot(plot_data, aes(x = x, y = y)) + geom_point()

# Add a missing value to the prediction set
new_values$temp[1] <- NA
# The predict method for `lm` defaults to `na.pass`:
predict(main_effect_fit, new_values)
# Alternatively 
predict(main_effect_fit, new_values, na.action = na.fail)
predict(main_effect_fit, new_values, na.action = na.omit)

corr_res <- map(mtcars %>% select(-mpg), cor.test, y = mtcars$mpg)

# The first of ten results in the vector: 
corr_res[[1]]

library(broom)

tidy(corr_res[[1]])

corr_res %>% 
  # Convert each to a tidy format; `map_dfr()` stacks the data frames 
  map_dfr(tidy, .id = "predictor") %>% 
  ggplot(aes(x = fct_reorder(predictor, estimate))) + 
  geom_point(aes(y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  labs(x = NULL, y = "Correlation with mpg")

##### 3.4 COMBINING BASE R MODELS AND THE TIDYVERSE #####
split_by_species <- 
  crickets %>% 
  group_nest(species) 
split_by_species

model_by_species <- 
  split_by_species %>% 
  mutate(model = map(data, ~ lm(rate ~ temp, data = .x)))
model_by_species

model_by_species %>% 
  mutate(coef = map(model, tidy)) %>% 
  select(species, coef) %>% 
  unnest(cols = c(coef))

##### 3.5 THE TIDYMODELS METAPACKAGE #####
library(tidymodels)

library(conflicted)
conflict_prefer("filter", winner = "dplyr")

tidymodels_prefer(quiet = FALSE)
