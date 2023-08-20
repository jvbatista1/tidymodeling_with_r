library(modeldata) # This is also loaded by the tidymodels package
data(ames)

# or, in one line:
#data(ames, package = "modeldata")

dim(ames)

##### 4.1 EXPLORING FEATURES OF HOMES IN AMES #####
library(tidymodels)
tidymodels_prefer()

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white")

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white") +
  scale_x_log10()

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

##### 5. SPENDING OUR DATA ######
##### 5.1 COMMON METHODS FOR SPLITTING DATA ######
# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(501)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80)
ames_split

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

##### 6. FITTING MODELS WITH parsnip ######
##### 6.1 CREATE A MODEL #####
#model <- lm(formula, data, ...)
#model <- stan_glm(formula, data, family = "gaussian", ...)
#model <- glmnet(x = matrix, y = vector, family = "gaussian", ...)

library(tidymodels)
tidymodels_prefer()

linear_reg() %>% set_engine("lm")
linear_reg() %>% set_engine("glmnet")
linear_reg() %>% set_engine("stan")

linear_reg() %>% set_engine("lm") %>% translate()
linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()
linear_reg() %>% set_engine("stan") %>% translate()

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_form_fit
lm_xy_fit

rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()

rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger", verbose = TRUE) %>% 
  set_mode("regression")

##### 6.2 USE THE MODEL RESULTS #####
lm_form_fit %>% extract_fit_engine()
lm_form_fit %>% extract_fit_engine() %>% vcov()

model_res <- 
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)

param_est

tidy(lm_form_fit)

##### 6.3 MAKE PREDICTIONS #####
ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int")) 

tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- 
  tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small))

##### 6.6 CHAPTER SUMMARY #####
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")

##### 7. A MODEL WORKFLOW #####
##### 7.1 Where does the model begin and end? #####
##### 7.2 Workflow basics #####

library(tidymodels)  # Includes the workflows package
tidymodels_prefer()

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow

lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:3))
