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

lm_fit %>% update_formula(Sale_Price ~ Longitude)

##### 7.3 Adding variables to the workflow() #####
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))
lm_wflow

#predictors = c(ends_with("tude"))
#predictors = everything()

fit(lm_wflow, ames_train)

##### 7.4 How does a workflow() use the formula #####
##### 7.4.1 Special formulas and inline functions #####
library(lme4)
library(nlme)
lmer(distance ~ Sex + (age | Subject), data = Orthodont)

model.matrix(distance ~ Sex + (age | Subject), data = Orthodont)

library(multilevelmod)

multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_workflow <- 
  workflow() %>% 
  # Pass the data along as-is: 
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevel_spec, 
            # This formula is given to the model
            formula = distance ~ Sex + (age | Subject))

multilevel_fit <- fit(multilevel_workflow, data = Orthodont)
multilevel_fit

library(censored)

parametric_spec <- survival_reg()

parametric_workflow <- 
  workflow() %>% 
  add_variables(outcome = c(fustat, futime), predictors = c(age, rx)) %>% 
  add_model(parametric_spec, 
            formula = Surv(futime, fustat) ~ age + strata(rx))

parametric_fit <- fit(parametric_workflow, data = ovarian)
parametric_fit

##### 7.5 Creating multiple workflows at once #####
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

library(workflowsets)
location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models
location_models$info[[1]]
extract_workflow(location_models, id = "coords_lm")

location_models <-
  location_models %>%
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
location_models$fit[[1]]

##### 7.6 Evaluating the test set #####
final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res

fitted_lm_wflow <- extract_workflow(final_lm_res)

collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)

##### 7.7 Chapter summary #####
library(tidymodels)
data(ames)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_fit <- fit(lm_wflow, ames_train)

##### 8. FEATURE ENGINEERING WITH RECIPES #####
##### 8.1 A simple recipe() for the Ames housing data #####
lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type, data = ames)

library(tidymodels) # Includes the recipes package
tidymodels_prefer()

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
simple_ames

##### 8.2 Using recipes ##### 
lm_wflow %>% 
  add_recipe(simple_ames)

lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)

predict(lm_fit, ames_test %>% slice(1:3))

# Get the recipe after it has been estimated:
lm_fit %>% 
  extract_recipe(estimated = TRUE)

# To tidy the model fit: 
lm_fit %>% 
  # This returns the parsnip object:
  extract_fit_parsnip() %>% 
  # Now tidy the linear model object:
  tidy() %>% 
  slice(1:5)

##### 8.3 How data are used by the recipe() #####
##### 8.4 Examples of recipe steps #####
##### 8.4.1 Encoding qualitative data in a numeric format #####
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

##### 8.4.2 Interaction terms #####
ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")

#Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Bldg_Type + 
#  log10(Gr_Liv_Area):Bldg_Type
# or
#Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) * Bldg_Type

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )

##### 8.4.3 Spline functions #####
library(patchwork)
library(splines)

plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = 10^Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      color = "lightblue",
      se = FALSE
    ) +
    labs(title = paste(deg_free, "Spline Terms"),
         y = "Sale Price (USD)")
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )

recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, deg_free = 20)

##### 8.8 Chapter summary #####
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

##### 9. JUDGING MODEL EFECTIVENESS #####
##### 9.1 Performance metrics and inference #####
##### 9.2 Regression metrics #####
ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res

ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

rmse(ames_test_res, truth = Sale_Price, estimate = .pred)

ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)

##### 9.3 Binary classification metrics #####
data(two_class_example)
tibble(two_class_example)

# A confusion matrix: 
conf_mat(two_class_example, truth = truth, estimate = predicted)

# Accuracy:
accuracy(two_class_example, truth, predicted)

# Matthews correlation coefficient:
mcc(two_class_example, truth, predicted)

# F1 metric:
f_meas(two_class_example, truth, predicted)

# Combining these three classification metrics together
classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(two_class_example, truth = truth, estimate = predicted)

f_meas(two_class_example, truth, predicted, event_level = "second")

two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve

roc_auc(two_class_example, truth, Class1)

##### 9.4 Multiclass classification metrics #####
data(hpc_cv)
tibble(hpc_cv)

accuracy(hpc_cv, obs, pred)

mcc(hpc_cv, obs, pred)

class_totals <- 
  count(hpc_cv, obs, name = "totals") %>% 
  mutate(class_wts = totals / sum(totals))
class_totals

cell_counts <- 
  hpc_cv %>% 
  group_by(obs, pred) %>% 
  count() %>% 
  ungroup()

# Compute the four sensitivities using 1-vs-all
one_versus_all <- 
  cell_counts %>% 
  filter(obs == pred) %>% 
  full_join(class_totals, by = "obs") %>% 
  mutate(sens = n / totals)
one_versus_all

# Three different estimates:
one_versus_all %>% 
  summarize(
    macro = mean(sens), 
    macro_wts = weighted.mean(sens, class_wts),
    micro = sum(n) / sum(totals)
  )

sensitivity(hpc_cv, obs, pred, estimator = "macro")

sensitivity(hpc_cv, obs, pred, estimator = "macro_weighted")

sensitivity(hpc_cv, obs, pred, estimator = "micro")

