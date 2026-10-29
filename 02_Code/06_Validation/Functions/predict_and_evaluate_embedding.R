predict_and_evaluate_embedding <- function(data) {
  
  set.seed(1337)
  
  # Split data
  split <- initial_split(data, prop = 0.8)
  train_data <- training(split)
  
  # Create a recipe for preprocessing
  recipe <- recipe(dv ~ ., data = train_data) %>%
    step_normalize(all_predictors())
  
  # use ridge with glm if n_predictor > 1, otherwise regular lm
  n_predictors <- ncol(data) - 1 
  
  if (n_predictors == 1) {
    
    # one predictor (frequency): ordinary linear regression, nothing to tune
    final_workflow <- workflow() %>%
      add_recipe(recipe) %>%
      add_model(linear_reg() %>% set_engine("lm"))
    
  } else {
    
    # multiple predictors: ridge regression with tuned penalty
    ridge_workflow <- workflow() %>%
      add_recipe(recipe) %>%
      add_model(linear_reg(penalty = tune(), mixture = 0) %>%
                  set_engine("glmnet", path_values = 10^seq(-4, 4, length.out = 100)))
    
    cv_folds <- vfold_cv(train_data, v = 10)
    
    tuned_ridge <- tune_grid(
      ridge_workflow,
      resamples = cv_folds,
      grid = grid_regular(penalty(range = c(-4, 4)), levels = 50),
      metrics = metric_set(rmse, rsq)
    )
    
    best_ridge <- select_best(tuned_ridge, metric = "rmse")
    final_workflow <- finalize_workflow(ridge_workflow, best_ridge) 
  }
  
  # Fit to training data, compute metrics on test data
  final_fit <- last_fit(
    final_workflow,
    split = split,
    metrics = metric_set(rmse, rsq, rsq_trad)
  )
  
  # extract metrics and CI
  test_metrics <- int_pctl(final_fit, alpha = 0.05)
  
  # collect predictions
  preds <- collect_predictions(final_fit)
  
  output <- list(
    rsq       = test_metrics$.estimate[test_metrics$.metric == "rsq_trad"],
    ci_lower  = test_metrics$.lower[test_metrics$.metric == "rsq_trad"],
    ci_upper  = test_metrics$.upper[test_metrics$.metric == "rsq_trad"],
    rsq_cor   = test_metrics$.estimate[test_metrics$.metric == "rsq"],
    cal_slope = unname(coef(lm(dv ~ .pred, data = preds))[2]),
    penalty   = if (n_predictors > 1) best_ridge$penalty else NA_real_
  )
  
  output
  
}
