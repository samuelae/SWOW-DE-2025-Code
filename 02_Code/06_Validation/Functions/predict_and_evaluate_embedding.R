predict_and_evaluate_embedding <- function(data) {
  
  set.seed(1337)
  
  # Split data
  split <- initial_split(data, prop = 0.8)
  train_data <- training(split)
  test_data <- testing(split)
  
  # Create a recipe for preprocessing
  recipe <- recipe(dv ~ ., data = train_data) %>%
    step_normalize(all_predictors())
  
  # Define ridge regression model
  ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
    set_engine("glmnet")
  
  # Set up cross-validation
  cv_folds <- vfold_cv(train_data, v = 10)
  
  # Workflow with recipe and model
  ridge_workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(ridge_spec)
  
  # Tune the model
  tuned_ridge <- tune_grid(
    ridge_workflow,
    resamples = cv_folds,
    grid = 25,
    metrics = metric_set(rmse, rsq)
  )
  
  # Select best model
  best_ridge <- select_best(tuned_ridge, metric = "rmse")
  
  # Finalize workflow and fit to training data
  final_ridge_workflow <- finalize_workflow(ridge_workflow, best_ridge)
  final_ridge_fit <- last_fit(final_ridge_workflow, split = split)
  
  # extract metrics and CI
  # test_metrics <- collect_metrics(final_ridge_fit)
  test_metrics <- int_pctl(final_ridge_fit, alpha = 0.05)
  
  
  output <- list("rsq" = test_metrics[test_metrics$.metric == "rsq", ]$.estimate,
                 "ci_lower" = test_metrics[test_metrics$.metric == "rsq", ]$.lower,
                 "ci_upper" = test_metrics[test_metrics$.metric == "rsq", ]$.upper)
  
  output
  
}
