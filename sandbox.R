library(tidymodels)

data(Chicago)
# Use a small sample to keep file sizes down:
Chicago <- Chicago %>% slice(1:365)

base_recipe <-
  recipe(ridership ~ ., data = Chicago) %>%
  # create date features
  step_date(date) %>%
  step_holiday(date) %>%
  # remove date from the list of predictors
  update_role(date, new_role = "id") %>%
  # create dummy variables from factor columns
  step_dummy(all_nominal()) %>%
  # remove any columns with a single unique value
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())


filter_rec <-
  base_recipe %>%
  step_corr(all_of(stations), threshold = tune())

pca_rec <-
  base_recipe %>%
  step_pca(all_of(stations), num_comp = tune()) %>%
  step_normalize(all_predictors())

regularized_spec <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

cart_spec <-
  decision_tree(cost_complexity = tune(), min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

knn_spec <-
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")


chi_models <-
  workflow_set(
    preproc = list(simple = base_recipe, filter = filter_rec,
                   pca = pca_rec),
    models = list(glmnet = regularized_spec, cart = cart_spec,
                  knn = knn_spec),
    cross = TRUE
  )

chi_models <-
  chi_models %>%
  anti_join(tibble(wflow_id = c("pca_glmnet", "filter_glmnet")),
            by = "wflow_id")

splits <-
  sliding_period(
    Chicago,
    date,
    "day",
    lookback = 300,   # Each resample has 300 days for modeling
    assess_stop = 7,  # One week for performance assessment
    step = 7          # Ensure non-overlapping weeks for assessment
  )


set.seed(123)
chi_models <-
  chi_models %>%
  # The first argument is a function name from the {{tune}} package
  # such as `tune_grid()`, `fit_resamples()`, etc.
  workflow_map("tune_grid", resamples = splits, grid = 10,
               control = control_grid(save_pred = TRUE),
               metrics = metric_set(mae), verbose = TRUE)
chi_models |>
  autoplot()


chi_models |>
  extract_workflow_set_result("simple_glmnet") |>
  shinymodels::explore()
