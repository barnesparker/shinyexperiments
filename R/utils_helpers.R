#' apply_steps
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
apply_steps <- function(rec, preproc_steps) {
  box::use(recipes)
  for (step in purrr::compact(preproc_steps)) {
    shiny::req(step$vars)
    var_exprs <-
      step$vars |>
      rlang::parse_exprs()
    rec <-
      get(step$func, envir = recipes)(rec, !!!var_exprs)
  }
  rec
}

#' build_recipe_from_config
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
build_recipe_from_config <- function(recipe_config, train_data) {
  rec <-
    recipes::recipe(
      as.formula(recipe_config$formula),
      data = train_data
    )
  rec <- apply_steps(rec, recipe_config$preproc_steps)
  rec
}



#' build_model_from_config
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# build_model_from_config <- function(model_config) {
#   box::use(parsnip)
#   do.call(
#     shiny::req(model_config$model_selection),
#     c(list(mode = model_config$model_mode, engine = shiny::req(model_config$engine)), model_config$params_list),
#     envir = parsnip
#   )
# }

#' build_tune_from_config
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
build_tune_from_config <- function(tune_config) {

}

#' get_saved_objects_list
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_saved_objects_list <- function(board, type, exp_id) {
  pin_names <-
    board |>
    pins::pin_list() |>
    purrr::keep(
      ~ stringr::str_starts(.x, exp_id) &
        stringr::str_split_1(.x, "_")[2] == type
    )

  pin_names
}

#' clean_object_names
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
clean_object_names <- function(object_names) {
  object_names |>
    purrr::map_chr(
      ~ stringr::str_remove_all(.x, "^(?:[^_]+_){2}")
    )
}


#' pluck_param
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import dials
pluck_param <- function(ls) {
  if (ls |> purrr::pluck_exists("range")) {
    ls |>
      purrr::pluck("range") |>
      eval() |>
      purrr::keep(~ .x != dials::unknown()) |>
      unlist()
  } else if (ls |> purrr::pluck_exists("values")) {
    ls |>
      purrr::pluck("values") |>
      eval()
  }
}

#' de_reactive
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
de_reactive <- function(x) { # function to recursively return all elements of list non reactive
  if (is.list(x)) {
    purrr::map(x, de_reactive)
  } else if (shiny::is.reactive(x)) {
    x()
  } else {
    x
  }
}

# function to select on columns who are numeric or have only 2 unique values
#' find_outcome_candiates
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
find_outcome_candiates <- function(df) {
  df |>
    purrr::keep(~ is.numeric(.x) | dplyr::n_distinct(.x, na.rm = T) == 2) |>
    colnames()
}

#' get_exp_board
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_exp_board <- function() {
  exp_board <- golem::get_golem_options("experiment_board")
  if (is.null(exp_board)) {
    exp_board <- pins::board_folder("_experiments", versioned = F)
  }
  exp_board
}

#' get_tune_grids_from_models
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_tune_grids_from_models <- function(model_ids) {
  tune_ids <-
    model_ids |>
    stringr::str_replace_all("_model_", "_tune_")

  tune_ids[tune_ids %in% pins::pin_list(get_exp_board())]
}

#' get_results_from_exp
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_results_from_exp <- function(exp_ids) {
  results_ids <-
    exp_ids |>
    stringr::str_replace_all("_experiment_", "_results_")

  results_ids[results_ids %in% pins::pin_list(get_exp_board())]
}

#' save_exp_config
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# save_exp_config <- function(config, exp_id, config_name, description = NULL, tags = NULL, config_type, pin_type = "json") {
#   exp_board <- get_exp_board()
#   exp_board |>
#     pins::pin_write(config,
#       name = paste0(exp_id, "_", config_type, "_", config_name),
#       description = description,
#       title = config_name,
#       tags = tags,
#       type = pin_type
#     )
# }

#' print_selected_objects
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
print_selected_objects <- function(selected_splits, selected_recipes, selected_models, selected_metrics, selected_exps = NULL) {
  out <- shiny::HTML(
    paste("Splitting Configs:", stringr::str_flatten_comma(clean_object_names(names(selected_splits))), "<br>"),
    paste("Preprocessors:", stringr::str_flatten_comma(clean_object_names(names(selected_recipes))), "<br>"),
    paste("Models:", stringr::str_flatten_comma(clean_object_names(names(selected_models))), "<br>"),
  )

  if (!is.null(selected_exps)) {
    out <- shiny::HTML(
      out,
      paste("Experiments:", stringr::str_flatten_comma(clean_object_names(names(selected_exps))))
    )
  }

  out
}

#' get_metrics_list
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_metrics_list <- function(mode) {
  if (mode == "classification") {
    c(
      # Class
      "Accuracy" = "accuracy",
      "Balanced Accuracy" = "bal_accuracy",
      "Detection Prevalance" = "detection_prevalence",
      "F1" = "f_meas",
      "J Index" = "j_index",
      "Kappa" = "kap",
      "MCC" = "mcc",
      "Pos Pred Value" = "ppv",
      "Neg Pred Value" = "npv",
      "Precision" = "precision",
      "Recall" = "recall",
      "Sensitivity" = "sensitivity",
      "Specificity" = "specificity",

      # Prob
      "Average Precision" = "average_precision",
      "Brier Class" = "brier_class",
      "Classification Cost" = "classification_cost",
      "Gain Captured" = "gain_capture",
      "Log Loss" = "mn_log_loss",
      "ROC AUC" = "roc_auc",
      "PR AUC" = "pr_auc"
    )
  } else if (mode == "regression") {
    c(
      "Concordance Correlation Coefficient" = "ccc",
      "Huber Loss" = "huber_loss",
      "Index of Ideality of Correlation" = "iic",
      "Mean Absolute Error" = "mae",
      "Mean Absolute Percentage Error" = "mape",
      "Mean Absolute Scaled Error" = "mase",
      "Mean Percentage Error" = "mpe",
      "Mean Signed Deviation" = "msd",
      "Root Mean Squared Error" = "rmse",
      "R Squared" = "rsq",
      "Symmetric Mean Absolute Percentage Error" = "smape"
    )
  }
}

#' get_metrics_list
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_default_metrics <- function(mode) {
  if (mode == "classification") {
    c("roc_auc", "accuracy")
  } else if (mode == "regression") {
    c("rmse", "rsq")
  }
}

#' get_exp_tags
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_exp_tags <- function() {
  exp_board <- get_exp_board()
  exp_board |>
    pins::pin_list() |>
    purrr::map(
      \(pin) {
        tags <- pins::pin_meta(exp_board, pin)$tags
        if (length(tags) == 0) {
          return(NA)
        } else {
          tags
        }
      }
    ) |>
    purrr::flatten_chr() |>
    unique() |>
    sort()
}

#' build_splits_from_config
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
build_splits_from_config <- function(split_config, data) {
  set.seed(split_config$seed)
  rsample::initial_split(
    data,
    strata = split_config$strata
  )
}

#' build_resamples_from_config
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
build_resamples_from_config <- function(split_config, split_object) {
  set.seed(split_config$seed)
  rsample::vfold_cv(
    rsample::training(split_object),
    v = split_config$folds,
    strata = split_config$strata,
    repeats = split_config$repeats
  )
}
