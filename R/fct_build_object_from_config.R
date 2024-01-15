#' build_from_config
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
build_object_from_config <- S7::new_generic("build_object_from_config", "config")





#' build_from_config
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
S7::method(build_object_from_config, split_config) <- function(config, data) {
  set.seed(config@seed)
  list(
    initial = rsample::initial_split(
      data,
      strata = config@strata
    ),
    resamples = rsample::vfold_cv(
      data,
      v = config@folds,
      repeats = config@repeats,
      strata = config@strata
    )
  )
}

S7::method(build_object_from_config, preproc_config) <- function(config, train_data) {

  rec <-
    recipes::recipe(
      config@formula,
      data = train_data
    )
  rec <- apply_steps(rec, config@preproc_steps)
  rec
}

S7::method(build_object_from_config, model_config) <- function(config) {
  box::use(parsnip)
  do.call(
    config@type,
    c(list(mode = config@mode, engine = config@engine), config@params),
    envir = parsnip
  )
}

S7::method(build_object_from_config, tune_config) <- function(config) {
  box::use(dials)
  do.call(config@grid, config@params, envir = dials)
}


S7::method(build_object_from_config, metrics_config) <- function(config) {
  metrics <-
    config@metrics |>
    purrr::map_chr(~paste0("yardstick::", .x)) |>
    rlang::parse_exprs()
  yardstick::metric_set(!!!metrics)
}

