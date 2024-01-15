#' print recipe
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
print.recipe <- function(rec) {
  paste(capture.output(rec, type = "message"), collapse = "\n") |>
    cat("\n\n")
}

#' print experiment
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
print.experiment <- function(exp) {
  exp_board <- get_exp_board()
  purrr::walk2(
    names(exp),
    exp,
    \(object_type, object_ids) {
      if (object_type == "wflow_set") {
        return()
      } else {
        cat(object_type, "------------------\n")
        purrr::walk(
          object_ids,
          \(id) {
            object <- pins::pin_read(exp_board, id)
            if (object_type == "Splits") {
              print(clean_object_names(object))
            } else {
              print(object)
            }
          }
        )
      }
    }
  )


  # exp
}



#' exp_config
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
exp_config <-
  S7::new_class(
    "exp_config",
    package = "shinyexperiments",
    properties = list(
      name = S7::class_character,
      description = S7::class_character,
      tags = S7::class_character,
      exp_id = S7::new_property(
        S7::class_character,
        default = get_golem_config("exp_id")
      ),
      outcome = S7::class_character,
      id = S7::new_property(
        S7::class_character,
        getter = function(self) {
          type <-
            class(self)[1] |>
            stringr::str_remove_all("_config|shinyexperiments::")

          paste0(self@exp_id, "_", type, "_", self@name)
        }
      )
    )
    # abstract = T
  )

#' split_config
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
split_config <-
  S7::new_class(
    "split_config",
    parent = exp_config,
    package = "shinyexperiments",
    properties = list(
      split_prop = S7::class_double,
      strata = S7::class_character,
      folds = S7::class_integer,
      repeats = S7::class_integer,
      seed = S7::class_integer
    )
  )

# test_split_config <-
#   split_config(
#     name = "test_split_config",
#     exp_id = "5c10954a1c203eab69d6b",
#     split_prop = 0.7,
#     strata = "Species",
#     folds = 5L,
#     repeats = 5L,
#     seed = 123L,
#     data_source = data_source_pinned("iris")
#   )



preproc_config <-
  S7::new_class(
    "preproc_config",
    parent = exp_config,
    package = "shinyexperiments",
    properties = list(
      predictors = S7::class_character,
      preproc_steps = S7::class_list,
      formula = S7::new_property(
        S7::new_S3_class("formula"),
        getter = function(self) {
          as.formula(
            paste(
              self@outcome, "~", paste(self@predictors, collapse = " + ")
            ),
          )
        }
      )
    )
  )

model_config <-
  S7::new_class(
    "model_config",
    parent = exp_config,
    package = "shinyexperiments",
    properties = list(
      type = S7::class_character,
      mode = S7::class_character,
      engine = S7::class_character,
      params = S7::class_list
    )
  )

tune_config <-
  S7::new_class(
    "tune_config",
    parent = exp_config,
    package = "shinyexperiments",
    properties = list(
      grid = S7::class_function,
      params = S7::class_list
    )
  )

metrics_config <-
  S7::new_class(
    "metrics_config",
    parent = exp_config,
    package = "shinyexperiments",
    properties = list(
      metrics = S7::class_list
    )
  )
