#' apply_steps
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import recipes
apply_steps <- function(rec, preproc_steps) {
  shiny::req(rec)
  box::use(recipes)
  for (step in purrr::compact(preproc_steps)) {
    shiny::req(step$vars())
    rec <- get(step$func())(rec, get(step$vars())())
  }
  rec
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
