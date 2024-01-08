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
# print(exp)
