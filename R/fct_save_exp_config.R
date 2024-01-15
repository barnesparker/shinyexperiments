#' save_exp_config
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
save_exp_config <- function(config, name, description = NA_character_, tags = NA_character_) {
  exp_board <- get_exp_board()

  config@name <- name
  config@description <- description
  config@tags <- tags

  exp_board |>
    pins::pin_write(
      config,
      name = config@id,
      description = description,
      title = name,
      tags = tags
      # type = "json"
    )
}
