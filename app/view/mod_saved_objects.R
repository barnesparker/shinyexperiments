box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  gt,
  rt = reactable,
  rtextras = reactable.extras,
  tibble,
  htmltools,
  stringr,
  utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sh$tagList(
    rtextras$reactable_extras_dependency(),
    bs$layout_columns(
      rt$reactableOutput(ns("recipes_rt")),
      rt$reactableOutput(ns("models_rt"))
    )
  )
}

#' @export
server <- function(id, saved_recipes, saved_models, saved_tune_configs) {
  moduleServer(id, function(input, output, session) {
    output$recipes_rt <- rt$renderReactable({
      tibble$tibble(
        Recipe = names(saved_recipes)
      ) |>
        rt$reactable(
          columns = list(
            Recipe = rt$colDef(
              name = "Recipe"
            )
          ),
          selection = "multiple",
          onClick = "select",
          details = function(index) {
            recipe_id <- names(saved_recipes)[index]

            htmltools$div(
              htmltools$pre(utils$capture.output(saved_recipes[[recipe_id]], type = "message") |>
                stringr$str_flatten(collapse = "\n"))
            )
          }
        )
    })

    output$models_rt <- rt$renderReactable({
      tibble$tibble(
        Model = names(saved_models)
      ) |>
        rt$reactable(
          columns = list(
            Model = rt$colDef(
              name = "Model"
            )
          ),
          selection = "multiple",
          onClick = "select",
          details = function(index) {
            model_id <- names(saved_models)[index]
            htmltools::div(
              htmltools::tags$pre(paste(utils::capture.output(saved_models[[model_id]], cat("Tune Grid\n"), saved_tune_configs[[model_id]]), collapse = "\n"))
            )
          }
        )
    })
  })
}
