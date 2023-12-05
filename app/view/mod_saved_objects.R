box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  rt = reactable,
  tibble,
  htmltools,
  stringr,
  utils,
  workflowsets,
  shinyjs
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sh$tagList(
    shinyjs$useShinyjs(),
    bs$layout_columns(
      rt$reactableOutput(ns("recipes_rt")),
      rt$reactableOutput(ns("models_rt"))
    ),
    sh$actionButton(ns("create_wflowset_button"), "Create Workflow Set")
  )
}

#' @export
server <- function(id, saved_recipes, saved_models, saved_tune_configs, saved_wflowsets) {
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


    sh$observe({
      sh$showModal(
        sh$modalDialog(
          title = "Create Workflow Set",
          bs$layout_columns(
            sh$textInput(session$ns("wflowset_name"), "Workflow Set Name", value = "workflowset"),
            sh$checkboxInput(session$ns("cross_checkbox"), "Cross?", value = T)
          ),
          footer = sh$tagList(
            sh$modalButton("Cancel"),
            sh$actionButton(session$ns("confirm_create_wflow"), "Create Workflow Set")
          )
        )
      )
    }) |>
      sh$bindEvent(input$create_wflowset_button)


    selected_recipes <- sh$reactive(sh$reactiveValuesToList(saved_recipes)[rt$getReactableState("recipes_rt", "selected", session = session)])
    selected_models <- sh$reactive(sh$reactiveValuesToList(saved_models)[rt$getReactableState("models_rt", "selected", session = session)])

    sh$observe({
      if (length(selected_recipes()) == 0 | length(selected_models()) == 0) {
        shinyjs$disable("create_wflowset_button")
      } else {
        shinyjs$enable("create_wflowset_button")
      }
    })

    sh$observe({
      sh$removeModal()

      wflow_set <-
        workflowsets$workflow_set(
          preproc = selected_recipes(),
          models = selected_models(),
          cross = input$cross_checkbox
        ) |>
        workflowsets$option_add(
          grid = 20
        )


      for (grid in names(saved_tune_configs)) {
        for (wflow_id in wflow_set$wflow_id) {
          if (stringr$str_ends(wflow_id, paste0("_", grid))) {
            wflow_set <- wflow_set |>
              workflowsets$option_add(
                grid = saved_tune_configs[[grid]],
                id = wflow_id
              )
          }
        }
      }


      saved_wflowsets[[input$wflowset_name]] <- wflow_set
    }) |>
      sh$bindEvent(input$confirm_create_wflow)
  })
}
