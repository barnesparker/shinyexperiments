#' saved_objects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_saved_objects_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    bslib::layout_columns(
      reactable::reactableOutput(ns("splits_rt")),
      reactable::reactableOutput(ns("recipes_rt")),
      reactable::reactableOutput(ns("models_rt"))
    ),
    shiny::actionButton(ns("create_wflowset_button"), "Create Workflow Set")
  )
}

#' saved_objects Server Functions
#'
#' @noRd
mod_saved_objects_server <- function(id, saved_split_configs, raw_data, saved_recipes, saved_models, saved_tune_configs, saved_wflowsets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$splits_rt <- reactable::renderReactable({
      tibble::tibble(
        Split = names(saved_split_configs)
      ) |>
        reactable::reactable(
          columns = list(
            Split = reactable::colDef(
              name = "Split"
            )
          ),
          selection = "single",
          onClick = "select",
          details = function(index) {
            split_id <- names(saved_split_configs)[index]
            split_config <- saved_split_configs[[split_id]]
            set.seed(split_config$seed)
            split <- rsample::initial_split(
              raw_data(),
              prop = split_config$split_prop,
              strata = split_config$strata
            )

            resamples <-
              rsample::vfold_cv(
                rsample::training(split),
                v = split_config$folds,
                strata = split_config$strata,
                repeats = split_config$repeats
              )

            # bslib::layout_columns(
            shiny::tags$div(
              shiny::tags$pre(paste(capture.output(
                cat("Seed:", split_config$seed, "\n", "Strata:", split_config$strata, "\n"), split, resamples, type = "output"), collapse = "\n"))
            )
            # htmltools::div(
            #   shiny::tags$pre(paste(capture.output(resamples, type = "message"), collapse = "\n"))
            # )
            # )
          }
        )
    })

    output$recipes_rt <- reactable::renderReactable({
      tibble::tibble(
        Recipe = names(saved_recipes)
      ) |>
        reactable::reactable(
          columns = list(
            Recipe = reactable::colDef(
              name = "Recipe"
            )
          ),
          selection = "multiple",
          onClick = "select",
          details = function(index) {
            recipe_id <- names(saved_recipes)[index]

            htmltools::div(
              shiny::tags$pre(paste(capture.output(saved_recipes[[recipe_id]] |> recipes::prep(), type = "message"), collapse = "\n"))
            )
          }
        )
    })

    output$models_rt <- reactable::renderReactable({
      tibble::tibble(
        Model = names(saved_models)
      ) |>
        reactable::reactable(
          columns = list(
            Model = reactable::colDef(
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


    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Create Workflow Set",
          bslib::layout_columns(
            shiny::textInput(ns("wflowset_name"), "Workflow Set Name", value = "workflowset"),
            shiny::checkboxInput(ns("cross_checkbox"), "Cross?", value = T)
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("confirm_create_wflow"), "Create Workflow Set")
          )
        )
      )
    }) |>
      shiny::bindEvent(input$create_wflowset_button)

    selected_splits <- shiny::reactive(shiny::reactiveValuesToList(saved_split_configs)[reactable::getReactableState("splits_rt", "selected", session = session)])
    selected_recipes <- shiny::reactive(shiny::reactiveValuesToList(saved_recipes)[reactable::getReactableState("recipes_rt", "selected", session = session)])
    selected_models <- shiny::reactive(shiny::reactiveValuesToList(saved_models)[reactable::getReactableState("models_rt", "selected", session = session)])

    shiny::observe({
      if (any(
        length(selected_recipes()) == 0,
        length(selected_models()) == 0,
        length(selected_splits()) == 0
      )
      ) {
        shinyjs::disable("create_wflowset_button")
      } else {
        shinyjs::enable("create_wflowset_button")
      }
    })

    shiny::observe({
      shiny::removeModal()

      wflow_set <-
        workflowsets::workflow_set(
          preproc = selected_recipes(),
          models = selected_models(),
          cross = input$cross_checkbox
        ) |>
        workflowsets::option_add(
          grid = 20
        )


      for (grid in names(saved_tune_configs)) {
        for (wflow_id in wflow_set$wflow_id) {
          if (stringr::str_ends(wflow_id, paste0("_", grid))) {
            wflow_set <- wflow_set |>
              workflowsets::option_add(
                grid = saved_tune_configs[[grid]],
                id = wflow_id
              )
          }
        }
      }


      saved_wflowsets[[input$wflowset_name]] <- wflow_set
    }) |>
      shiny::bindEvent(input$confirm_create_wflow)
  })
}

## To be copied in the UI
# mod_saved_objects_ui("saved_objects_1")

## To be copied in the server
# mod_saved_objects_server("saved_objects_1")
