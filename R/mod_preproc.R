#' preproc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_preproc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::accordion(
      open = T,
      id = ns("preproc_accordion"),
      bslib::accordion_panel(
        "Formula Definition",
        shiny::selectInput(
          ns("outcome_select"),
          "Outcome",
          choices = NULL
        ),
        shiny::selectInput(
          ns("predictor_select"),
          "Predictors",
          choices = NULL,
          multiple = T
        ),
        shiny::verbatimTextOutput(ns("formula_preview"))
      ),
      bslib::accordion_panel(
        "Recipe Steps",
        bslib::accordion(
          id = ns("rec_steps_accordion"),
          open = T
        ),
        bslib::layout_columns(
          id = ns("rec_manage_buttons"),
          shiny::actionButton(
            ns("add_step"),
            "Add Step",
            icon = shiny::icon("plus")
          ),
          shiny::actionButton(
            ns("remove_step"),
            "Remove Step",
            icon = shiny::icon("trash")
          ),
          shiny::actionButton(
            ns("preview_recipe_button"),
            "Preview Recipe",
            icon = shiny::icon("eye")
          ),
          shiny::actionButton(
            ns("save_recipe_button"),
            "Save Recipe",
            icon = shiny::icon("save")
          )
        )
      )
    ),
    shiny::uiOutput(ns("recipe_preview"))
  )
}

#' preproc Server Functions
#'
#' @noRd
mod_preproc_server <- function(id, reactive_training, saved_recipes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::observe({
      outcome_candidates <-
        reactive_training() |>
        find_outcome_candiates()

      shiny::freezeReactiveValue(input, "outcome_select")
      shiny::updateSelectInput(
        session,
        "outcome_select",
        choices = outcome_candidates,
        selected = outcome_candidates[1]
      )
    })

    shiny::observe({
      shiny::freezeReactiveValue(input, "predictor_select")
      cols <- colnames(reactive_training())
      cols <- cols[cols != input$outcome_select]
      shiny::updateSelectInput(
        session,
        "predictor_select",
        choices = cols,
        selected = cols
      )
    })
    preproc_steps <- shiny::reactiveValues()

    step_count <- shiny::reactiveVal(0)


    shiny::observe({
      step_count(step_count() + 1)

      rec_step_id <- paste0("mod_rec_step_", step_count())

      bslib::accordion_panel_insert(
        id = "rec_steps_accordion",
        # target = paste0("Step", step_count() - 1),
        panel = bslib::accordion_panel(
          paste0("Step", step_count()),
          mod_rec_step_ui(
            ns(rec_step_id),
            step_count()
          )
        )
      )

      bslib::accordion_panel_open(
        id = "rec_steps_accordion",
        # session = session,
        values = paste0("Step", step_count())
        # values = T
      )
    }) |>
      shiny::bindEvent(input$add_step)

    shiny::observe({
      shiny::req(step_count() > 0)
      preproc_steps[[as.character(step_count())]] <- mod_rec_step_server(
        paste0("mod_rec_step_", step_count()),
        step_count,
        reactive_training
      )
    })



    shiny::observe({
      bslib::accordion_panel_remove(
        "rec_steps_accordion",
        paste0("Step", step_count())
      )

      preproc_steps[[as.character(step_count())]] <- NULL

      step_count(step_count() - 1)
    }) |>
      shiny::bindEvent(input$remove_step)



    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Save Recipe",
          shiny::textInput(ns("recipe_name"), "Recipe Name", value = "recipe"),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_save_button"),
              "Save Recipe",
              icon = shiny::icon("save")
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$save_recipe_button)

    shiny::observe({
      shiny::removeModal()

      saved_recipes[[input$recipe_name]] <- reactive_recipe()
    }) |>
      shiny::bindEvent(input$confirm_save_button)




    reactive_formula <-
      shiny::reactive({
        as.formula(
          paste0(
            shiny::req(input$outcome_select),
            " ~ ",
            paste0(shiny::req(input$predictor_select), collapse = " + ")
          )
        )
      })

    output$formula_preview <-
      shiny::renderPrint({
        print(reactive_formula(), showEnv = F)
      })


    reactive_recipe <-
      shiny::reactive({
        recipes::recipe(
          shiny::req(reactive_formula()),
          data = reactive_training()
        ) |>
          apply_steps(shiny::reactiveValuesToList(preproc_steps))
      })

    reactive_recipe_prepped <-
      shiny::reactive({
        reactive_recipe() |>
          recipes::prep()
      }) |>
      shiny::bindEvent(input$preview_recipe_button)




    output$recipe_preview <-
      shiny::renderUI({
        shiny::tags$pre(paste(capture.output(reactive_recipe_prepped(), type = "message"), collapse = "\n"))
      }) |>
      shiny::bindEvent(input$preview_recipe_button)

    train_data_juiced <- shiny::reactive({
      reactive_recipe() |>
        recipes::prep() |>
        recipes::juice()
    })

    reactive_mode <-
      shiny::reactive({
        if (dplyr::n_distinct(reactive_training()[[input$outcome_select]], na.rm = T) == 2) {
          "classification"
        } else {
          "regression"
        }
      })

    reactive_recipe_prepped
  })
}

## To be copied in the UI
# mod_preproc_ui("preproc_1")

## To be copied in the server
# mod_preproc_server("preproc_1")
