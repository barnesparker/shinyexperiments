#' preproc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import recipes
mod_preproc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::accordion(
      open = T,
      id = ns("preproc_accordion"),
      bslib::accordion_panel(
        "Select Predictors",
        shiny::uiOutput(ns("predictor_select_ui")),
        shiny::verbatimTextOutput(ns("formula_preview"))
      ),
      bslib::accordion_panel(
        "Preprocessing Steps",
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
            "Preview",
            icon = shiny::icon("eye")
          ),
          mod_save_object_dialog_ui(
            ns("save_object_dialog_recipe"),
            "Save"
          )
          # shiny::actionButton(
          #   ns("save_recipe_button"),
          #   "Save Recipe",
          #   icon = shiny::icon("save")
          # )
        )
      )
    ),
    shiny::uiOutput(ns("recipe_preview"))
  )
}

#' preproc Server Functions
#'
#' @noRd
mod_preproc_server <- function(id, reactive_training, saved_recipes, outcome, exp_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$predictor_select_ui <- shiny::renderUI({
      cols <- colnames(reactive_training())
      cols <- cols[cols != outcome()]
      shiny::selectizeInput(
        ns("predictor_select"),
        "Predictors",
        multiple = T,
        choices = cols,
        selected = cols,
        options = list(
          plugins = list("remove_button")
        )
      )
    })

    pred_cols <- shiny::reactiveValues()

    # shiny::observe({
    #   pred_cols[["0"]] <- c(input$predictor_select, outcome())
    # })
    preproc_steps <- shiny::reactiveValues()

    step_count <- shiny::reactiveVal(0)


    shiny::observe({
      step_count(step_count() + 1)

      rec_step_title <- paste0("Step", step_count())
      rec_step_id <- paste0("mod_rec_step_", step_count())

      bslib::accordion_panel_insert(
        id = "rec_steps_accordion",
        # target = paste0("Step", step_count() - 1),
        panel = bslib::accordion_panel(
          rec_step_title,
          mod_rec_step_ui(
            ns(rec_step_id),
            step_count()
          )
        )
      )

      bslib::accordion_panel_open(
        id = "rec_steps_accordion",
        # session = session,
        values = rec_step_title
        # values = T
      )
    }) |>
      shiny::bindEvent(input$add_step)

    shiny::observe({
      shiny::req(step_count() > 0)
      preproc_steps[[as.character(step_count())]] <- mod_rec_step_server(
        paste0("mod_rec_step_", step_count()),
        step_count(), # this should be a static value for the server
        reactive_training,
        pred_cols
      )
    })



    shiny::observe({
      shiny::req(step_count() > 0)
      bslib::accordion_panel_remove(
        "rec_steps_accordion",
        paste0("Step", step_count())
      )

      preproc_steps[[as.character(step_count())]] <- NULL

      step_count(step_count() - 1)
    }) |>
      shiny::bindEvent(input$remove_step)



    mod_save_object_dialog_server("save_object_dialog_recipe", "Recipe", reactive_recipe, exp_id)

    # shiny::observe({
    #   shiny::removeModal()
    #
    #   # saved_recipes[[input$recipe_name]] <- reactive_recipe()
    #
    #   # recipe_config <-
    #   #   list(
    #   #     formula = Reduce(paste, deparse(reactive_formula())),
    #   #     preproc_steps = shiny::reactiveValuesToList(preproc_steps) |> de_reactive()
    #   #   )
    #
    #   save_exp_config(reactive_recipe(), exp_id(), input$recipe_name, config_type = "preproc")
    #
    # }) |>
    #   shiny::bindEvent(input$confirm_save_button)




    reactive_formula <-
      shiny::reactive({
        as.formula(
          paste0(
            outcome(),
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

        recipe_steps <- shiny::reactiveValuesToList(preproc_steps)

        recipes::recipe(
          shiny::req(reactive_formula()),
          data = reactive_training()
        ) |>
          apply_steps(recipe_steps)
      })

    reactive_recipe_prepped <-
      shiny::reactive({
        shiny::req(reactive_recipe())
        reactive_recipe() |>
          recipes::prep()
      }) |>
      shiny::bindEvent(input$preview_recipe_button)

    shiny::observe({
      current_cols <-
        reactive_recipe_prepped() |>
        summary() |>
        dplyr::pull(variable)

      pred_cols[[as.character(step_count())]] <- current_cols
    })



    output$recipe_preview <-
      shiny::renderUI({
        shiny::tags$pre(paste(capture.output(reactive_recipe_prepped(), type = "message"), collapse = "\n"))
      }) |>
      shiny::bindEvent(input$preview_recipe_button)

    train_data_juiced <- shiny::reactive({
      reactive_recipe_prepped() |>
        recipes::juice()
    })

    reactive_recipe_prepped
  })
}

## To be copied in the UI
# mod_preproc_ui("preproc_1")

## To be copied in the server
# mod_preproc_server("preproc_1")
