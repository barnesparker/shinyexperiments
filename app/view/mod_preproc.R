box::use(
  sh = shiny,
  bs = bslib,
  recipes,
  rsample,
  stats,
  purrr,
  dp = dplyr,
  utils
)

box::use(
  app/view/mod_rec_step,
  app/logic/helpers
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  sh$tagList(
    bs$accordion(
      open = T,
      id = ns("preproc_accordion"),
      bs$accordion_panel(
        "Formula Definition",
        sh$selectInput(
          ns("outcome_select"),
          "Outcome",
          choices = NULL
        ),
        sh$selectInput(
          ns("predictor_select"),
          "Predictors",
          choices = NULL,
          multiple = T
        ),
        sh$verbatimTextOutput(ns("formula_preview"))
      ),
      bs$accordion_panel(
        "Recipe Steps",
        bs$accordion(
          id = ns("rec_steps_accordion"),
          open = T
        ),
        bs$layout_columns(
          id = ns("rec_manage_buttons"),
          sh$actionButton(
            ns("add_step"),
            "Add Step",
            icon = sh$icon("plus")
          ),
          sh$actionButton(
            ns("remove_step"),
            "Remove Step",
            icon = sh$icon("trash")
          ),
          sh$actionButton(
            ns("preview_recipe_button"),
            "Preview Recipe",
            icon = sh$icon("eye")
          ),
          sh$actionButton(
            ns("save_recipe_button"),
            "Save Recipe",
            icon = sh$icon("save")
          )
        )
      )
    ),
    sh$uiOutput(ns("recipe_preview"))
  )
}

#' @export
server <- function(id, reactive_training, saved_recipes) {
  sh$moduleServer(id, function(input, output, session) {
    ns <- session$ns




    sh$observe({
      outcome_candidates <-
        reactive_training() |>
        helpers$find_outcome_candiates()

      sh$freezeReactiveValue(input, "outcome_select")
      sh$updateSelectInput(
        session,
        "outcome_select",
        choices = outcome_candidates,
        selected = outcome_candidates[1]
      )
    })

    sh$observe({
      sh$freezeReactiveValue(input, "predictor_select")
      cols <- colnames(reactive_training())
      cols <- cols[cols != input$outcome_select]
      sh$updateSelectInput(
        session,
        "predictor_select",
        choices = cols,
        selected = cols
      )
    })
    preproc_steps <- sh$reactiveValues()

    step_count <- sh$reactiveVal(0)


    sh$observe({
      step_count(step_count() + 1)

      rec_step_id <- paste0("mod_rec_step_", step_count())

      bs$accordion_panel_insert(
        id = "rec_steps_accordion",
        # target = paste0("Step", step_count() - 1),
        panel = bs$accordion_panel(
          paste0("Step", step_count()),
          mod_rec_step$ui(
            ns(rec_step_id),
            step_count()
          )
        )
      )

      bs$accordion_panel_open(
        id = "rec_steps_accordion",
        # session = session,
        values = paste0("Step", step_count())
        # values = T
      )
    }) |>
      sh$bindEvent(input$add_step)

    sh$observe({
      sh$req(step_count() > 0)
      preproc_steps[[as.character(step_count())]] <- mod_rec_step$server(
        paste0("mod_rec_step_", step_count()),
        step_count,
        reactive_training
      )
    })



    sh$observe({
      bs$accordion_panel_remove(
        "rec_steps_accordion",
        paste0("Step", step_count())
      )

      preproc_steps[[as.character(step_count())]] <- NULL

      step_count(step_count() - 1)
    }) |>
      sh$bindEvent(input$remove_step)



    sh$observe({
      sh$showModal(
        sh$modalDialog(
          title = "Save Recipe",
          sh$textInput(ns("recipe_name"), "Recipe Name", value = "recipe"),
          footer = sh$tagList(
            sh$modalButton("Cancel"),
            sh$actionButton(
              ns("confirm_save_button"),
              "Save Recipe",
              icon = sh$icon("save")
            )
          )
        )
      )
    }) |>
      sh$bindEvent(input$save_recipe_button)

    sh$observe({
      sh$removeModal()

      saved_recipes[[input$recipe_name]] <- reactive_recipe()
    }) |>
      sh$bindEvent(input$confirm_save_button)




    reactive_formula <-
      sh$reactive({
        stats$as.formula(
          paste0(
            sh$req(input$outcome_select),
            " ~ ",
            paste0(sh$req(input$predictor_select), collapse = " + ")
          )
        )
      })

    output$formula_preview <-
      sh$renderPrint({
        print(reactive_formula(), showEnv = F)
      })


    reactive_recipe <-
      sh$reactive({
        recipes$recipe(
          sh$req(reactive_formula()),
          data = reactive_training()
        ) |>
          helpers$apply_steps(sh$reactiveValuesToList(preproc_steps))
      }) |>
      sh$bindEvent(input$preview_recipe_button)




    output$recipe_preview <-
      sh$renderUI({
        recipe_prepped <-
          reactive_recipe() |>
          recipes$prep()

        sh$tags$pre(paste(utils$capture.output(recipe_prepped, type = "message"), collapse = "\n"))
      }) |>
      sh$bindEvent(input$preview_recipe_button)

    train_data_juiced <- sh$reactive({
      reactive_recipe() |>
        recipes$prep() |>
        recipes$juice()
    })

    reactive_mode <-
      sh$reactive({
        if (dp$n_distinct(reactive_training()[[input$outcome_select]], na.rm = T) == 2) {
          "classification"
        } else {
          "regression"
        }
      })

    reactive_recipe
  })
}
