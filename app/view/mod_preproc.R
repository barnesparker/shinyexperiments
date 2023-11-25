box::use(
  sh = shiny,
  bs = bslib,
  recipes,
  rsample,
  stats,
  purrr,
  dp = dplyr
)

box::use(
  app/view/mod_rec_step
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
        sh$uiOutput(ns("outcome_select_ui")),
        sh$uiOutput(ns("predictor_select_ui"))
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
            ns("preview_recipe"),
            "Preview Recipe",
            icon = sh$icon("eye")
          )
        )
      )
    ),
    sh$textOutput(ns("recipe_preview"))
  )
}

#' @export
server <- function(id, reactive_training) {
  sh$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # function to select on columns who are numeric or have only 2 unique values
    find_outcome_candiates <- function(df) {
      df |>
        purrr$keep(~is.numeric(.x) | dp$n_distinct(.x, na.rm = T) == 2) |>
        colnames()
    }


    output$outcome_select_ui <- sh$renderUI({

      outcome_candidates <-
        reactive_training() |>
        find_outcome_candiates()

      sh$selectInput(
        ns("outcome_select"),
        "Outcome",
        choices = outcome_candidates
      )
    })

    output$predictor_select_ui <- sh$renderUI({
      cols <- colnames(reactive_training())
      cols <- cols[cols != input$outcome_select]
      sh$selectInput(
        ns("predictor_select"),
        "Predictors",
        choices = cols,
        selected = cols,
        multiple = T
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

    apply_steps <- function(rec) {
      sh$req(rec)
      for (step in purrr$compact(sh$reactiveValuesToList(preproc_steps))) {
        sh$req(step$vars())
        rec <- do.call(step$func(), list(rec, step$vars()), envir = recipes)
      }
      rec
    }


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


    reactive_recipe <-
      sh$reactive({
        recipes$recipe(
          sh$req(reactive_formula()),
          data = reactive_training()
        ) |>
          apply_steps()
      })




    output$recipe_preview <-
      sh$renderText({
        reactive_recipe()
      }) |>
      sh$bindEvent(input$preview_recipe)

    train_data_juiced <- sh$reactive({
      reactive_recipe() |>
        recipes$prep() |>
        recipes$juice()
    })

    reactive_mode <-
      sh$reactive({
        if (dp$n_distinct(reactive_training()[[input$outcome_select]], na.rm = T) == 2)
          "classification"
        else
          "regression"
      })


    reactive_recipe
    # list(
    # preproc_steps,
    # train_data_juiced
    # )
  })
}
