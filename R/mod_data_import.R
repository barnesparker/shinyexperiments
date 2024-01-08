#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_ui <- function(id) {
  ns <- NS(id)
  shiny::modalDialog(
    shiny::radioButtons(
      ns("data_source"),
      "Choose a data source",
      choices = c(
        "Pinned data set (recommended)" = "pinned",
        "Demo dataset from modeldata" = "demo",
        "Local file upload" = "file"
      ),
      selected = "demo"
    ),
    shiny::uiOutput(ns("dataset_picker_ui")),
    shiny::uiOutput(ns("outcome_select")),
    shiny::verbatimTextOutput(ns("raw_data_glimpse")),
    footer = shiny::actionButton(
      ns("confirm_data_import_button"),
      "Confirm",
      icon = shiny::icon("check")
    )
  )
}

#' data_import Server Functions
#'
#' @noRd
mod_data_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_board <- golem::get_golem_options("data_board")


    output$dataset_picker_ui <-
      shiny::renderUI({
        if (shiny::req(input$data_source) == "demo") {
          box::use(modeldata)
          datasets <- ls(modeldata)
          shiny::selectInput(
            ns("demo_dataset"),
            "Choose a dataset",
            choices = datasets[datasets != "%>%" &
              !datasets |> stringr::str_starts("sim_|bivariate")],
            selected = "credit_data"
          )
        } else if (input$data_source == "pinned") {
          if (!is.null(data_board)) {
            shiny::selectInput(
              ns("pinned_dataset_name"),
              "Choose a dataset",
              choices = pins::pin_list(data_board)
            )
          } else {
            shiny::h4("No data board found")
          }
        } else if (input$data_source == "file") {
          shiny::fileInput(
            ns("file_dataset"),
            "Choose a dataset",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        }
      })


    dataset_choice <-
      shiny::reactive({
        box::use(modeldata)
        if (shiny::req(input$data_source) == "pinned") {
          if (is.null(data_board)) {
            print("Please re-run using run_app(data_board = pins::board_*()")
          } else {
            pins::pin_read(data_board, shiny::req(input$pinned_dataset_name))
          }
        } else if (input$data_source == "file") {
          readr::read_csv(shiny::req(input$file_dataset$datapath))
        } else if (input$data_source == "demo") {
          dat <- modeldata[[shiny::req(input$demo_dataset)]]
          # if (input$demo_dataset == "penguins") {
          #   dat <- dat |> tidyr::drop_na(body_mass_g)
          # }
          dat
        }
      })

    dataset_hash <-
      shiny::reactive({
        if (shiny::req(input$data_source) == "pinned") {
          shiny::req(data_board)
          pins::pin_meta(data_board, shiny::req(input$pinned_dataset_name))$pin_hash
        } else if (input$data_source == "file") {
          digest::digest(shiny::req(input$file_dataset$datapath), file = T, algo = "xxhash64")
        } else {
          digest::digest(shiny::req(input$demo_dataset), file = F, algo = "xxhash64")
        }
      })

    output$outcome_select <- shiny::renderUI({
      shiny::selectInput(
        ns("outcome_select"),
        "Outcome",
        choices = find_outcome_candiates(dataset_choice())
      )
    })

    output$raw_data_glimpse <- shiny::renderPrint({
      dplyr::glimpse(shiny::req(dataset_choice()))
    })

    list(
      dataset_choice = dataset_choice,
      dataset_hash = dataset_hash,
      outcome = shiny::reactive(input$outcome_select),
      confirm_data_import_button = reactive(input$confirm_data_import_button)
    )
  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_1")

## To be copied in the server
# mod_data_import_server("data_import_1")
