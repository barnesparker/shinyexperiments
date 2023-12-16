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
  box::use(modeldata)
  shiny::modalDialog(
    shiny::radioButtons(
      ns("data_source"),
      "Choose a data source",
      choices = c(
        "Pinned data set (recommended)" = "pinned",
        "Demo dataset from modeldata" = "demo",
        "Local file upload" = "file"
      )
    ),
    shiny::uiOutput(ns("dataset_picker_ui")),
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



    box::use(modeldata)

    output$dataset_picker_ui <-
      shiny::renderUI({
        if (shiny::req(input$data_source) == "demo") {
          shiny::selectInput(
            ns("demo_dataset"),
            "Choose a dataset",
            choices = ls(modeldata)[ls(modeldata) != "%>%"],
            selected = "penguins"
          )
        } else if (input$data_source == "pinned") {
          if (!is.null(data_board)) {
            shiny::selectInput(
              ns("pinned_dataset"),
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
        if (shiny::req(input$data_source) == "pinned") {
          if (is.null(data_board)) {
            "Please re-run using run_app(data_board = pins::board_*()"
          } else {
            pins::pin_read(data_board, shiny::req(input$pinned_dataset))
          }
        } else if (input$data_source == "file") {
          readr::read_csv(shiny::req(input$file_dataset$datapath))
        } else if (input$data_source == "demo") {
          modeldata[[shiny::req(input$demo_dataset)]]
        }
      })

    output$raw_data_glimpse <- shiny::renderPrint({
      dplyr::glimpse(shiny::req(dataset_choice()))
    })

    list(
      dataset_choice = dataset_choice,
      confirm_data_import_button = reactive({
        shiny::req(input$confirm_data_import_button)
      })
    )
  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_1")

## To be copied in the server
# mod_data_import_server("data_import_1")
