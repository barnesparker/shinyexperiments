#' save_object_dialog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_save_object_dialog_ui <- function(id, label = "Save") {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shiny::actionButton(
      ns("save_object_button"),
      label = label,
      icon = shiny::icon("save")
    )
  )
}

#' save_object_dialog Server Functions
#'
#' @noRd
mod_save_object_dialog_server <- function(id, type, reactive_config,
                                          object_name = shiny::reactive(NULL),
                                          enable_var = shiny::reactiveVal(T)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    config_type <- switch(type,
      Recipe = "preproc",
      `Splitting Configuration` = "split",
      Model = "model",
      `Tune Config` = "tune",
      Metrics = "metrics",
      Results = "results",
      Experiment = "experiment"
    )

    shiny::observe({
      if (!enable_var()) {
        shinyjs::disable("save_object_button")
      } else {
        shinyjs::enable("save_object_button")
      }
    })

    output$object_preview <-
      shiny::renderPrint({
        # if (type == "Recipe") {
        #   shiny::tags$pre(paste(capture.output(reactive_config(), type = "message"), collapse = "\n"))
        # } else {
          # print(reactive_config())
        reactive_config()
          # build_object_from_config()
        # }
      })

    shiny::observe({
      if (type == "Tune Config") {
        title <- paste("Save Tune Configuration for model spec:", object_name())
      } else if (type == "Results") {
        title <- paste("Save Results for Experiment:", object_name())
      } else {
        title <- paste("Save", type)
      }
      shiny::showModal(
        shiny::modalDialog(
          title = title,
          # shiny::verbatimTextOutput(ns("object_preview")),
          if (!type %in% c("Tune Config", "Results")) {
            shiny::textInput(ns("object_name"), "Name", value = config_type)
          },
          shiny::textAreaInput(
            ns("object_description"),
            "Description (optional)"
          ),
          shiny::selectizeInput(
            ns("object_tags"),
            "Add Tags",
            choices = get_exp_tags(),
            multiple = T,
            selected = NULL,
            options = list(
              plugins = list("remove_button"),
              create = T
            )
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_save_button"),
              "Save",
              icon = shiny::icon("save")
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$save_object_button)


    shiny::observe({
      shiny::removeModal()

      config_name <- ifelse(type %in% c("Tune Config", "Results"), object_name(), input$object_name)
      pin_type <- ifelse(type == "Results", "csv", "rds")
      save_exp_config(
        reactive_config(),
        config_name,
        description = dplyr::coalesce(input$object_description, NA_character_),
        tags = dplyr::coalesce(input$object_tags, NA_character_)
      )
    }) |>
      shiny::bindEvent(input$confirm_save_button)
  })
}
