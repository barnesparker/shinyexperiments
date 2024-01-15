exp_data_source <-
  S7::new_class(
    "exp_data_source",
    package = "shinyexperiments",
    properties = list(
      data = S7::new_property(
        S7::class_data.frame,
        getter = function(self) {
          ingest_data_source(self)
        }
      )
    ),
    abstract = T
  )

data_source_pinned <-
  S7::new_class(
    "data_source_pinned",
    package = "shinyexperiments",
    parent = exp_data_source,
    properties = list(
      pin_name = S7::class_character,
      pin_version = S7::class_character,
      data_board = S7::new_property(
        S7::new_S3_class("pins_board"),
        getter = function(self) {
          get_golem_config("data_board")
        }
      )
    )
  )

data_source_demo <-
  S7::new_class(
    "data_source_demo",
    package = "shinyexperiments",
    parent = exp_data_source,
    properties = list(
      demo_name = S7::class_character
    )
  )

data_source_file <-
  S7::new_class(
    "data_source_file",
    package = "shinyexperiments",
    parent = exp_data_source,
    properties = list(
      data_path = S7::new_property(
        S7::class_character,
        validator = function(value) {
          if (!stringr::str_ends(value, ".csv")) {
            "file must be a csv"
          }
        }
      )
    )
  )
