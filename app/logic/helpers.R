box::use(
  purrr,
  recipes,
  sh = shiny,
  dials = dials[unknown]
)

#' @export
apply_steps <- function(rec, preproc_steps) {
  sh$req(rec)
  for (step in purrr$compact(preproc_steps)) {
    sh$req(step$vars())
    rec <- do.call(step$func(), list(rec, step$vars()), envir = recipes)
  }
  rec
}

#' @export
pluck_param <- function(ls) {
  if (ls |> purrr$pluck_exists("range")) {
    ls |> purrr$pluck("range") |> eval() |>
      purrr$keep(~.x != dials$unknown()) |>
      unlist()
  } else if (ls |> purrr$pluck_exists("values")) {
    ls |> purrr$pluck("values") |> eval(envir = dials)
  }
}
