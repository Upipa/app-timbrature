trasfertePanelUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Trasferte",
    card(
      card_header("Percorsi"),
      gt_output(ns("percorsi"))
    )
  )
}

trasfertePanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$percorsi <- render_gt(
        {
          tbl(pool, "percorsi") |>
            collect() |>
            gt() |>
            cols_hide(id) |>
            cols_label(
              localita_di_partenza = "Località partenza",
              ente_di_partenza = "Ente partenza",
              indirizzo_di_partenza = "Indirizzo partenza",
              localita_di_arrivo = "Località arrivo",
              ente_di_arrivo = "Ente arrivo",
              indirizzo_di_arrivo = "Indirizzo arrivo",
              distanza = "Distanza",
              tempo = "Tempo di viaggio"
            ) |>
            fmt_number(
              distanza,
              decimals = 1,
              pattern = "{x} km"
            ) |>
            fmt_duration(
              tempo,
              input_units = "hours",
              output_units = c("hours", "minutes")
            ) |>
            sub_missing() |>
            opt_interactive(
              use_filters = TRUE
            )
        }
      )
    }
  )
}
