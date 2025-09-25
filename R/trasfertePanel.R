trasfertePanelUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Trasferte",
    card(
      card_header("Percorsi"),
      gt_output(ns("percorsi"))
    ),
    tags$script(HTML(sprintf(
      "
      (function(){
        var nsPrefix = '%s';                    // es: 'trasferte_panel-'
        var btnIdPrefix = '%s';                // es: 'trasferte_panel-aggiungi_trasferta_'

        document.addEventListener('click', function(e){
          var btn = e.target.closest('button[id^=\"' + btnIdPrefix + '\"]');
          if (btn) {
            // invia l'id del bottone cliccato a Shiny con il nome namespaced
            Shiny.setInputValue(nsPrefix + 'aggiungi_trasferta_click', btn.id, {priority: 'event'});
          }
        });
      })();
    ",
      ns(""),
      ns("aggiungi_trasferta_")
    )))
  )
}

trasfertePanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$aggiungi_trasferta_click, {
        showNotification(paste0(
          "Hai cliccato il bottone con id: ",
          input$aggiungi_trasferta_click
        ))
        # qui parsare l'id oppure rimuovere il prefisso con sub(ns(""), "", input$aggiungi_trasferta_click)
      })

      output$percorsi <- render_gt(
        {
          ns <- session$ns

          tbl(pool, "percorsi") |>
            collect() |>
            mutate(
              button = str_glue("aggiungi_trasferta_{id}"),
              button = ns(button),
              button = str_glue(
                '<button id=\"{button}\" type=\"button\" class=\"btn btn-default action-button\">\n  <i class=\"fas fa-plus\" role=\"presentation\" aria-label=\"plus icon\"></i>\n</button>'
              )
            ) |>
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
            fmt_markdown(button) |>
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
