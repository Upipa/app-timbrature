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
    ))),
    tags$span(style = "display:none;", icon("plus")) # Modo brutto per assicurarsi che venga caricato il necessario per font awesome allo stesso modo di come farebbe normalmente shiny
  )
}

trasfertePanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      id_percorso <- reactive({
        input$aggiungi_trasferta_click |>
          str_extract("\\d+$") |>
          as.numeric()
      })

      observe({
        showModal(
          modalDialog(
            selectInput(
              ns("moltiplicatore_t"),
              "Quante volte devo aggiungere il tempo di viaggio al tuo tempo lavorato?",
              choices = c(
                2:0
              )
            ),
            selectInput(
              ns("moltiplicatore_km"),
              "Quale tratta ti deve essere rimborsata?",
              choices = c(
                "Andata e ritorno" = 2,
                "Solo uno dei sensi" = 1,
                "Nessuna (es. passaggio da un'altra persona)" = 0
              )
            ),
            textAreaInput(
              ns("note_trasferta"),
              "Note aggiuntive (facoltativo)"
            ),
            layout_column_wrap(
              value_box(
                "Tempo lavoro aggiunto",
                textOutput(ns("tempo_aggiunto")),
                showcase = bs_icon("clock")
              ),
              value_box(
                textOutput(ns("titolo_rimborso")),
                textOutput(ns("euro_aggiunti")),
                showcase = bs_icon("currency-euro")
              )
            ),
            footer = actionButton("aggiungi_trasferta", "Aggiungi trasferta"),
            easyClose = TRUE
          )
        )
      }) |>
        bindEvent(input$aggiungi_trasferta_click)

      output$tempo_aggiunto <- renderText({
        tempo <- tbl(pool, "percorsi") |>
          filter(id == !!id_percorso()) |>
          pull(tempo)

        vec_fmt_duration(
          tempo * as.numeric(input$moltiplicatore_t),
          input_units = "hours",
          output_units = c("hours", "minutes"),
          locale = "it"
        )
      })

      output$euro_aggiunti <- renderText({
        distanza <- tbl(pool, "percorsi") |>
          filter(id == !!id_percorso()) |>
          pull(distanza)

        vec_fmt_currency(
          distanza * 0.5 * as.numeric(input$moltiplicatore_km),
          locale = "it"
        )
      })

      output$percorsi <- render_gt(
        {
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
            cols_move(ente_di_partenza, localita_di_partenza) |>
            cols_move(ente_di_arrivo, localita_di_arrivo) |>
            cols_label(
              localita_di_partenza = "Località partenza",
              ente_di_partenza = "Ente partenza",
              indirizzo_di_partenza = "Indirizzo partenza",
              localita_di_arrivo = "Località arrivo",
              ente_di_arrivo = "Ente arrivo",
              indirizzo_di_arrivo = "Indirizzo arrivo",
              distanza = "Distanza",
              tempo = "Tempo di viaggio",
              button = ""
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

      output$titolo_rimborso <- renderText({
        localita_urbane <- tbl(pool, "localita_urbane") |>
          pull(localita)

        trasferta_urbana <- tbl(pool, "percorsi") |>
          filter(id == !!id_percorso()) |>
          collect() |>
          summarise(
            trasferta_urbana = str_to_lower(localita_di_partenza) %in%
              localita_urbane &
              str_to_lower(localita_di_arrivo) %in% localita_urbane
          ) |>
          pull(trasferta_urbana)

        if (trasferta_urbana) {
          "Premio aggiunto a fine anno per trasferte urbane"
        } else {
          "Rimborso chilometrico aggiuntivo"
        }
      })
    }
  )
}
