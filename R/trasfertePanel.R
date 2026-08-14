#' Pannello dei turni
#'
#' Pannello per la visualizzazione delle trasferte del mese e dei percorsi
#' @param id id character del pannello. Serve solo come riferimento per la parte server del modulo

trasfertePanelUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Trasferte",
    card(
      card_header("Percorsi"),
      gt_output(ns("percorsi"))
    ),
    card(
      card_header("Trasferte del mese"),
      gt_output(ns("trasferte_mese")),
      min_height = "33%"
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

#' Server del pannello Trasferte
#'
#' Si occupa della logica server di filtraggio dei dati in base ai filtri selezionati dalla sidebar
#'
#' @param id id character del pannello turni da gestire
#' @param dipendente nome cognome del dipendente su cui filtrare la visuale
#' @param anno anno numerico su cui filtrare la visuale
#' @param mese mese character abbreviato su cui filtrare i dati
#' @param trasferte_poll reactivePoll della tabella trasferte
#' @param percorsi_poll reactivePoll della tabella percorsi

trasfertePanelServer <- function(
  id,
  dipendente,
  anno,
  mese,
  trasferte_poll,
  percorsi_poll
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # trigger per aggiornamento immediato dopo inserimento
    trasferte_trigger <- reactiveVal(0)

    id_percorso <- reactive({
      input$aggiungi_trasferta_click |>
        str_extract("\\d+$") |>
        as.numeric()
    })

    observe({
      req(mese(), anno())
      min_date <- make_date(
        year = anno(),
        month = mese_numerico(mese()),
        day = 1
      )
      max_date <- (min_date %m+% months(1)) - days(1)
      giorni_seq <- seq.Date(min_date, max_date, by = "day")
      choices_giorni <- setNames(
        giorni_seq,
        str_c(
          wday(giorni_seq, label = TRUE),
          " ",
          day(giorni_seq)
        )
      )
      label_giorno <- str_glue(
        "In quale giorno hai svolto la trasferta? ({mese()} {anno()})"
      )

      showModal(
        modalDialog(
          selectInput(
            ns("giorno_trasferta"),
            label = label_giorno,
            choices = choices_giorni,
            selected = format(min_date, "%d")
          ),
          selectInput(
            ns("moltiplicatore_t"),
            "Quante volte devo aggiungere il tempo di viaggio al tuo tempo lavorato?",
            choices = c(2:0)
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
          footer = actionButton(
            ns("aggiungi_trasferta"),
            "Aggiungi trasferta"
          ),
          easyClose = TRUE
        )
      )
    }) |>
      bindEvent(input$aggiungi_trasferta_click)

    observe({
      user_id <- utenti |>
        filter(display_name == !!dipendente()) |>
        pull(user_id)

      trasferta_da_aggiungere <- percorsi_poll() |>
        filter(id == !!id_percorso()) |>
        select(-id) |>
        mutate(
          data = input$giorno_trasferta,
          user_id = user_id,
          moltiplicatore_t = input$moltiplicatore_t,
          moltiplicatore_km = input$moltiplicatore_km,
          note = input$note_trasferta
        )

      tryCatch(
        {
          dbAppendTable(pool, "trasferte", trasferta_da_aggiungere)
          info(log, "Nuova trasferta inserita")
          trasferte_trigger(trasferte_trigger() + 1)
          removeModal()
        },
        error = function(e) {
          error(
            log,
            str_glue("Errore inserimento trasferta: {conditionMessage(e)}")
          )
          showNotification(
            "Errore durante l'inserimento della trasferta. Riprova.",
            type = "error",
            duration = NULL
          )
        }
      )
    }) |>
      bindEvent(input$aggiungi_trasferta)

    output$tempo_aggiunto <- renderText({
      tempo <- percorsi_poll() |>
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
      distanza <- percorsi_poll() |>
        filter(id == !!id_percorso()) |>
        pull(distanza)

      vec_fmt_currency(
        distanza * 0.5 * as.numeric(input$moltiplicatore_km),
        locale = "it",
        output = "plain"
      )
    })

    output$percorsi <- render_gt(
      {
        percorsi_poll() |>
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

    trasferte_src <- reactiveVal()

    observe({
      trasferte_poll() |>
        trasferte_src()
    })

    observe({
      tbl(pool, "trasferte") |>
        collect() |>
        trasferte_src()
    }) |>
      bindEvent(trasferte_trigger())

    # render della tabella mese: dipende dal trigger (immediato) e dal poll (sincronizzazione esterna)
    output$trasferte_mese <- render_gt({
      # dipendenza per aggiornamento immediato

      # preferisco leggere nuovamente dal DB qui (o usare il valore del poll se lo preferisci)
      trasferte_tbl <- trasferte_src() |>
        filter_user(dipendente()) |>
        filter(
          year(data) == !!anno(),
          month(data) == !!mese_numerico(mese())
        ) |>
        mutate(
          rimborso = 0.5 * distanza * moltiplicatore_km,
          tempo_lavoro = tempo * moltiplicatore_t
        ) |>
        mutate(
          data = str_c(
            wday(data, label = TRUE),
            " ",
            day(data)
          )
        )

      trasferte_tbl |>
        gt() |>
        cols_hide(c(
          id,
          user_id,
          moltiplicatore_t,
          moltiplicatore_km,
          distanza,
          tempo
        )) |>
        cols_label(
          data = "Giorno",
          localita_di_partenza = "Località partenza",
          ente_di_partenza = "Ente partenza",
          indirizzo_di_partenza = "Indirizzo partenza",
          localita_di_arrivo = "Località arrivo",
          ente_di_arrivo = "Ente arrivo",
          indirizzo_di_arrivo = "Indirizzo arrivo",
          rimborso = "Rimborso",
          tempo_lavoro = "Tempo lavoro aggiunto",
          note = "Note"
        ) |>
        sub_missing() |>
        fmt_currency(rimborso, locale = "it") |>
        fmt_duration(
          tempo_lavoro,
          input_units = "hours",
          output_units = c("hours", "minutes")
        )
    })
  })
}
