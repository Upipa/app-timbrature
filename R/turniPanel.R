#' Pannello dei turni
#'
#' Pannello per la visualizzazione delle timbrature, pause e pianificazione di un dipendente in un dato anno/mese
#'
#' @param id id character del pannello. Serve solo come riferimento per la parte server del modulo

turniPanelUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Turni",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Timbrature"),
        DTOutput(ns("timbrature_table"))
      ),
      card(
        card_header("Pause"),
        DTOutput(ns("pause_table"))
      )
    ),
    card(
      card_header("Pianificazione"),
      gt_output(ns("pianificazione")),
      full_screen = TRUE
    )
  )
}

#' Server del pannello Turni
#'
#' Si occupa della logica server di filtraggio dei dati in base ai filtri selezionati dalla sidebar
#'
#' @param id id character del pannello turni da gestire
#' @param dipendente() nome cognome del dipendente su cui filtrare la visuale
#' @param anno() anno numerico su cui filtrare la visuale
#' @param mese() mese character abbreviato su cui filtrare i dati

turniPanelServer <- function(id, dipendente, anno, mese) {
  moduleServer(
    id,
    function(input, output, session) {
      timbrature_data <- reactivePoll(
        27000,
        session,
        checkFunc = function() {
          tbl(pool, "timbrature") |>
            filter_user(dipendente()) |>
            filter(
              year(clock_in_event_date_time) == !!anno(),
              month(clock_in_event_date_time) == !!mese_numerico(mese())
            ) |>
            summarise(
              last_modified_date_time = max(last_modified_date_time)
            ) |>
            collect()
        },
        valueFunc = function() {
          debug(
            log,
            str_glue(
              "Filtri globali:
                display_name == {dipendente()},
                year(clock_in_event_date_time) == {anno()},
                 month(clock_in_event_date_time) == {mese_numerico(mese())}"
            )
          )

          tbl(pool, "timbrature") |>
            filter_user(dipendente()) |>
            filter(
              year(clock_in_event_date_time) == !!anno(),
              month(clock_in_event_date_time) == !!mese_numerico(mese())
            ) |>
            collect() |>
            mutate(
              Giorno = str_c(
                wday(clock_in_event_date_time, label = TRUE),
                " ",
                day(clock_in_event_date_time)
              ),
              across(where(is.POSIXct), ~ with_tz(., "Europe/Rome")),
              Entrata = as_hms(clock_in_event_date_time) |>
                parse_hm() |>
                str_remove(":00$"),
              Uscita = as_hms(clock_out_event_date_time) |>
                parse_hm() |>
                str_remove(":00$")
            ) |>
            arrange(clock_in_event_date_time)
        }
      )

      output$timbrature_table <- renderDT(
        {
          timbrature_data() |>
            select(
              Giorno,
              Entrata,
              Uscita
            )
        },
        options = list(
          info = FALSE,
          ordering = FALSE,
          paging = FALSE,
          searching = FALSE
        ),
        rownames = FALSE,
        fillContainer = TRUE,
        selection = "single"
      )

      output$pause_table <- renderDT(
        {
          timecard_id <- timbrature_data()$id[
            input$timbrature_table_rows_selected
          ]

          req(timecard_id)

          tbl(pool, "pause") |>
            filter(id == timecard_id) |>
            collect() |>
            mutate(
              `Inizio pausa` = as_hms(breaks_start_date_time) |>
                parse_hm() |>
                str_remove(":00$"),
              `Fine pausa` = as_hms(breaks_end_date_time) |>
                parse_hm() |>
                str_remove(":00$")
            ) |>
            select(`Inizio pausa`, `Fine pausa`)
        },
        options = list(
          info = FALSE,
          ordering = FALSE,
          paging = FALSE,
          searching = FALSE
        ),
        rownames = FALSE,
        fillContainer = TRUE
      )

      output$pianificazione <- render_gt({
        pianificazione(anno(), mese(), dipendente())
      })
    }
  )
}
