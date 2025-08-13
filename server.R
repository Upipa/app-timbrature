server <- function(input, output, session) {
    info(log, "Applicazione avviata")

    user <- if (config::is_active("shinyapps")) {
        session$user
    } else {
        config::get("user")
    }

    dipendente_loggato <- tbl(pool, "utenti") |>
        filter(email == user) |>
        pull(display_name)

    output$ruolo <- reactive({
        tbl(pool, "utenti") |>
            filter(email == user) |>
            pull(ruolo)
    })

    outputOptions(output, "ruolo", suspendWhenHidden = FALSE)

    updateSelectInput(session, "dipendente", selected = dipendente_loggato)
    debug(
        log,
        str_glue(
            "Fatto l'update del select input con id 'dipendente' aggiornando la selezione a {dipendente_loggato}"
        )
    )

    timbrature_data <- reactivePoll(
        27000,
        session,
        checkFunc = function() {
            mese_numerico <- which(mese_choices == input$mese)

            tbl(pool, "timbrature") |>
                left_join_check(tbl(pool, "utenti"), join_by(user_id)) |>
                filter(
                    display_name == input$dipendente,
                    year(clock_in_event_date_time) == input$anno,
                    month(clock_in_event_date_time) == mese_numerico
                ) |>
                summarise(
                    last_modified_date_time = max(last_modified_date_time)
                ) |>
                collect()
        },
        valueFunc = function() {
            mese_numerico <- which(mese_choices == input$mese)

            debug(
                log,
                str_glue(
                    "Filtri globali:
                display_name == {input$dipendente},
                year(clock_in_event_date_time) == {input$anno},
                 month(clock_in_event_date_time) == {mese_numerico}"
                )
            )

            tbl(pool, "timbrature") |>
                left_join_check(tbl(pool, "utenti"), join_by(user_id)) |>
                filter(
                    display_name == input$dipendente,
                    year(clock_in_event_date_time) == input$anno,
                    month(clock_in_event_date_time) == mese_numerico
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

    output$banca_ore <- renderText({
        scales::number(
            banca_ore(input$dipendente),
            scale_cut = scales::cut_time_scale()
        )
    })
}
