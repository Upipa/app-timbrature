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
    debug(log, str_glue("Fatto l'update del select input con id 'dipendente' aggiornando la selezione a {dipendente_loggato}"))

    output$timbrature_table <- renderDT(
        {
            mese_numerico <- which(mese_choices == input$mese)

            nrow_before <- tbl(pool, "timbrature") |> 
                count() |> 
                pull(n)

            joined_data <- tbl(pool, "timbrature") |>
                left_join(tbl(pool, "utenti"), join_by(user_id))

            nrow_after <- joined_data |> 
                count() |> 
                pull(n)

            if(nrow_before != nrow_after) {
                warn(log, "Numero di record inatteso nella join tra timbrature e utenti")
            }

            debug(log, str_glue("Filtri globali:
                display_name == {input$dipendente},
                year(clock_in_event_date_time) == {input$anno},
                 month(clock_in_event_date_time) == {mese_numerico}"))

            joined_data |>
                filter(
                    display_name == input$dipendente,
                    year(clock_in_event_date_time) == input$anno,
                    month(clock_in_event_date_time) == mese_numerico
                ) |>
                mutate(
                    Giorno = day(clock_in_event_date_time)
                ) |>
                collect() |>
                mutate(
                    across(where(is.POSIXct), ~ with_tz(., "Europe/Rome")),
                    Entrata = as_hms(clock_in_event_date_time) |>
                        parse_hm() |>
                        str_remove(":00$"),
                    Uscita = as_hms(clock_out_event_date_time) |>
                        parse_hm() |>
                        str_remove(":00$")
                ) |>
                arrange(clock_in_event_date_time) |>
                select(
                    Giorno,
                    Entrata,
                    Uscita
                )
        },
        rownames = FALSE,
        fillContainer = TRUE
    )
}