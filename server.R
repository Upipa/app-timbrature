server <- function(input, output, session) {
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

    output$timbrature_table <- renderDT(
        {
            mese_numerico <- which(mese_choices == input$mese)

            tbl(pool, "timbrature") |>
                left_join(tbl(pool, "utenti"), join_by(user_id)) |>
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