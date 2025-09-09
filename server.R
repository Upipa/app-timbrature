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

    output$banca_ore <- renderText({
        scales::number(
            banca_ore(input$dipendente),
            scale_cut = scales::cut_time_scale()
        )
    })

    turniPanelServer(
        "turni_panel",
        reactive(input$dipendente),
        reactive(input$anno),
        reactive(input$mese)
    )

    output$percorsi <- renderDT(
        {
            tbl(pool, "percorsi") |>
                collect()
        },
        filter = "top"
    )
}
