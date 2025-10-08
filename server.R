server <- function(input, output, session) {
    info(log, "Applicazione avviata")

    user <- if (config::is_active("shinyapps")) {
        session$user
    } else {
        config::get("user")
    }

    user <- tbl(pool, "utenti") |>
        filter(email == user) |>
        collect()

    output$ruolo <- reactive(user$ruolo)
    outputOptions(output, "ruolo", suspendWhenHidden = FALSE)

    updateSelectInput(session, "dipendente", selected = user$display_name)

    debug(
        log,
        str_glue(
            "Fatto l'update del select input con id 'dipendente' aggiornando la selezione a {user$display_name}"
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

    trasfertePanelServer(
        "trasferte_panel"
    )
}
