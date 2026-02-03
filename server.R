server <- function(input, output, session) {
    info(log, "Applicazione avviata")

    opts <- parseQueryString(isolate(session$clientData$url_search))
    if (is.null(opts$code)) {
        return()
    }

    token <- get_azure_token(
        resource,
        tenant,
        app,
        password = pwd,
        auth_type = "authorization_code",
        authorize_args = list(redirect_uri = redirect),
        version = 2,
        use_cache = FALSE,
        auth_code = opts$code
    )

    user <- tbl(pool, "utenti") |>
        filter(
            user_id == ms_graph$new(token = token)$get_user()$properties$id
        ) |>
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
        "trasferte_panel",
        reactive(input$dipendente),
        reactive(input$anno),
        reactive(input$mese)
    )
}
