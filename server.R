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

    user <- utenti |>
        filter(
            user_id == ms_graph$new(token = token)$get_user()$properties$id
        )

    output$ruolo <- reactive(user$ruolo)
    outputOptions(output, "ruolo", suspendWhenHidden = FALSE)

    updateSelectInput(session, "dipendente", selected = user$display_name)

    debug(
        log,
        str_glue(
            "Fatto l'update del select input con id 'dipendente' aggiornando la selezione a {user$display_name}"
        )
    )

    timbrature_poll <- poll(pool, "timbrature", refresh_time, session)
    pianificazione_poll <- poll(pool, "pianificazione", refresh_time, session)
    permessi_poll <- poll(pool, "permessi", refresh_time, session)
    causali_poll <- poll(pool, "causali", refresh_time, session)

    pause_poll <- reactivePoll(
        refresh_time,
        session,
        checkFunc = function() {
            tbl(pool, "timbrature") |>
                summarise(
                    last_modified_date_time = max(last_modified_date_time)
                ) |>
                collect()
        },
        valueFunc = function() {
            tbl(pool, "pause") |>
                collect()
        }
    )

    trasferte_poll <- poll(
        pool,
        "trasferte",
        refresh_time,
        session,
        check_col = id,
        check_fn = sum
    )
    percorsi_poll <- poll(
        pool,
        "percorsi",
        refresh_time,
        session,
        check_col = id,
        check_fn = sum
    )

    output$banca_ore <- renderText({
        scales::number(
            banca_ore(
                input$dipendente,
                timbrature_poll(),
                pianificazione_poll(),
                pause_poll(),
                permessi_poll(),
                causali_poll(),
                trasferte_poll()
            ),
            scale_cut = scales::cut_time_scale()
        )
    }) |>
        bindEvent(
            input$dipendente,
            timbrature_poll(),
            pianificazione_poll(),
            pause_poll(),
            permessi_poll(),
            causali_poll(),
            trasferte_poll(),
            ignoreInit = TRUE
        )

    turniPanelServer(
        "turni_panel",
        reactive(input$dipendente),
        reactive(input$anno),
        reactive(input$mese),
        timbrature_poll,
        pianificazione_poll,
        pause_poll,
        permessi_poll,
        causali_poll
    )

    trasfertePanelServer(
        "trasferte_panel",
        reactive(input$dipendente),
        reactive(input$anno),
        reactive(input$mese),
        trasferte_poll,
        percorsi_poll
    )
}
