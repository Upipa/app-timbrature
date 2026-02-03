library(shiny)

ui <- function(req) {
    opts <- parseQueryString(req$QUERY_STRING)
    if (is.null(opts$code)) {
        auth_uri <- build_authorization_uri(
            resource,
            tenant,
            app,
            redirect_uri = redirect,
            version = 2
        )
        redir_js <- sprintf("location.replace(\"%s\");", auth_uri)
        tags$script(HTML(redir_js))
    } else {
        page_navbar(
            title = "Timbrature 2.0",
            sidebar = sidebar(
                title = "Filtri globali",
                conditionalPanel(
                    "output.ruolo == 'admin'",
                    selectInput("dipendente", "Dipendente", dipendente_choices)
                ),
                numericInput("anno", "Anno", year(today()), step = 1),
                selectInput(
                    "mese",
                    "Mese",
                    mese_choices,
                    selected = month(today(), label = TRUE)
                ),
                value_box(
                    "Banca ore",
                    textOutput("banca_ore"),
                    showcase = bs_icon("hourglass-split")
                )
            ),
            nav_spacer(),
            turniPanelUI("turni_panel"),
            trasfertePanelUI("trasferte_panel")
        )
    }
}
