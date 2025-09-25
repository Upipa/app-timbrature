library(shiny)

ui <- page_navbar(
    title = "Timbrature 2.0",
    tags$head(
        tags$link(
            href = "font-awesome-6.5.2/css/all.min.css",
            rel = "stylesheet"
        ),
        tags$link(
            href = "font-awesome-6.5.2/css/v4-shims.min.css",
            rel = "stylesheet"
        )
    ),
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
