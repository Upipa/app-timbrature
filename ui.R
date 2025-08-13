ui <- page_sidebar(
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
    card(
        card_header("Timbrature"),
        DTOutput("timbrature_table")
    ),
    card(
        card_header("Pause"),
        DTOutput("pause_table")
    )
)
