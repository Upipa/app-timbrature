library(bslib)
library(lubridate)
library(forcats)
library(pool)
library(odbc)
library(DT)
library(hms)
library(stringr)
library(dplyr)
library(dbplyr)
library(log4r)
library(bsicons)
library(tidyr)
library(purrr)
library(gt)
library(AzureAuth)
library(AzureGraph)
library(Microsoft365R)

options(rsconnect.locale = "it_IT.UTF-8")
Sys.setlocale(category = "LC_ALL", locale = "it_IT.UTF-8")


source("R/db_timbrature_pool.R")

log <- logger(config::get("log_level"))
info(log, str_glue("Log level impostato su {config::get('log_level')}"))
info(log, str_glue("LC_TIME impostato su {Sys.getlocale('LC_TIME')}"))

info(log, "Tentativo di connessione al database")
pool <- db_timbrature_pool()
info(log, "Connessione avvenuta con successo")

utenti <- tbl(pool, "utenti") |>
    collect()

dipendente_choices <- utenti |>
    arrange(display_name) |>
    pull(display_name)

mese_choices <- ordered(
    c(
        "gen",
        "feb",
        "mar",
        "apr",
        "mag",
        "giu",
        "lug",
        "ago",
        "set",
        "ott",
        "nov",
        "dic"
    ),
    levels = c(
        "gen",
        "feb",
        "mar",
        "apr",
        "mag",
        "giu",
        "lug",
        "ago",
        "set",
        "ott",
        "nov",
        "dic"
    )
)

tenant <- "f017cce5-ae05-41bc-ab46-d4bfe78b7c4c"
app <- "04ce4067-073d-4801-95ce-c116ec3ed36d"

redirect <- config::get("redirect")
port <- httr::parse_url(redirect)$port
options(shiny.port = if (is.null(port)) 3838 else as.numeric(port))

pwd <- Sys.getenv("SHINY_CLIENT_SECRET")
if (pwd == "") {
    pwd <- NULL
}

resource <- c(
    "https://graph.microsoft.com/.default",
    "openid",
    "offline_access"
)

refresh_time <- 56000


onStop(function() {
    info(log, "Applicazione terminata")
})
