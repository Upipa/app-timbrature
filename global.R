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

options(rsconnect.locale = "it_IT.UTF-8")
Sys.setlocale(category = "LC_ALL", locale = "it_IT.UTF-8")


source("R/db_timbrature_pool.R")

log <- logger(config::get("log_level"))
info(log, str_glue("Log level impostato su {config::get('log_level')}"))
info(log, str_glue("LC_TIME impostato su {Sys.getlocale('LC_TIME')}"))

info(log, "Tentativo di connessione al database")
pool <- db_timbrature_pool()
info(log, "Connessione avvenuta con successo")

dipendente_choices <- tbl(pool, "utenti") |>
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

onStop(function() {
    info(log, "Applicazione terminata")
})
