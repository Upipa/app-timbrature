library(shiny)
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

source("R/db_timbrature_pool.R")

Sys.setlocale(category = "LC_ALL", locale = "it_IT.UTF-8")

pool <- db_timbrature_pool()

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
