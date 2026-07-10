library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(log4r)

# Source di tutte le funzioni dell'app (path risolto dalla project root)
app_root <- rprojroot::find_root(rprojroot::has_file(".Rprofile"))
for (f in list.files(
  file.path(app_root, "R"),
  full.names = TRUE,
  pattern = "\\.R$"
)) {
  source(f)
}

# Sopprime i warning di join durante i test
log <- logger("ERROR")

# ── Utente di test ────────────────────────────────────────────────────────────

test_user <- "Test Utente"
test_user_id <- "utente-test-001"

# ── Factory functions ─────────────────────────────────────────────────────────
# Ogni funzione costruisce una tibble minimale con i campi necessari alla
# funzione banca_ore(). I default producono un singolo record il 2026-03-01.

make_utenti <- function(banca_ore_iniziale = 0) {
  tibble(
    display_name = test_user,
    user_id = test_user_id,
    banca_ore_iniziale_2026 = as.numeric(banca_ore_iniziale)
  )
}

make_timbrature <- function(
  date = "2026-03-01",
  in_time = "08:00:00",
  out_time = "16:00:00",
  id = 1L
) {
  tibble(
    id = id,
    user_id = test_user_id,
    clock_in_event_date_time = ymd_hms(paste(date, in_time)),
    clock_out_event_date_time = ymd_hms(paste(date, out_time))
  )
}

make_pause <- function(
  timbratura_id = 1L,
  date = "2026-03-01",
  start_time = "12:00:00",
  end_time = "12:30:00"
) {
  tibble(
    id = timbratura_id,
    breaks_start_date_time = ymd_hms(paste(date, start_time)),
    breaks_end_date_time = ymd_hms(paste(date, end_time))
  )
}

make_pianificazione <- function(
  date = "2026-03-01",
  start_time = "08:00:00",
  end_time = "16:00:00"
) {
  tibble(
    user_id = test_user_id,
    start_date_time = ymd_hms(paste(date, start_time)),
    end_date_time = ymd_hms(paste(date, end_time)),
    activities_start_date_time = as.POSIXct(NA),
    activities_end_date_time = as.POSIXct(NA)
  )
}

make_permessi <- function(
  date = "2026-03-01",
  start_time = "08:00:00",
  end_time = "12:00:00",
  reason_id = "causale-001"
) {
  tibble(
    user_id = test_user_id,
    start_date_time = ymd_hms(paste(date, start_time)),
    end_date_time = ymd_hms(paste(date, end_time)),
    time_off_reason_id = reason_id
  )
}

make_causali <- function(id = "causale-001", riduce_pianificazione = TRUE) {
  tibble(
    id = id,
    is_active = TRUE,
    riduce_pianificazione = riduce_pianificazione,
    display_name = "Ferie"
  )
}

make_trasferte <- function(
  date = "2026-03-01",
  tempo = 1,
  moltiplicatore_t = 1
) {
  tibble(
    user_id = test_user_id,
    data = as_date(date),
    tempo = as.numeric(tempo),
    moltiplicatore_t = as.numeric(moltiplicatore_t)
  )
}

# ── Tabelle vuote ─────────────────────────────────────────────────────────────

empty_timbrature <- function() make_timbrature()[0L, ]
empty_pause <- function() make_pause()[0L, ]
empty_pianificazione <- function() make_pianificazione()[0L, ]
empty_permessi <- function() make_permessi()[0L, ]
empty_causali <- function() make_causali()[0L, ]
empty_trasferte <- function() make_trasferte()[0L, ]
