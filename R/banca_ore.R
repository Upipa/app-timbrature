#' Calcola la Banca ore
#'
#' Funzione che dato il nome cognome di un dipendente calcola la sua banca ore.
#'
#' @param .display_name nome cognome del dipendente
#'
#' @return numerico con numero di secondi nella banca ore

banca_ore <- function(.display_name) {
  anno_inizio_banca_ore <- 2026

  timbrato <- tbl(pool, "timbrature") |>
    filter(year(clock_in_event_date_time) >= anno_inizio_banca_ore) |>
    filter_user(.display_name) |>
    daily_summarise(
      clock_in_event_date_time,
      timbrato,
      as.duration(clock_out_event_date_time - clock_in_event_date_time)
    )

  durata_pause <- tbl(pool, "timbrature") |>
    filter(year(clock_in_event_date_time) >= anno_inizio_banca_ore) |>
    filter_user(.display_name) |>
    left_join_check(tbl(pool, "pause"), join_by(id)) |>
    daily_summarise(
      clock_in_event_date_time,
      durata_pause,
      as.duration(breaks_end_date_time - breaks_start_date_time)
    )

  pianificato <- tbl(pool, "pianificazione") |>
    filter(year(end_date_time) >= anno_inizio_banca_ore) |>
    filter_user(.display_name) |>
    daily_summarise(
      start_date_time,
      pianificato,
      as.duration(end_date_time - start_date_time) -
        coalesce(
          as.duration(activities_end_date_time - activities_start_date_time),
          as.duration(0)
        )
    )

  permessi <- tbl(pool, "permessi") |>
    filter(year(start_date_time) >= anno_inizio_banca_ore) |>
    filter_user(.display_name) |>
    left_join_check(
      tbl(pool, "causali"),
      join_by(time_off_reason_id == id),
      suffix = c("", ".y")
    ) |>
    filter(is_active, riduce_pianificazione) |>
    select(-display_name) |>
    daily_summarise(
      start_date_time,
      permessi,
      as.duration(end_date_time - start_date_time)
    )

  banca_ore_accumulata <- timbrato |>
    full_join_check(durata_pause, join_by(giorno)) |>
    full_join_check(pianificato, join_by(giorno)) |>
    full_join_check(permessi, join_by(giorno)) |>
    mutate(
      across(-giorno, ~ coalesce(., 0)),
      lavorato = timbrato - durata_pause,
      pianificato_meno_permessi = pmax(pianificato - permessi, 0),
      banca_ore = (lavorato - pianificato_meno_permessi)
    ) |>
    summarise(banca_ore = sum(banca_ore)) |>
    pull(banca_ore)

  banca_ore_base <- tbl(pool, "utenti") |>
    filter(display_name == .display_name) |>
    pull(banca_ore_iniziale_2026)

  banca_ore_base * 3600 + banca_ore_accumulata
}
