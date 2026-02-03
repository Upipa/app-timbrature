#' Calcola la Banca ore
#'
#' Funzione che dato il nome cognome di un dipendente calcola la sua banca ore.
#'
#' @param .display_name nome cognome del dipendente
#' @param timbrature_tbl tibble della tabella timbrature
#' @param pianificazione_tbl tibble della tabella pianificazione
#' @param pause_tbl tibble della tabella pause
#' @param permessi_tbl tibble della tabella permessi
#' @param causali_tbl tibble della tabella causali
#' @param trasferte_tbl tibble della tabella trasferte
#'
#' @return numerico con numero di secondi nella banca ore

banca_ore <- function(
  .display_name,
  timbrature_tbl,
  pianificazione_tbl,
  pause_tbl,
  permessi_tbl,
  causali_tbl,
  trasferte_tbl
) {
  anno_inizio_banca_ore <- 2026

  timbrato <- timbrature_tbl |>
    filter(year(clock_in_event_date_time) >= anno_inizio_banca_ore) |>
    filter_user(.display_name) |>
    daily_summarise(
      clock_in_event_date_time,
      timbrato,
      as.duration(clock_out_event_date_time - clock_in_event_date_time)
    )

  durata_pause <- timbrature_tbl |>
    filter(year(clock_in_event_date_time) >= anno_inizio_banca_ore) |>
    filter_user(.display_name) |>
    left_join_check(pause_tbl, join_by(id)) |>
    daily_summarise(
      clock_in_event_date_time,
      durata_pause,
      as.duration(breaks_end_date_time - breaks_start_date_time)
    )

  pianificato <- pianificazione_tbl |>
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

  permessi <- permessi_tbl |>
    filter(year(start_date_time) >= anno_inizio_banca_ore) |>
    filter_user(.display_name) |>
    left_join_check(
      causali_tbl,
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

  banca_ore_base <- utenti |>
    filter(display_name == .display_name) |>
    pull(banca_ore_iniziale_2026)

  banca_ore_base * 3600 + banca_ore_accumulata
}
