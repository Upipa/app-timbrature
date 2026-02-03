#' Estrae la pianificazione mensile di un dipendente
#'
#' @param .anno anno numerico di cui estrarre la pianificazione
#' @param .mese label abbreviato del mese di cui estrarre la pianificazione
#' @param .display_name nome del dipendente di cui estrarre la pianificazione
#' @param pianificazione_tbl tibble della tabella pianificazione
#' @param permessi_tbl tibble della tabella permessi
#' @param causali_tbl tibble della tabella causali
#'
#' @return tabella gt adatta alla visualizzazione nell'applicazione

pianificazione <- function(
  .anno,
  .mese,
  .display_name,
  pianificazione_tbl,
  permessi_tbl,
  causali_tbl
) {
  mese_numerico <- which(mese_choices == .mese)
  giorni_nel_mese <- 1:days_in_month(ymd(str_glue("{.anno} {.mese} 1")))

  month_interval_start <- ymd(str_glue("{.anno} {.mese} 1"))
  month_interval_end <- month_interval_start + months(1) - seconds(1)

  month_interval <- month_interval_start %--% month_interval_end

  giorni <- tibble(
    giorno = ymd(str_glue("{.anno} {.mese} {giorni_nel_mese}"))
  ) |>
    mutate(
      intervalli_temporali = int_diff(force_tz(
        c(giorno, last(giorno) + days(1)),
        tz = "Europe/Rome"
      ))
    )

  pianificato <- pianificazione_tbl |>
    filter_user(.display_name) |>
    filter(
      year(start_date_time) == .anno,
      month(start_date_time) == mese_numerico
    ) |>
    daily_summarise(
      start_date_time,
      pianificato,
      as.duration(end_date_time - start_date_time) -
        coalesce(
          as.duration(activities_end_date_time - activities_start_date_time),
          as.duration(0)
        ),
      until_today = FALSE
    ) |>
    mutate(
      pianificato = pianificato / 3600
    )

  permessi <- permessi_tbl |>
    filter_user(.display_name) |>
    left_join(causali_tbl, join_by(time_off_reason_id == id)) |>
    mutate(
      across(start_date_time:end_date_time, ~ with_tz(., "Europe/Rome")),
      intervallo_permesso = start_date_time %--% end_date_time
    ) |>
    filter(int_overlaps(intervallo_permesso, month_interval))

  if (nrow(permessi) > 0) {
    permessi <- permessi |>
      rename(causale = display_name) |>
      mutate(
        intervallo_spezzato = map(intervallo_permesso, \(int) {
          giorni |>
            mutate(
              intervalli_temporali = intersect(intervalli_temporali, int)
            ) |>
            drop_na()
        })
      ) |>
      select(causale, intervallo_spezzato) |>
      unnest(intervallo_spezzato) |>
      mutate(
        durata = int_length(intervalli_temporali)
      ) |>
      filter(durata > 0) |> # per come è costruito il codice compaiono intervalli nulli nei boundary del permesso. Li filtro via
      mutate(
        durata = durata / 3600
      ) |>
      pivot_wider(id_cols = giorno, names_from = causale, values_from = durata)
  } else {
    permessi <- tibble(giorno = ymd())
  }

  pianificato_tab <- giorni |>
    left_join(pianificato, join_by(giorno)) |>
    left_join(permessi, join_by(giorno)) |>
    mutate(
      across(!giorno:pianificato, ~ pmin(., pianificato))
    ) |>
    rename(Pianificato = pianificato) |>
    select(-intervalli_temporali) |>
    pivot_longer(-giorno, names_to = "causale", values_to = "durata") |>
    mutate(
      giorno = str_c(
        wday(giorno, label = TRUE),
        " ",
        day(giorno)
      )
    ) |>
    pivot_wider(id_cols = causale, values_from = durata, names_from = giorno)

  theme <- bs_theme()

  pianificato_tab |>
    gt(rowname_col = "causale") |>
    fmt_duration(input_units = "hours") |>
    sub_missing(missing_text = "") |>
    tab_style_body(
      list(
        cell_fill(bs_get_variables(theme, "primary")),
        cell_text(bs_get_contrast(theme, "primary"))
      ),
      rows = 1,
      fn = \(x) !is.na(x)
    ) |>
    tab_style_body(
      list(
        cell_fill(bs_get_variables(theme, "secondary")),
        cell_text(bs_get_contrast(theme, "secondary"))
      ),
      rows = causale != "Pianificato",
      fn = \(x) !is.na(x)
    ) |>
    opt_stylize()
}
