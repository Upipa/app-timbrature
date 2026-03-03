split_permessi <- function(permessi) {
  permessi <- permessi |>
    mutate(
      across(start_date_time:end_date_time, ~ with_tz(., "Europe/Rome")),
      intervallo_permesso = start_date_time %--% end_date_time
    )

  if (nrow(permessi) > 0) {
    min_day <- date(min(permessi$start_date_time))
    max_day <- date(max(permessi$end_date_time))

    giorni <- tibble(
      giorno = as_date(min_day:max_day)
    ) |>
      mutate(
        intervalli_temporali = int_diff(force_tz(
          c(giorno, last(giorno) + days(1)),
          tz = "Europe/Rome"
        ))
      )

    permessi <- permessi |>
      mutate(
        intervallo_spezzato = map(intervallo_permesso, \(int) {
          giorni |>
            mutate(
              intervalli_temporali = intersect(intervalli_temporali, int)
            ) |>
            drop_na()
        })
      ) |>
      select(time_off_reason_id, intervallo_spezzato) |>
      unnest(intervallo_spezzato)
  } else {
    permessi <- tibble(
      time_off_reason_id = character(),
      giorno = ymd(),
      intervalli_temporali = interval()
    )
  }

  permessi
}
