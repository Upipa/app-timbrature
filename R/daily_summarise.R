#' Riepilogo giornaliero
#'
#' Helper function per raggruppare un data frame per giorno e calcolare una statistica riassuntiva per un dato dipendente.
#'
#' @param df oggetto tbl su cui eseguire il raggruppamento
#' @param .display_name nome cognome del dipendente
#' @param date_time_var variabile datetime da cui estrarre il giorno
#' @param stat_var nome assegnato alla statistica nel riepilogo
#' @param expr espressione utilizzata per valutare la statistica
#'
#' @return un tbl con colonne giorno e il valore assegnato a stat_var

daily_summarise <- function(df, .display_name, date_time_var, stat_var, expr) {
  df |>
    left_join(tbl(pool, "utenti"), join_by(user_id)) |>
    filter(display_name == .display_name) |>
    collect() |>
    mutate(
      giorno = date({{ date_time_var }}),
      {{ stat_var }} := {{ expr }}
    ) |>
    filter(giorno <= today()) |>
    drop_na({{ stat_var }}) |>
    group_by(giorno) |>
    summarise({{ stat_var }} := sum({{ stat_var }}))
}
