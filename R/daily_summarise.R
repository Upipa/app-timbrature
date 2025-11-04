#' Riepilogo giornaliero
#'
#' Helper function per raggruppare un data frame per giorno e calcolare una statistica riassuntiva.
#'
#' @param df oggetto tbl su cui eseguire il raggruppamento
#' @param date_time_var variabile datetime da cui estrarre il giorno
#' @param stat_var nome assegnato alla statistica nel riepilogo
#' @param expr espressione utilizzata per valutare la statistica
#' @param until_today booleano regola se filtrare fino al giorno corrente. Default su TRUE.
#'
#' @return un tbl con colonne giorno e il valore assegnato a stat_var

daily_summarise <- function(
  df,
  date_time_var,
  stat_var,
  expr,
  until_today = TRUE
) {
  df |>
    collect() |>
    mutate(
      giorno = date({{ date_time_var }}),
      {{ stat_var }} := {{ expr }}
    ) |>
    filter(giorno <= today() | !until_today) |>
    drop_na({{ stat_var }}) |>
    group_by(giorno) |>
    summarise({{ stat_var }} := sum({{ stat_var }}))
}
