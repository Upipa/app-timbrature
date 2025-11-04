#' Filtra per nome utente
#'
#' Helper function per sempliricare il filtraggio tramite nome utente dei dati di una tabella.
#'
#' @param df tbl_sql contenente la chiave user_id
#' @param .display_name nome cognome dell'utente per cui filtrare
#'
#' @return tbl_sql joinato con la tabella utenti e filtrato per l'utente in input

filter_user <- function(df, .display_name) {
  df |>
    left_join_check(
      tbl(pool, "utenti"),
      join_by(user_id),
      suffix = c("", ".y")
    ) |>
    filter(display_name == .display_name)
}
