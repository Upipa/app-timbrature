#' Filtra per nome utente
#'
#' Helper function per sempliricare il filtraggio tramite nome utente dei dati di una tabella.
#'
#' @param df tbl_sql contenente la chiave user_id
#' @param .display_name nome cognome dell'utente per cui filtrare
#'
#' @return tbl_sql joinato con la tabella utenti e filtrato per l'utente in input

filter_user <- function(df, .display_name) {
  .user_id <- utenti |>
    filter(display_name == .display_name) |>
    pull(user_id)

  df |>
    filter(user_id == .user_id)
}
