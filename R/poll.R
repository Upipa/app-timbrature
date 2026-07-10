#' Table poll constructor
#'
#' Piccola helper function per creare reactivePoll di tabelle del database
#' @param src pool o connessione DBI alla tabella
#' @param table_name nome della tabella di cui creare il poll
#' @param intervalMillis numero approssimativo di millisecondi di attesa tra i refresh
#' @param session sessione shiny di riferimento
#' @param check_col nome della colonna usata nella checkFunc. Default `"last_modified_date_time"`
#' @param check_fn funzione di aggregazione applicata a `check_col` nella checkFunc. Default `max`
#'
#' @return un reactivePoll che verifica ogni `intervalMillis` se ci sono nuovi record nella tabella e in caso
#'         aggiorna il reactive

poll <- function(
  src,
  table_name,
  intervalMillis,
  session,
  check_col = last_modified_date_time,
  check_fn = max
) {
  reactivePoll(
    intervalMillis,
    session,
    checkFunc = function() {
      tbl(pool, table_name) |>
        summarise(check = check_fn({{ check_col }})) |>
        collect()
    },
    valueFunc = function() {
      tbl(pool, table_name) |>
        collect()
    }
  )
}
