#' Table poll constructor
#'
#' Piccola helper function per creare reactivePoll di tabelle del database
#' @param src pool o connessione DBI alla tabella
#' @param table_name nome della tabella di cui creare il poll
#' @param intervalMillis numero approssimativo di millisecondi di attesa tra i refresh
#' @param session sessione shiny di riferimento
#'
#' @return un reactivePoll che verifica ogni `intervalMillis` se ci sono nuovi record nella tabella e in caso
#'         aggiorna il reactive

poll <- function(src, table_name, intervalMillis, session) {
  reactivePoll(
    intervalMillis,
    session,
    checkFunc = function() {
      tbl(pool, table_name) |>
        summarise(
          last_modified_date_time = max(last_modified_date_time)
        ) |>
        collect()
    },
    valueFunc = function() {
      tbl(pool, table_name) |>
        collect()
    }
  )
}
