#' Table poll constructor
#'
#' Piccola helper function per creare reactivePoll di tabelle del database
#' @param src pool o connessione DBI alla tabella
#' @param table_name nome della tabella di cui creare il poll
#' @param intervalMillis numero approssimativo di millisecondi di attesa tra i refresh
#' @param session sessione shiny di riferimento
#' @param check_col nome nudo della colonna usata nella checkFunc. Default `last_modified_date_time`
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
  # check_col viene catturata come quosure qui in poll(), prima che checkFunc
  # venga eseguita. Serve per iniettare il nome della colonna e la funzione
  # direttamente nell'espressione tramite inject(), così dbplyr traduce
  # correttamente in SQL (es. MAX("last_modified_date_time")) invece di
  # passare il nome della variabile R letteralmente.
  check_col_quo <- enquo(check_col)

  reactivePoll(
    intervalMillis,
    session,
    checkFunc = function() {
      rlang::inject(
        tbl(pool, table_name) |>
          summarise(check = (!!check_fn)(!!check_col_quo)) |>
          collect()
      )
    },
    valueFunc = function() {
      tbl(pool, table_name) |>
        collect()
    }
  )
}
