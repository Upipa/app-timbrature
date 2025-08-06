#' CPool al database delle timbrature
#' 
#' Wrapper attorno a dbPool con campi impostati di default per connettersi al database delle timbrature.
#' 
#' @param user character con il nome dell'utente d'accesso. Recuperato di default dalla variabile d'ambiente USERID
#' @param pw password character dell'utente con cui si fa l'accesso. Recuperato di default dalla variabile d'ambiente PASSWORD
#' @param ... altri parametri passati a dbConnect
#' @param driver character col nome del driver con cui accedere al database tramite odbc. Default su 'ODBC Driver 18 for SQL Server'. 
#'               Attenzione, il driver va correttamente installato sulla macchina in cui viene eseguito il codice
#' 
#' @return Una connessione DBI

library(pool)
library(odbc)

db_timbrature_pool <- function(
  user = config::get("service_userid"),
  pw = config::get("password"),
  ...,
  driver = config::get("driver")
  ) {
  
  dbPool(
    odbc(),
    driver = driver,
    server = "upipa-acs.database.windows.net",
    database = "IndicareSaluteLab",
    uid = user,
    pwd = pw,
    ...
  )
  
}