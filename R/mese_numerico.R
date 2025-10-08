#' Restituisce il mese in formato numerico
#'
#' Dato un mese character in formato abbreviato restituisce il numero corrispondente.
#' E' una semplicissima helper function per assicurarmi di seguire lo stesso principio ovunque
#'
#' @param mese_label_abbr mese abbreviato es. gen, feb, mar
#' @return numerico rappresentante il mese

mese_numerico <- function(mese_label_abbr) {
  which(mese_choices == mese_label_abbr)
}
