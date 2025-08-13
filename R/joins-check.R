#' Mutating joins wrapper
#'
#' Wrapper delle join con integrato il logging in caso di numero in atteso di record nel risultato della join.
#' Per una documentazione più completa vedere documentazione delle join originali.

left_join_check <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  nrow_before <- x |>
    count() |>
    pull(n)

  res <- left_join(
    x,
    y,
    by = by,
    copy = copy,
    suffix = suffix,
    ...,
    keep = keep
  )

  nrow_after <- res |>
    count() |>
    pull(n)

  if (nrow_after < nrow_before) {
    warn(log, "Numero di record nella left join diverso dall'attesso")
  }

  res
}

full_join_check <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  nrow_x <- x |>
    count() |>
    pull(n)

  nrow_y <- y |>
    count() |>
    pull(n)

  res <- full_join(
    x,
    y,
    by = by,
    copy = copy,
    suffix = suffix,
    ...,
    keep = keep
  )

  nrow_after <- res |>
    count() |>
    pull(n)

  if (nrow_after < max(c(nrow_x, nrow_y))) {
    warn(log, "Numero di record nella full join diverso dall'attesso")
  }

  res
}
