test_that("tabelle vuote restituisce solo la banca_ore_iniziale convertita in secondi", {
  utenti <<- make_utenti(banca_ore_iniziale = 5)

  result <- banca_ore(
    test_user,
    empty_timbrature(),
    empty_pianificazione(),
    empty_pause(),
    empty_permessi(),
    empty_causali(),
    empty_trasferte()
  )

  expect_equal(result, 5 * 3600)
})

test_that("lavorato uguale a pianificato non modifica la banca_ore_iniziale", {
  utenti <<- make_utenti(banca_ore_iniziale = 3)

  result <- banca_ore(
    test_user,
    make_timbrature(in_time = "08:00:00", out_time = "16:00:00"),  # 8 ore
    make_pianificazione(start_time = "08:00:00", end_time = "16:00:00"),  # 8 ore
    empty_pause(),
    empty_permessi(),
    empty_causali(),
    empty_trasferte()
  )

  expect_equal(result, 3 * 3600)
})

test_that("ore lavorate in eccesso rispetto al pianificato incrementano la banca_ore", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  result <- banca_ore(
    test_user,
    make_timbrature(in_time = "08:00:00", out_time = "17:00:00"),  # 9 ore
    make_pianificazione(start_time = "08:00:00", end_time = "16:00:00"),  # 8 ore
    empty_pause(),
    empty_permessi(),
    empty_causali(),
    empty_trasferte()
  )

  expect_equal(result, 1 * 3600)  # +1 ora
})

test_that("ore lavorate inferiori al pianificato decrementano la banca_ore", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  result <- banca_ore(
    test_user,
    make_timbrature(in_time = "08:00:00", out_time = "15:00:00"),  # 7 ore
    make_pianificazione(start_time = "08:00:00", end_time = "16:00:00"),  # 8 ore
    empty_pause(),
    empty_permessi(),
    empty_causali(),
    empty_trasferte()
  )

  expect_equal(result, -1 * 3600)  # -1 ora
})

test_that("la pausa viene sottratta dall'orario lavorato", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  # 8 ore lordi - 30 min pausa = 7.5 ore netti = pianificato → delta zero
  result <- banca_ore(
    test_user,
    make_timbrature(in_time = "08:00:00", out_time = "16:00:00"),
    make_pianificazione(start_time = "08:00:00", end_time = "15:30:00"),
    make_pause(start_time = "12:00:00", end_time = "12:30:00"),
    empty_permessi(),
    empty_causali(),
    empty_trasferte()
  )

  expect_equal(result, 0)
})

test_that("il permesso con riduce_pianificazione=TRUE scala il monte ore pianificato", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  # lavorato = 4h, pianificato = 8h, permesso = 4h
  # pianificato_meno_permessi = max(8h - 4h, 0) = 4h = lavorato → delta zero
  result <- banca_ore(
    test_user,
    make_timbrature(in_time = "08:00:00", out_time = "12:00:00"),
    make_pianificazione(start_time = "08:00:00", end_time = "16:00:00"),
    empty_pause(),
    make_permessi(start_time = "08:00:00", end_time = "12:00:00"),
    make_causali(riduce_pianificazione = TRUE),
    empty_trasferte()
  )

  expect_equal(result, 0)
})

test_that("il permesso con riduce_pianificazione=FALSE non scala il monte ore pianificato", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  # lavorato = 4h, pianificato = 8h, permesso non riduce
  # pianificato_meno_permessi = 8h, lavorato = 4h → delta = -4h
  result <- banca_ore(
    test_user,
    make_timbrature(in_time = "08:00:00", out_time = "12:00:00"),
    make_pianificazione(start_time = "08:00:00", end_time = "16:00:00"),
    empty_pause(),
    make_permessi(start_time = "08:00:00", end_time = "12:00:00"),
    make_causali(riduce_pianificazione = FALSE),
    empty_trasferte()
  )

  expect_equal(result, -4 * 3600)
})

test_that("i permessi non rendono il pianificato negativo (pmax)", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  # pianificato = 2h, permesso = 8h (supera il pianificato)
  # pianificato_meno_permessi = max(2h - 8h, 0) = 0, lavorato = 0 → delta zero
  result <- banca_ore(
    test_user,
    empty_timbrature(),
    make_pianificazione(start_time = "08:00:00", end_time = "10:00:00"),
    empty_pause(),
    make_permessi(start_time = "08:00:00", end_time = "16:00:00"),
    make_causali(riduce_pianificazione = TRUE),
    empty_trasferte()
  )

  expect_equal(result, 0)
})

test_that("la trasferta aggiunge ore al lavorato", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  # lavorato = 7h + 1h trasferta = 8h = pianificato → delta zero
  result <- banca_ore(
    test_user,
    make_timbrature(in_time = "08:00:00", out_time = "15:00:00"),
    make_pianificazione(start_time = "08:00:00", end_time = "16:00:00"),
    empty_pause(),
    empty_permessi(),
    empty_causali(),
    make_trasferte(tempo = 1, moltiplicatore_t = 1)
  )

  expect_equal(result, 0)
})

test_that("il moltiplicatore_t scala correttamente le ore di trasferta", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  # lavorato = 8h + (1h * 2) trasferta = 10h, pianificato = 8h → delta = +2h
  result <- banca_ore(
    test_user,
    make_timbrature(in_time = "08:00:00", out_time = "16:00:00"),
    make_pianificazione(start_time = "08:00:00", end_time = "16:00:00"),
    empty_pause(),
    empty_permessi(),
    empty_causali(),
    make_trasferte(tempo = 1, moltiplicatore_t = 2)
  )

  expect_equal(result, 2 * 3600)
})

test_that("i dati ante-2026 vengono ignorati", {
  utenti <<- make_utenti(banca_ore_iniziale = 0)

  result <- banca_ore(
    test_user,
    make_timbrature(date = "2025-06-01", in_time = "08:00:00", out_time = "16:00:00"),
    make_pianificazione(date = "2025-06-01", start_time = "08:00:00", end_time = "16:00:00"),
    empty_pause(),
    empty_permessi(),
    empty_causali(),
    empty_trasferte()
  )

  expect_equal(result, 0)
})
