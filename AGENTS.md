# App Timbrature — Memoria progetto

## Panoramica

App Shiny per la gestione delle timbrature dei dipendenti UPIPA. Autenticazione tramite Azure AD (OAuth2). Deployment su Posit Connect Cloud.

## Stack

- **Frontend**: Shiny + bslib
- **Database**: Azure SQL (`IndicareSaluteLab`) via `pool` + `odbc`
- **Auth**: Azure AD via `AzureAuth` + `AzureGraph`
- **Logging**: `log4r`
- **R**: 4.5.2, gestione pacchetti con `renv`

## Struttura file

```
App timbrature/
├── ui.R
├── server.R
├── global.R
├── config.yml
├── R/
│   ├── db_timbrature_pool.R   # Pool connessione DB (usa env var SERVICE_USERID, PASSWORD)
│   ├── banca_ore.R            # Calcolo banca ore (logica principale)
│   ├── turniPanel.R           # Modulo turni (UI + Server)
│   ├── trasfertePanel.R       # Modulo trasferte (UI + Server)
│   ├── pianificazione.R       # Generatore vista pianificazione
│   ├── daily_summarise.R      # Helper aggregazione giornaliera
│   ├── filter_user.R          # Helper filtro per utente
│   ├── split_permessi.R       # Splitting permessi multi-giorno
│   ├── poll.R                 # Wrapper reactivePoll
│   ├── joins-check.R          # Join con logging
│   └── mese_numerico.R        # Converter mese → numero
├── tests/
│   └── testthat/
│       ├── setup.R            # Fixture, factory functions, source R/
│       └── test-banca_ore.R   # 11 test unitari per banca_ore()
```

## Dipendenze globali rilevanti

- `utenti`: tibble caricata staticamente all'avvio da `global.R`. Usata da `filter_user()` e direttamente in `banca_ore()`. Aggiornamento manuale (gli utenti cambiano raramente).
- `log`: oggetto `log4r` usato da `joins-check.R`.
- `pool`: pool DBI creato da `db_timbrature_pool()`.
- `refresh_time`: intervallo poll in ms (definito in `global.R`).

## Pattern poll

Il wrapper `poll()` in `R/poll.R` accetta:
- `check_col`: colonna usata nella checkFunc (default `last_modified_date_time`, bare name)
- `check_fn`: funzione di aggregazione (default `max`)

Le tabelle `trasferte` e `percorsi` **non hanno** `last_modified_date_time` — usano `check_col = id, check_fn = sum`. Le trasferte sono immutabili (solo insert/delete).

`pause_poll` è un caso speciale: controlla `timbrature` ma carica `pause` — rimane come `reactivePoll` esplicito.

## Test unitari

Eseguire con:
```r
testthat::test_dir("tests/testthat/")
```

`setup.R` usa `rprojroot::find_root(rprojroot::has_file(".Rprofile"))` per trovare la project root e sourciare `R/` indipendentemente dal working directory.

**Ricorda**: valutare sempre se aggiungere o modificare test man mano che si sviluppa l'app. Ogni nuova funzione o modifica alla logica di `banca_ore()` dovrebbe riflettersi nei test.

## Note sviluppo

- `anno_inizio_banca_ore <- 2026` è definita solo dentro `banca_ore()`. La definizione in `global.R` era dead code ed è stata rimossa.
- Riga 76 di `banca_ore.R` (`permessi` da sola) è dead code — da rimuovere.
- Shiny carica automaticamente tutte le funzioni in `R/` tramite `runApp()` — non servono `source()` espliciti in `global.R`.
- `deploy.R` è obsoleto e può essere cancellato.
