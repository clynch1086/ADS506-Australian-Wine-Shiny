# Australian Wine Sales Shiny App — ADS 506 Week 5

## Project structure

- `app.R` — Shiny application
- `analysis_mockup.qmd` — static analysis/mockup used before the Shiny build
- `data/AustralianWines.csv` — source dataset

## Required packages

Install these in R before running the app:

```r
install.packages(c("shiny", "fpp3", "purrr"))
```

`fpp3` loads the tidyverts forecasting packages used by the assignment.

## Run

Open the project folder in RStudio or Positron and run:

```r
shiny::runApp()
```

## Data note

The original CSV contains two `*` values in the Rose series. The app reads `*` as missing and uses linear interpolation for those observations before fitting models. This keeps ETS and other complete-data models from failing.

## AI-use acknowledgment

OpenAI ChatGPT was used to help draft application boilerplate and explain/debug code. All code, modeling choices, validation, and final interpretation were reviewed by the student, who is responsible for the submitted work.
