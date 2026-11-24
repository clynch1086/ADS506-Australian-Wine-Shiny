library(shiny)
library(fpp3)

# ---------------------------
# 1. Load and wrangle data
# ---------------------------

wine_wide <- readr::read_csv(
  "data/AustralianWines.csv",
  na = "*",
  show_col_types = FALSE
)

# Remove accidental trailing spaces in column names
names(wine_wide) <- trimws(names(wine_wide))

wine_data <- wine_wide |>
  mutate(
    Month = yearmonth(
      as.Date(paste0("01-", Month), format = "%d-%b-%y")
    )
  ) |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  arrange(Varietal, Month) |>
  group_by(Varietal) |>
  mutate(
    # The source file contains two "*" values for Rose.
    # read_csv() converts those to NA; linearly interpolate them
    # so ETS and other models that require complete data can run.
    Sales = if (anyNA(Sales)) {
      approx(
        x = which(!is.na(Sales)),
        y = Sales[!is.na(Sales)],
        xout = seq_along(Sales),
        rule = 2
      )$y
    } else {
      Sales
    }
  ) |>
  ungroup() |>
  as_tsibble(index = Month, key = Varietal)

min_date <- as.Date(min(wine_data$Month))
max_date <- as.Date(max(wine_data$Month))

all_varietals <- sort(unique(wine_data$Varietal))

# ---------------------------
# 2. User interface
# ---------------------------

ui <- fluidPage(
  titlePanel("Australian Wine Sales Forecast Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "varietals",
        "Varietal(s)",
        choices = all_varietals,
        selected = c("sparkling", "Dry white"),
        multiple = TRUE
      ),

      dateRangeInput(
        "date_range",
        "Analysis date range",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date
      ),

      dateInput(
        "train_end",
        "Training period ends",
        value = as.Date("1992-12-01"),
        min = min_date,
        max = max_date
      ),

      sliderInput(
        "horizon",
        "Future forecast horizon (months)",
        min = 3,
        max = 24,
        value = 12,
        step = 1
      ),

      checkboxGroupInput(
        "display_models",
        "Models to display in forecast plots",
        choices = c("TSLM", "ETS", "ARIMA"),
        selected = c("TSLM", "ETS", "ARIMA")
      ),

      helpText(
        "Tip: keep at least 36 months of training data and 12 months of validation data."
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          h4("Filtered monthly sales"),
          plotOutput("series_plot", height = "420px"),
          h4("Average seasonal profile"),
          plotOutput("season_plot", height = "420px")
        ),

        tabPanel(
          "Models & Accuracy",
          h4("Model specifications"),
          tableOutput("spec_table"),
          br(),
          h4("Training and validation accuracy"),
          tableOutput("accuracy_table"),
          br(),
          h4("Best validation model by RMSE"),
          tableOutput("best_model_table")
        ),

        tabPanel(
          "Forecasts",
          h4("Validation-window comparison"),
          plotOutput("validation_plot", height = "500px"),
          h4("Future comparative forecasts"),
          plotOutput("future_plot", height = "500px")
        ),

        tabPanel(
          "About",
          h3("About this app"),
          p(
            "This app compares forecasting models for monthly Australian wine sales by varietal."
          ),
          tags$ul(
            tags$li("TSLM: linear trend plus monthly seasonality."),
            tags$li("ETS: automatically selected exponential smoothing model."),
            tags$li("ARIMA: automatically selected seasonal/nonseasonal ARIMA model."),
            tags$li("Training and validation accuracy are reported separately."),
            tags$li("Future forecasts include 80% and 95% prediction intervals.")
          ),
          h4("Data preparation"),
          p(
            "The source CSV contains two '*' entries in the Rose series. They are treated as missing values and linearly interpolated so that models requiring complete data can be fit. This preprocessing choice is disclosed for reproducibility."
          ),
          h4("How to reproduce a figure"),
          p(
            "Select the varietal(s), analysis date range, training cutoff date, forecast horizon, and models. Record those settings in the final submission beside each screenshot."
          ),
          h4("Development note"),
          p(
            "OpenAI ChatGPT was used to help draft application boilerplate and explain/debug code. All code and modeling choices were reviewed and are the responsibility of the author."
          )
        )
      )
    )
  )
)

# ---------------------------
# 3. Server
# ---------------------------

server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$varietals, input$date_range)
    
    wine_data |>
      filter(
        Varietal %in% input$varietals,
        as.Date(Month) >= input$date_range[1],
        as.Date(Month) <= input$date_range[2]
      )
  })
  
  split_info <- reactive({
    dat <- filtered_data()
    cutoff <- input$train_end
    
    counts <- dat |>
      as_tibble() |>
      group_by(Varietal) |>
      summarise(
        n_train = sum(as.Date(Month) <= cutoff, na.rm = TRUE),
        n_validation = sum(as.Date(Month) > cutoff, na.rm = TRUE),
        .groups = "drop"
      )
    
    validate(
      need(
        nrow(dat) > 0,
        "No data are available for these selections."
      ),
      need(
        all(counts$n_train >= 36),
        "Choose an earlier start date or a later training cutoff so every selected varietal has at least 36 training months."
      ),
      need(
        all(counts$n_validation >= 12),
        "Choose a training cutoff that leaves at least 12 months for validation."
      )
    )
    
    list(
      cutoff = cutoff,
      counts = counts
    )
  })
  
  training_data <- reactive({
    cutoff <- split_info()$cutoff
    
    filtered_data() |>
      filter(as.Date(Month) <= cutoff)
  })
  
  validation_data <- reactive({
    cutoff <- split_info()$cutoff
    
    filtered_data() |>
      filter(as.Date(Month) > cutoff)
  })

  training_models <- reactive({
    training_data() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })

  validation_forecasts <- reactive({
    validation_index <- validation_data() |>
      select(Varietal, Month)

    training_models() |>
      forecast(new_data = validation_index)
  })

  full_models <- reactive({
    filtered_data() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })

  future_forecasts <- reactive({
    req(input$horizon)

    full_models() |>
      forecast(h = input$horizon)
  })

  model_specs <- reactive({
    training_models() |>
      pivot_longer(
        cols = c(TSLM, ETS, ARIMA),
        names_to = "Model",
        values_to = "Fit"
      ) |>
      mutate(
        Specification = purrr::map_chr(Fit, fabletools::model_sum),
        Specification = if_else(
          Model == "TSLM",
          "TSLM(Sales ~ trend() + season())",
          Specification
        )
      ) |>
      as_tibble() |>
      select(Varietal, Model, Specification)
  })

  training_accuracy <- reactive({
    training_models() |>
      augment() |>
      as_tibble() |>
      group_by(Varietal, .model) |>
      summarise(
        RMSE = sqrt(mean(.resid^2, na.rm = TRUE)),
        MAE = mean(abs(.resid), na.rm = TRUE),
        MAPE = mean(abs(.resid / Sales), na.rm = TRUE) * 100,
        .groups = "drop"
      ) |>
      transmute(
        Varietal,
        Model = .model,
        Window = "Training",
        RMSE,
        MAE,
        MAPE
      )
  })
  
  validation_accuracy <- reactive({
    forecast_values <- validation_forecasts() |>
      as_tibble() |>
      select(
        Varietal,
        Month,
        Model = .model,
        Forecast = .mean
      )
    
    actual_values <- validation_data() |>
      as_tibble() |>
      select(
        Varietal,
        Month,
        Actual = Sales
      )
    
    forecast_values |>
      left_join(
        actual_values,
        by = c("Varietal", "Month")
      ) |>
      group_by(Varietal, Model) |>
      summarise(
        RMSE = sqrt(
          mean((Actual - Forecast)^2, na.rm = TRUE)
        ),
        MAE = mean(
          abs(Actual - Forecast),
          na.rm = TRUE
        ),
        MAPE = mean(
          abs((Actual - Forecast) / Actual),
          na.rm = TRUE
        ) * 100,
        .groups = "drop"
      ) |>
      mutate(
        Window = "Validation",
        .before = RMSE
      )
  })

  all_accuracy <- reactive({
    bind_rows(
      training_accuracy(),
      validation_accuracy()
    ) |>
      mutate(
        across(c(RMSE, MAE, MAPE), ~ round(.x, 2))
      ) |>
      arrange(Varietal, Window, RMSE)
  })

  best_models <- reactive({
    validation_accuracy() |>
      group_by(Varietal) |>
      slice_min(order_by = RMSE, n = 1, with_ties = FALSE) |>
      ungroup() |>
      transmute(
        Varietal,
        Best_Model = Model,
        Validation_RMSE = round(RMSE, 2),
        Validation_MAE = round(MAE, 2),
        Validation_MAPE = round(MAPE, 2)
      )
  })

  output$series_plot <- renderPlot({
    dat <- filtered_data()

    ggplot(dat, aes(x = Month, y = Sales)) +
      geom_line(linewidth = 0.7) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(
        x = NULL,
        y = "Monthly sales",
        title = "Australian wine sales over time"
      ) +
      theme_minimal(base_size = 12)
  })

  output$season_plot <- renderPlot({
    dat <- filtered_data() |>
      as_tibble() |>
      mutate(
        MonthNum = lubridate::month(as.Date(Month))
      ) |>
      group_by(Varietal, MonthNum) |>
      summarise(
        Average_Sales = mean(Sales, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        MonthName = factor(
          month.abb[MonthNum],
          levels = month.abb
        )
      )
    
    ggplot(
      dat,
      aes(
        x = MonthName,
        y = Average_Sales,
        group = 1
      )
    ) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
      facet_wrap(
        vars(Varietal),
        scales = "free_y"
      ) +
      labs(
        x = "Month",
        y = "Average monthly sales",
        title = "Average seasonal sales pattern"
      ) +
      theme_minimal(base_size = 12)
  })

  output$spec_table <- renderTable({
    model_specs()
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$accuracy_table <- renderTable({
    all_accuracy()
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$best_model_table <- renderTable({
    best_models()
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$validation_plot <- renderPlot({
    req(input$display_models)
    
    fc <- validation_forecasts() |>
      filter(.model %in% input$display_models)
    
    # Show the final 36 months of training plus the validation period
    history_start <- input$train_end %m-% months(35)
    
    plot_data <- filtered_data() |>
      filter(as.Date(Month) >= history_start)
    
    fc |>
      autoplot(
        plot_data,
        level = c(80, 95)
      ) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(
        x = NULL,
        y = "Monthly sales",
        title = "Validation forecasts compared with actual sales",
        subtitle = "Last 36 training months plus the held-out validation period",
        colour = "Model"
      ) +
      theme_minimal(base_size = 12)
  })

  output$future_plot <- renderPlot({
    req(input$display_models)
    
    fc <- future_forecasts() |>
      filter(.model %in% input$display_models)
    
    # Show only the most recent 36 months before the forecast
    history_start <- as.Date(max(filtered_data()$Month)) %m-% months(35)
    
    plot_data <- filtered_data() |>
      filter(as.Date(Month) >= history_start)
    
    fc |>
      autoplot(
        plot_data,
        level = c(80, 95)
      ) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(
        x = NULL,
        y = "Monthly sales",
        title = paste0(
          input$horizon,
          "-month comparative forecast"
        ),
        subtitle = "Recent history with 80% and 95% prediction intervals",
        colour = "Model"
      ) +
      theme_minimal(base_size = 12)
  })
}

shinyApp(ui = ui, server = server)
