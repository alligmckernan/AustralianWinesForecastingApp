library(shiny)
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(lubridate)
library(feasts)
library(zoo)

# Load & Wrangle
wines_raw <- read_csv("C:/Users/allig/Downloads/AustralianWines.csv")

wines_ts <- wines_raw %>%
  mutate(
    Rose = as.numeric(Rose),
    Month = yearmonth(as.yearmon(Month, "%b-%y"))
  ) %>%
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) %>%
  drop_na(Sales) %>%  # okay because there's only 2 missing values in the Rose column
    as_tsibble(index = Month, key = Varietal)

varietals <- unique(wines_ts$Varietal)

# UI
ui <- fluidPage(
  
  titlePanel("Australian Wines Forecasting App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variety", "Select varietal(s):",
                  choices = varietals,
                  selected = varietals[1],
                  multiple = TRUE),
      
      dateRangeInput("daterange", "Date range:",
                     start = as.Date("1980-01-01"),
                     end = as.Date("1994-12-01"),
                     min = as.Date("1980-01-01"),
                     max = as.Date("1994-12-01"),
                     format = "yyyy-mm"),
      
      sliderInput("train_end",
                  "Training end date:",
                  min = as.Date("1980-01-01"),
                  max = as.Date("1994-12-01"),
                  value = as.Date("1994-12-01"),
                  step = 30),
      
      numericInput("h", "Forecast horizon (months):", value = 12, min = 1),
      
      checkboxInput("show_decomp", "Show decomposition plot", FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview Plot", plotOutput("overviewPlot")),
        tabPanel("Forecasts", plotOutput("fcPlot")),
        tabPanel("Model Specs", tableOutput("modelSpecs")),
        tabPanel("Accuracy", tableOutput("accuracyTbl")),
        tabPanel("About", 
                 h3("Australian Wine Sales Forecasting Dashboard"),
                 br(),
                 h4("📊 Overview"),
                 p("This interactive dashboard analyzes and forecasts monthly Australian wine sales across six wine varietals 
                   from 1980-1994. The app implements three state-of-the-art time series forecasting models to provide 
                   comprehensive sales predictions with uncertainty quantification."),
                 
                 h4("🍷 Wine Varietals"),
                 tags$ul(
                   tags$li("Fortified wines"),
                   tags$li("Red wines"), 
                   tags$li("Rosé wines"),
                   tags$li("Sparkling wines"),
                   tags$li("Sweet white wines"),
                   tags$li("Dry white wines")
                 ),
                 
                 h4("🤖 Forecasting Models"),
                 tags$ul(
                   tags$li(tags$strong("TSLM:"), " Time Series Linear Model with trend and seasonal components"),
                   tags$li(tags$strong("ETS:"), " Error, Trend, Seasonal exponential smoothing with automatic model selection"),
                   tags$li(tags$strong("ARIMA:"), " AutoRegressive Integrated Moving Average with automatic parameter selection")
                 ),
                 
                 h4("📈 Key Features"),
                 tags$ul(
                   tags$li("Interactive varietal selection and date filtering"),
                   tags$li("Customizable training/validation split for model evaluation"),
                   tags$li("Time series decomposition visualization"),
                   tags$li("Multi-step ahead forecasting with prediction intervals"),
                   tags$li("Model accuracy comparison (RMSE, MAE, MAPE)"),
                   tags$li("Faceted forecasting plots for multivariate analysis")
                 ),
                 
                 h4("🔧 How to Use"),
                 tags$ol(
                   tags$li("Select wine varietals of interest from the dropdown"),
                   tags$li("Adjust the date range to focus on specific periods"),
                   tags$li("Move the training end date slider to control the train/validation split"),
                   tags$li("Set forecast horizon for future predictions"),
                   tags$li("Explore different tabs to view forecasts, model specifications, and accuracy metrics")
                 ),
                 
                 h4("💡 Pro Tips"),
                 tags$ul(
                   tags$li("Use the decomposition plot to understand seasonal patterns"),
                   tags$li("Compare accuracy metrics across models to identify the best performer"),
                   tags$li("Adjust training end date to see how models perform on out-of-sample data"),
                   tags$li("Higher forecast horizons increase prediction uncertainty")
                 ),
                 
                 br(),
                 p(em("Built with R Shiny, fable, and tsibble for robust time series analysis and forecasting."))
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Filtered data reactive
  filtered_data <- reactive({
    wines_ts %>%
      filter(
        Varietal %in% input$variety,
        Month >= yearmonth(input$daterange[1]),
        Month <= yearmonth(input$daterange[2])
      )
  })
  
  # Overview Plot
  output$overviewPlot <- renderPlot({
    df <- filtered_data()
    
    req(nrow(df) > 2) 
    
    if (input$show_decomp) {
      df %>%
        model(STL = STL(Sales ~ season(window = "periodic"))) %>%
        components() %>%
        autoplot() +
        labs(title = "Decomposition by Varietal")
    } else {
      df %>%
        autoplot(Sales) +
        labs(title = "Sales over Time")
    }
  })
  
  # Modeling & Forecasts
  model_fit <- reactive({
    
    df <- filtered_data()
    req(nrow(df) > 36)
    
    train_end <- yearmonth(input$train_end)
    
    train <- df %>% filter(Month <= train_end)
    valid <- df %>% filter(Month > train_end)
    
    req(nrow(train) > 36)
    
    mdl <- train %>%
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
    
    list(
      train = train,
      valid = valid,
      mdl = mdl
    )
  })
  
  # Forecast plot
  output$fcPlot <- renderPlot({
    fit <- model_fit()
    mdl <- fit$mdl
    valid <- fit$valid
    df <- filtered_data()
    
    h <- input$h
    
    fc_future <- mdl %>% forecast(h = h) %>% mutate(.type = "Future")
    
    if (nrow(valid) > 0) {
      fc_valid <- mdl %>% forecast(new_data = valid) %>%
        mutate(.type = "Validation")
      fc_all <- bind_rows(fc_future, fc_valid)
    } else {
      fc_all <- fc_future
    }
    
    autoplot(df, Sales) +
      autolayer(fc_all, level = 95) +
      facet_wrap(~Varietal, scales = "free_y") +
      labs(title = "Forecasts with Prediction Intervals")
  })
  
  # Model specifications
  output$modelSpecs <- renderTable({
    fit <- model_fit()
    mdl <- fit$mdl
    
    tryCatch({
      # Use glance() which works on the full mable without pivoting
      glance_results <- mdl %>% glance()
      
      # Extract just the model specifications we need
      specs_df <- glance_results %>%
        select(Varietal, .model) %>%
        mutate(
          Specification = case_when(
            .model == "TSLM" ~ "TSLM: trend() + season()",
            .model == "ETS" ~ {
              # Get ETS specs from glance results
              ets_info <- glance_results %>% filter(.model == "ETS") %>% pull(.model)
              paste("ETS model (auto-selected)")
            },
            .model == "ARIMA" ~ {
              # Get ARIMA specs from glance results  
              arima_info <- glance_results %>% filter(.model == "ARIMA") %>% pull(.model)
              paste("ARIMA model (auto-selected)")
            },
            TRUE ~ "Unknown model"
          )
        ) %>%
        rename(Model = .model)
      
      specs_df
      
    }, error = function(e) {
      # Simple fallback that doesn't use pivot operations
      varietals <- unique(mdl$Varietal)
      models <- c("TSLM", "ETS", "ARIMA")
      
      expand_grid(Varietal = varietals, Model = models) %>%
        mutate(
          Specification = case_when(
            Model == "TSLM" ~ "TSLM: trend() + season()",
            Model == "ETS" ~ "ETS (auto-selected)",
            Model == "ARIMA" ~ "ARIMA (auto-selected)",
            TRUE ~ "Model fitted"
          )
        )
    })
  })
  
  # Accuracy table
  output$accuracyTbl <- renderTable({
    
    fit <- model_fit()
    mdl <- fit$mdl
    train <- fit$train
    valid <- fit$valid
    
    tryCatch({
      
      acc_list <- list()
      models <- c("TSLM", "ETS", "ARIMA")
      
      # Iterate over each Varietal (each row of mdl)
      for (i in seq_len(nrow(mdl))) {
        
        v <- mdl$Varietal[i]
        
        for (m in models) {
          
          # Extract the model object safely from its column
          model_obj <- mdl[[m]][[i]]
          
          # --- Training accuracy ---
          train_v <- train %>% filter(Varietal == v)
          fitted_vals <- fitted(model_obj)
          
          train_acc <- tibble(
            Varietal = v,
            .model   = m,
            Window   = "Training",
            RMSE     = sqrt(mean((train_v$Sales - fitted_vals$.fitted)^2, na.rm = TRUE)),
            MAE      = mean(abs(train_v$Sales - fitted_vals$.fitted), na.rm = TRUE),
            MAPE     = mean(abs((train_v$Sales - fitted_vals$.fitted)/train_v$Sales)*100, na.rm = TRUE)
          )
          
          acc_list[[length(acc_list) + 1]] <- train_acc
          
          # --- Validation accuracy ---
          valid_v <- valid %>% filter(Varietal == v)
          if (nrow(valid_v) > 0) {
            fc <- forecast(model_obj, new_data = valid_v)
            
            valid_acc <- tibble(
              Varietal = v,
              .model   = m,
              Window   = "Validation",
              RMSE     = sqrt(mean((valid_v$Sales - fc$.mean)^2, na.rm = TRUE)),
              MAE      = mean(abs(valid_v$Sales - fc$.mean), na.rm = TRUE),
              MAPE     = mean(abs((valid_v$Sales - fc$.mean)/valid_v$Sales)*100, na.rm = TRUE)
            )
            
            acc_list[[length(acc_list) + 1]] <- valid_acc
          }
          
        } # end model loop
      } # end Varietal loop
      
      # Combine results and round
      acc_combined <- bind_rows(acc_list) %>%
        mutate(
          RMSE = round(RMSE, 2),
          MAE  = round(MAE, 2),
          MAPE = round(MAPE, 2)
        )
      
      acc_combined
      
    }, error = function(e) {
      data.frame(
        Varietal = "Error",
        .model   = "Error",
        Window   = "Error",
        RMSE     = paste("Error:", substr(e$message, 1, 50)),
        MAE      = "",
        MAPE     = "",
        stringsAsFactors = FALSE
      )
    })
  })
  
  
}

shinyApp(ui, server)