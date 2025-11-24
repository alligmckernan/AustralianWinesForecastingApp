# Australian Wines Forecasting App

This Shiny app forecasts monthly Australian wine sales for six varietals (1980–1994) using **TSLM**, **ETS**, and **ARIMA** models.

## Features

- Interactive selection of wine varietals
- Customizable date range and training/validation split
- Forecast horizon selection
- Decomposition plot to explore trends and seasonality
- Accuracy metrics (RMSE, MAE, MAPE) for training and validation
- Faceted plots for comparing multiple varietals

## Deployment

The app is deployed on [Shinyapps.io]](https://alligmckernan.shinyapps.io/assignment5/) and can be accessed via a web browser.

## Usage

1. Select one or more wine varietals
2. Adjust date range and training end date
3. Set forecast horizon
4. Explore different tabs: Overview, Forecasts, Model Specs, Accuracy, About

## Data

The dataset `AustralianWines.csv` includes monthly sales counts for six wine varietals.

## Built With

- R, Shiny, tsibble, fable, fabletools, feasts, tidyverse
