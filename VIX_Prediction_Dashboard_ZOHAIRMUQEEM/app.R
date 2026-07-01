library(shiny)
library(bslib)
library(plotly)
library(glue)
library(vars)
library(tidyverse)
library(bsicons)

#Load Models and Data
source("pipeline.R")
df = API_data_extraction()
ols_model = standard_OLS_model(df)
var_model = VAR_model(df)
deltas = delta_calc(df)
delta_model = delta_OLS_model(deltas)
var_forecast = VARFORECAST(var_model)
latest = tail(df, 1)
summary(var_model)

#UI
ui <- page_navbar(
  title = tags$span("FRED MACRO RISK DASHBOARD"),
  theme = bs_theme(
    bootswatch   = "lux",
    base_font    = font_google("IBM Plex Sans"),
    heading_font = font_google("IBM Plex Sans"),
    bg = "#FFF",
    fg = "#000"
  ),

#Tab 1: Market Monitor
  nav_panel(
    "Current Market Monitor",
    icon = bs_icon("clipboard2-pulse", size = "1.5em"),

    div(
      style = "padding: 1.5rem 1rem 0 1rem;",
      p(style = "font-size: 11px; color: #888; margin-bottom: 8px;",
        glue("Lastest FRED Trading Update: {format(max(df$date),
             '%B %d, %Y')}")
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title    = "VIX — Stock Volatility Index",
          value    = textOutput("latest_vix"),
          showcase = bs_icon("graph-up-arrow", style = "color: #000;"),
          theme    = value_box_theme(bg = "#F8F9FA", fg = "#343A40"),
          style    = "border: 1px solid #F3EBE6; border-top: 4px solid #E9D2C4;text-align: center;"
        ),
        value_box(
          title    = "Federal Funds Rate",
          value    = textOutput("latest_ffr"),
          showcase = bs_icon("bank", style = "color: #000;"),
          theme    = value_box_theme(bg = "#F8F9FA", fg = "#343A40"),
          style    = "border: 1px solid #E3DACF; border-top: 4px solid #C69C6D;text-align: center;"

        ),
        value_box(
          title    = "Yield Curve Spread",
          value    = textOutput("latest_ycs"),
          showcase = bs_icon("arrow-left-right", style = "color: #000;"),
          theme    = value_box_theme(bg = "#F8F9FA", fg = "#343A40"),
          style    = "border: 1px solid #DFCDBC ; border-top: 4px solid #B87333;text-align: center;"
        ),
        value_box(
          title    = "High Yield Spread",
          value    = textOutput("latest_hys"),
          showcase = bs_icon("bar-chart-steps", style = "color: #000;"),
          theme    = value_box_theme(bg = "#F8F9FA", fg = "#343A40"),
          style    = "border: 1px solid #A0A0A0; border-top: 4px solid #3D2314;text-align: center;"

        )
      ),

      br(),
      card(
        card_header("Economic Indicators Over Time"),
        div(
          style = "padding: 0 15px;",
          sliderInput(
            "date_range", NULL,
            min   = as.Date("2023-01-01"),
            max   = Sys.Date(),
            value = c(as.Date("2023-01-01"), Sys.Date()),
            width = "100%"
          ),
          plotlyOutput("time_series_plot", height = "400px")
        )),
      br(),
      card(
        card_header("VIX 3 Month Velocity Window/Δ"),
        div(
          style = "display: flex; justify-content: center; width: 100%;",
          imageOutput("ridge_delta_gif" , height = "500px", width = "800px")
        )
      )
    )
  ),

#Tab 2: VIX Explorer
  nav_panel(
    "VIX Explorer",
    icon = bsicons::bs_icon("search"),

    div(
      style = "padding: 1.5rem 1rem 0 1rem;",
      layout_sidebar(
        sidebar = sidebar(
          width = 260,
          selectInput(
            "indicator",
            "Compare VIX against:",
            choices = c(
              "Yield Curve Spread" = "yield_curve_spread",
              "High Yield Spread"  = "high_yield_spread",
              "Federal Funds Rate" = "fed_funds_rate"
            )
          ),
          hr(),
          p(style = "font-size: 12px; color: #666;",
            "Points color-coded by date where darker = earlier and light = recent observations.
            Blue LOESS curve represents nonlinear variable relationships.
            Pearson Correlation computed across all observations")
        ),
        card(
          card_header(textOutput("explorer_title")),
          plotlyOutput("scatter_plot", height = "420px"), #renders.R
          card_footer(textOutput("correlationdisplay"))
        )
      )
    )
  ),

#Tab 3: VIX Calculator
  nav_panel(
    "VIX Calculator",
    icon = bs_icon("calculator"),

    div(
      style = "padding: 1.5rem 1rem 0 1rem;",
      layout_sidebar(
        sidebar = sidebar(
          width = 200,
          h6("Set Market Conditions"),
          hr(),
          sliderInput("ffr", "Federal Funds Rate (%)",
                      min = 2, max = 5,
                      value = round(latest$fed_funds_rate, 2),
                      step = 0.25),
          sliderInput("ycs", "Yield Curve Spread (%)",
                      min = -5, max = 15,
                      value = round(latest$yield_curve_spread, 2),
                      step = 0.25),
          sliderInput("hys", "High Yield Spread (%)",
                      min = -5, max = 15,
                      value = round(latest$high_yield_spread, 2),
                      step = 0.25),
          hr(),
          p(style = "font-size: 11px; color: #666;",
            "Sliders are initialized to most recent FRED API observed indicators.
            OLS and VAR prediction reacts to slider inputs.
            30-Day VAR Forecast utilzes 3 lags, includes 95% Prediction Interval. ")
        ),

        card(
          card_header("30-Day VAR Forecast - VIX"),
          plotlyOutput("varforecastplot", height = "320px"),
          card_footer(
            tags$small(style = "color: #888; font-size: 0.75rem;",
                       "*Shaded band represents 95% Prediction Interval.
                       High Yield Spread appaerent in Granger Causality
                       test for VIX (p = 5.86e-9) while Federal Funds Rate
                       and Yield do not (p = 0.33).")
          )
        ),

        layout_columns(
          col_widths = c(4, 4, 4), fill = FALSE,

          card(
            card_header(
              tags$span(bs_icon("dash-circle"), "Contemporaneous OLS Regression"),
              style = "padding: 1rem; background: #F3EBE6;text-align: center; "
            ),
            div(
              style = "padding: 1rem;",
              div(style = "font-size: 1rem; font-weight: 700; color: #1a1a2e;",
                  textOutput("olspredval")),
              div(style = "font-size: 1rem; color: #666; margin-top: 4px;",
                  textOutput("olspredci")),
              hr(),
              tags$small(style = "font-size: 1rem; color: #888;", HTML("R&sup2;: 0.4981")),
              tags$small(style = "font-size: 1rem; color: #888;", "Poor fit due to autocorrelation and nonlinear indicator relationships")

            )
          ),

          card(
            card_header(
              tags$span(bs_icon("slash-circle"), "First-Difference 3MΔ OLS Regression"),
              style = "padding: 1rem; background: #E3DACF;text-align: center; "
            ),
            div(
              style = "padding: 1rem;",
              div(style = "font-size: 1rem; font-weight: 700; color: #1a1a2e;",
                  textOutput("dolspredval")),
              div(style = "font-size: 1rem; color: #666; margin-top: 4px;",
                  textOutput("dolspredci")),
              hr(),
              tags$small(style = "font-size: 1rem; color: #888;", HTML("R&sup2;: 0.4346")),
              tags$small(style = "font-size: 1rem; color: #888;", "Poor fit due to autocorrelation and nonlinear indicator relationships")

            )
          ),

          card(
            card_header(
              tags$span(bs_icon("check-circle"), "VAR (Vector Auto Regression)"),
              style = "padding: 1rem; background: #DFCDBC;text-align: center; "
            ),
            div(
              style = "padding: 1rem;",
              div(style = "font-size: 1rem; font-weight: 700; color: #1a1a2e;",
                  textOutput("varpredval")),
              div(style = "font-size: 1rem; color: #666; margin-top: 4px;",
                  textOutput("varpredci")),
              hr(),
              tags$small(style = "font-size: 1rem; color: #888;", HTML("R&sup2;: 0.8411")),
              tags$small(style = "font-size: 1rem; color: #888;", "Improved performance from 3 lags and recognizing path-dependent VIX trend")

            )
          )
        )
      )
    )
  ),

#Github and Markdown
  nav_spacer(),
  nav_item(
    tags$a(
      bs_icon("github"), " GitHub",
      href   = "github.com/zohairmuqeem-projects",
      target = "_blank",
      style  = "color: #666; font-size: 14px;"
    )
  ),
    nav_item(
      tags$a(
        bs_icon("info-circle"), "Methodology",
        href = "FREDR_Data_Pipeline_Sample_ZOHAIR_MUQEEM.html",
        target = "_blank",
        style  = "color: #666; font-size: 14px;"
    )
  )
)
#Server
server <- function(input, output, session) {
  observeEvent(input$reset_button, {
    min_data_date <- min(as.Date(df$date), na.rm = TRUE)
    max_data_date <- max(as.Date(df$date), na.rm = TRUE)
    updateSliderInput(
      session,
      inputId = "date_range",
      value   = c(as.Date("2000-01-01"), Sys.Date())
    )
  })

#Server Tab 1: Market Monitor
  output$latest_vix <- renderText({
    as.character(round(latest$vix, 2))
  })
  output$latest_ffr <- renderText({
    paste0(round(latest$fed_funds_rate, 2), "%")
  })
  output$latest_ycs <- renderText({
    paste0(round(latest$yield_curve_spread, 2), "%")
  })
  output$latest_hys <- renderText({
    paste0(round(latest$high_yield_spread, 2), "%")
  })

  output$time_series_plot <- renderPlotly({


    filtered_df = df %>%
      filter(date >= input$date_range[1], date <= input$date_range[2]) %>%
      pivot_longer(-date, values_to = "values" , names_to = "indicators")

    plot_ly(filtered_df,
            x      = ~date,
            y      = ~values,
            color  = ~indicators,
            type   = "scatter",
            mode   = "lines",
            colors = c(
              "vix"                = "#E9D2C4",
              "fed_funds_rate"     = "#C69C6D",
              "yield_curve_spread" = "#B87333",
              "high_yield_spread"  = "#3D2314"
            )) %>%
      layout(
        hovermode     = "x unified",
        legend        = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.4),
        yaxis         = list(title = "Values"),
        xaxis         = list(title = "", automargin = TRUE)
      )
  })

output$ridge_delta_gif = renderImage({
  list(src = "www/macro_momentum_ridges.gif",
  contentType = "image/gif",
  width= 800,
  height = 500)
  }, deleteFile = FALSE)

#Server Tab 2: VIX Explorer
  output$explorer_title <- renderText({
    glue("VIX vs {input$indicator}")
  })

  output$scatter_plot <- renderPlotly({
    xval   <- df[[input$indicator]]
    numdate <- as.numeric(df$date)
    df$yearfac <- as.factor(format(as.Date(df$date), "%Y"))
    lo       <- loess(df$vix ~ xval, span = 0.4)
    xsort <- sort(xval)
    ypred <- predict(lo, newdata = xsort)
    year_color <- c(
      "2022" = "#E9D2C9", "2023" = "#DEBBA6", "2024" = "#C69C6D",
      "2025" = "#B87333", "2026" = "#3D2314", "2027" = "#000000"
    )

    plot_ly(data = df, x = ~xval, y = ~vix,
            color = ~yearfac, colors = year_color) %>%
      add_trace(type = "scatter",mode = "markers",
        marker = list(
          size       = 4,
          opacity    = 0.80
        ),
        name = ~yearfac,
        showlegend = TRUE
      ) %>%
      add_trace(
        x    = xsort,
        y    = ypred,
        type = "scatter",
        mode = "lines",
        line = list(color = "#191970", width = 2.5),
        name = "LOESS trend",
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        xaxis         = list(title = input$indicator),
        yaxis         = list(title = "VIX"),
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        legend        = list(orientation = "v", x = 1)
      )
  })

  output$correlationdisplay <- renderText({
    corval <- cor(df[[input$indicator]], df$vix, use = "complete.obs")
    paste0("Pearson Correlation with VIX: ", round(corval, 3))
  })

#Server Tab 3: VIX Calculator
  output$dolspredval <- renderText({
    prev_data = df[nrow(df) - 63, ]
    fed_fund_rate_d = input$ffr - prev_data$fed_funds_rate
    yield_curve_spread_d = input$ycs - prev_data$yield_curve_spread
    high_yield_spread_d = input$hys - prev_data$high_yield_spread

    new_data_d = data.frame(
      fed_funds_rate_l     = fed_fund_rate_d,
      yield_curve_spread_l = yield_curve_spread_d,
      high_yield_spread_l  = high_yield_spread_d
    )
    dpred <- predict(delta_model, newdata = new_data_d, interval = "prediction")
    paste0("Predicted VIX Value: ", as.character(round(dpred[1], 2)))
  })

  output$dolspredci <- renderText({
    prev_data = df[nrow(df) - 63, ]
    fed_fund_rate_d = input$ffr - prev_data$fed_funds_rate
    yield_curve_spread_d = input$ycs - prev_data$yield_curve_spread
    high_yield_spread_d = input$hys - prev_data$high_yield_spread

    new_data_d = data.frame(
      fed_funds_rate_l     = fed_fund_rate_d,
      yield_curve_spread_l = yield_curve_spread_d,
      high_yield_spread_l  = high_yield_spread_d
    )
    pred <- predict(delta_model, newdata = new_data_d, interval = "prediction")
    paste0("95% PI: (", round(pred[2], 2), ", ", round(pred[3], 2), ")")
  })

  output$olspredval <- renderText({
    new_data <- data.frame(
      fed_funds_rate     = input$ffr,
      yield_curve_spread = input$ycs,
      high_yield_spread  = input$hys
    )
    pred <- predict(ols_model, newdata = new_data, interval = "prediction")
    paste0("Predicted VIX Value: ", as.character(round(pred[1], 2)))
  })

  output$olspredci <- renderText({
    new_data <- data.frame(
      fed_funds_rate     = input$ffr,
      yield_curve_spread = input$ycs,
      high_yield_spread  = input$hys
    )
    pred <- predict(ols_model, newdata = new_data, interval = "prediction")
    paste0("95% PI: (", round(pred[2], 2), ", ", round(pred[3], 2), ")")
  })

  output$varpredval <- renderText({
    pred     <- predict(var_model, n.ahead = 1)
    vix_pred <- pred$fcst$vix[1, "fcst"]
    paste0("Predicted VIX Value: ", as.character(round(vix_pred[1], 2)))
  })

  output$varpredci <- renderText({
    pred      <- predict(var_model, n.ahead = 1)
    vixlower <- pred$fcst$vix[1, "lower"]
    vixupper <- pred$fcst$vix[1, "upper"]
    paste0("95% PI: (", round(vixlower, 2), ", ", round(vixupper, 2), ")")
  })

  output$varforecastplot <- renderPlotly({
    forecastvix  = var_forecast$fcst$vix
    n            = nrow(forecastvix)
    last_date    = df %>% pull(date) %>% tail(1)
    futuredate = seq(last_date + 1, by = "day", length.out = n)

    plot_ly(data = df) %>%
      add_trace(
        x    = ~tail(date, 60),
        y    = ~tail(vix, 60),
        type = "scatter",
        mode = "lines",
        line = list(color = "#B87333", width = 2),
        name = "Historical VIX"
      ) %>%
      add_trace(
        x    = futuredate,
        y    = forecastvix[, "fcst"],
        type = "scatter",
        mode = "lines",
        line = list(color = "#B87333", width = 2, dash = "dash"),
        name = "VAR Forecast"
      ) %>%
      add_trace(
        x          = c(futuredate, rev(futuredate)),
        y          = c(forecastvix[, "upper"], rev(forecastvix[, "lower"])),
        type       = "scatter",
        mode       = "none",
        fill       = "toself",
        fillcolor  = "rgba(247, 242, 232, 0.7)",
        name       = "95% Prediction Interval",
        showlegend = TRUE
      ) %>%
      layout(
        xaxis         = list(title = ""),
        yaxis         = list(title = "VIX"),
        hovermode     = "x unified",
        legend        = list(orientation = "h", y = -0.2),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })
}

#Run
shinyApp(ui = ui, server = server)


