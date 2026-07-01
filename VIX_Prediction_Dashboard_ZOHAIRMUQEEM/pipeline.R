library(fredr)
library(vars)
library(tidyverse)
library(gganimate)
library(ggridges)
fredr_set_key(Sys.getenv("FRED_API_KEY"))

API_data_extraction = function() {
indicators = c(
    'T10Y2Y' = 'yield_curve_spread',
    'BAMLH0A0HYM2' = 'high_yield_spread',
    'DFF' = 'fed_funds_rate',
    'VIXCLS' = 'vix'
  )


data_frames = lapply(names(indicators) , function(ticker){
     fredr(series_id = ticker, observation_start = as.Date('2000-01-01')) %>%
      select(date, value) %>%
      rename(!!indicators[ticker] := value)
  })

merge_df = data_frames %>%
  reduce(inner_join , by = 'date') %>%
  arrange(date) %>%
  drop_na() %>%
  as_tibble()

return(merge_df)
}

delta_calc = function(df) {
  delta_frame = lapply(names(df)[c(2,3,4)] , function(name){
    df %>%
      arrange(date) %>%
      mutate("{name}_l" := (.data[[name]] - lag(.data[[name]], 63))) %>%
      select(date, paste0(name, "_l"))
  })

  delta_df = delta_frame %>%
    reduce(inner_join , by = 'date') %>%
    inner_join(df, by = 'date') %>%
    arrange(date) %>%
    drop_na() %>%
    as_tibble()

  delta_df[is.infinite(as.matrix(delta_df)) | is.nan(as.matrix(delta_df))] = NA
  delta_df = delta_df %>% drop_na()


  return(delta_df)
}

standard_OLS_model = function(df){
  full_model = lm(vix ~ high_yield_spread + yield_curve_spread
                  + fed_funds_rate  , data = df)
  return(full_model)
}

delta_OLS_model = function(deltas){


  delta_full_model = lm(vix ~ high_yield_spread_l + yield_curve_spread_l
                  + fed_funds_rate_l  , data = deltas)
  return(delta_full_model)
}

VAR_model = function(df){var_model = VAR(df %>% arrange(date) %>%
  select(yield_curve_spread , high_yield_spread, fed_funds_rate, vix)
  , p = 3 , type = "const")

  return(var_model)
}

VARFORECAST = function(model){
  return(predict(model, n.ahead=30))
}







