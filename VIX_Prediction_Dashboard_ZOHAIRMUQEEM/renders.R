# Ridge Graph Renders
source("pipeline.R")

df = API_data_extraction()
delta_df = delta_calc(df)
delta_df_fix = delta_df %>%
  select(date, yield_curve_spread_l, high_yield_spread_l, fed_funds_rate_l) %>%
  pivot_longer(cols = c(yield_curve_spread_l, high_yield_spread_l, fed_funds_rate_l),
               names_to = "indicator",
               values_to = "slope_values") %>%
  filter(is.finite(slope_values)) %>%
  mutate(
    indicator = case_when(
      indicator == "yield_curve_spread_l" ~ "Yield Curve Spread (3M Δ)",
      indicator == "high_yield_spread_l"  ~ "High Yield Spread (3M Δ)",
      indicator == "fed_funds_rate_l"    ~ "Fed Funds Rate (3M Δ)"
    ),
    display_month = format(date, "%B %Y")
  )
render = ggplot(delta_df_fix , aes(x = slope_values, y = indicator, fill = indicator)) +
  geom_density_ridges(alpha = 0.9, scale = 0.8,
                      color = "gray", show.legend = FALSE,
                      bandwidth = 0.1, rel_min_height = 0.01) +
  theme_minimal() +
  scale_fill_manual(values  = c(
    "Yield Curve Spread (3M Δ)" = "#B87333",
    "High Yield Spread (3M Δ)"  = "#3D2314",
    "Fed Funds Rate (3M Δ)"     = "#C69C6D"
  )) +
  labs(
    title = "Macroeconomic Slope Density Distribution",
    subtitle = "Timeline Horizon: {closest_state}",
    x = "3 Month Velocity Window/Δ",
    y = "",
    caption = "St.Louis FRED API Economic Indicators"
  ) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme(plot.title = element_text(face = "bold", size = 16),
        panel.grid.major.y = element_blank(), plot.margin = margin(10, 40, 10, 40),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
  ) +
  transition_states(display_month, transition_length = 0, state_length = 1) +
  ease_aes('linear')
render_gif <- animate(
  render,
  nframes = 100,
  fps = 3,
  width = 800,
  height = 500,
  renderer = gifski_renderer()
)
render_gif
#anim_save("www/macro_momentum_ridges.gif", animation = render_gif)
