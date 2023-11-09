energy = energy_table() |> 
  group_by(year, month) |> 
  summarize(kWh = sum(value)/1000, .groups='drop') |> 
  collect() |> 
  rename(Month=month) |> 
  filter(!Month %in% c(4, 11))

prediction = monthly_predictions() |> filter(Month %in% energy$Month)

energy |> 
  left_join(prediction, by='Month', suffix=c('.Actual', '.Predicted')) |> 
  mutate(Difference=kWh.Actual-kWh.Predicted,
         Month=month.name[Month],
         'Percent difference'=Difference/kWh.Predicted) |> 
  select(-year) |> 
  gt() |>
  grand_summary_rows(columns=2:4, 
                     fns=list(label='Total') ~ sum(.),
                     fmt=~fmt_number(., decimals=0),
                     missing_text='') |> 
  fmt_number(columns=2:4, decimals=0) |> 
  fmt_percent(columns=5, decimals=0) |> 
  tab_spanner_delim('.') |> 
  tab_header(title='Actual and predicted energy generation', 
    subtitle='1 Aldrich St, 2023')

monthly_energy_chart() +
  scale_x_continuous(breaks=5:10, labels=month.abb[5:10], limits=c(4,11))
