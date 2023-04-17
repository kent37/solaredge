# Visualization

#' 15-minute power (watts) for a month, by day
#' @param start_date A date within the desired month
#' @export
power_chart = function(start_date=today()) {
  start_date = floor_date(make_date(start_date), 'month')
  power = power_table() 
  daily = power |> 
    filter(date >= start_date) |> 
    collect() |> 
    mutate(time=hms::hms(as.numeric(time)))
    
  energy = energy_table()
  energy = energy |> 
    filter(year==year(start_date), month==month(start_date)) |> 
    collect() |> 
    mutate(kwh = value / 1000,
           label=str_glue('{round(kwh, 1)} KWh'))
  
  total_energy = sum(energy$kwh)
  # ggplot(daily, aes(time, day, height=value, group=day)) +
  #   geom_density_ridges(stat = "identity", scale = 1.5) +
  #   theme_minimal()
  
  ggplot(daily, aes(time, value/1000)) +
    geom_line(linewidth=0.4, aes(group=day)) +
    geom_line(data=daily |> rename(group=day), aes(group=group),
              linewidth=0.1, color='lightgrey') +
    geom_text(data=energy, aes(label=label), x=0, y=6, 
              hjust=-0.1, vjust=1.2) +
    scale_x_time(breaks=scales::date_breaks('6 hours'),
                 labels = hour) +
    scale_y_continuous(minor_breaks=NULL) +
    labs(x='Time of day', y='KW',
         title=str_glue('Daily power generation (KW) ',
         '{format(start_date, "%B %Y")}'),
         subtitle=str_glue('Total energy generation: {round(total_energy, 1)} KWh')) +
    facet_wrap(~day) +
    theme_minimal()
}

#' Daily energy (KWh) for the year, by month
#' @export
energy_chart = function() {
  energy = energy_table()

  monthly = energy |> 
    collect()
  
  ggplot(monthly, aes(day, month, height=value/1000, group=month))  +
    geom_density_ridges(stat = "identity", scale = 1.5) +
    labs(x='Day of month', y='KWh',
         title=str_glue('Daily energy generation (KWh) ')) +
    theme_minimal()
}
