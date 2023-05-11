# Visualization

#' 15-minute power (watts) for a month, by day
#' @param start_date A date within the desired month
#' @export
power_chart = function(start_date=today()) {
  start_date = floor_date(make_date(start_date), 'month')
  end_date = ceiling_date(make_date(start_date), 'month')
  power = power_table() 
  daily = power |> 
    filter(date >= start_date, date <= end_date) |> 
    collect() |> 
    mutate(time=hms::hms(as.numeric(time)))
    
  energy = energy_table()
  energy = energy |> 
    filter(year==year(start_date), month==month(start_date)) |> 
    collect() |> 
    mutate(kwh = value / 1000,
           label=str_glue('{round(kwh, 1)} KWh'))
  
  total_energy = sum(energy$kwh)
  days = diff(range(daily$day)) + 1
  daily_average = total_energy / days
  
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
         subtitle=str_glue('Total energy generation: ',
         '{round(total_energy, 0)} KWh, ',
         'daily average {round(daily_average, 0)} KWh, ',
         'monthly estimate {round(30*daily_average, 0)} KWh')) +
    facet_wrap(~day) +
    theme_minimal()
}

#' Daily maximum power
#' @export
daily_max_power_chart = function() {
  power = power_table() 
  daily_max = power |> 
    summarize(.by=c(year, month, day), 
              max_power = max(value),
              date=min(date)) |> 
    collect()

  max_max = max(daily_max$max_power)
  
  ggplot(daily_max, aes(date, max_power)) +
    geom_line() +
    geom_hline(yintercept=max_max, linetype=2, color='gray') +
    labs(x='Date', y='Maximum power (W)',
         title='Daily maximum power generation',
         subtitle=str_glue('Overall maximum {max_max/1000} kW')) +
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
