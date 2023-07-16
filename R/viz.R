# Visualization

#' 15-minute power (watts) for a month, by day
#' @param start_date A date within the desired month
#' @export
power_chart = function(start_date=today()) {
  start_date = floor_date(make_date(start_date), 'month')
  end_date = ceiling_date(make_date(start_date), 'month')
  power = power_table() 
  daily = power |> 
    filter(date >= start_date, date < end_date) |> 
    collect() |> 
    mutate(time=hms::hms(as.numeric(time)))
    
  energy = energy_table()
  energy = energy |> 
    filter(year==year(start_date), month==month(start_date)) |> 
    collect() |> 
    mutate(kwh = value / 1000,
           label=str_glue('{round(kwh, 1)} kWh'))
  
  total_energy = sum(energy$kwh)
  days = diff(range(daily$day)) + 1
  daily_average = total_energy / days
  
  ggplot(daily, aes(time, value/1000)) +
    geom_line(linewidth=0.4, aes(group=day)) +
    # geom_line(data=daily |> rename(group=day), aes(group=group),
    #           linewidth=0.1, color='lightgrey') +
    geom_text(data=energy, aes(label=label), x=0, y=4,
              hjust=-0.1, vjust=1.2) +
    scale_x_time(breaks=scales::date_breaks('6 hours'),
                 labels = hour) +
    scale_y_continuous(minor_breaks=NULL) +
    labs(x='Time of day', y='KW',
         title=str_glue('Daily power generation (kW) ',
         '{format(start_date, "%B %Y")}'),
         subtitle=str_glue('Total energy generation: ',
         '{round(total_energy, 0)} kWh, ',
         'daily average {round(daily_average, 0)} kWh, ',
         'monthly estimate {round(30*daily_average, 0)} kWh')) +
    facet_wrap(~day) +
    theme_minimal()
}

#' Daily maximum power generation
#' @export
daily_max_power_chart = function() {
  power = power_table() 
  daily_max = power |> 
    summarize(.by=c(year, month, day), 
              max_power = max(value),
              date=as.Date(min(date))) |> 
    collect()

  max_max = max(daily_max$max_power)
  
  ggplot(daily_max, aes(date, max_power)) +
    geom_col(aes(fill=max_power>6000), just=0) +
    scale_x_date(minor_breaks=NULL) +
    scale_fill_manual(values=c(`TRUE`='darkred', `FALSE`='grey40'),
                      guide='none') +
    labs(x='Date', y='Maximum power (W)',
         title='Daily maximum power generation',
         subtitle=('Peak days of 6kW in red')) +
    theme_minimal()
}


#' Histogram of daily maximum power generation
#' @export
daily_max_power_histogram = function() {
  power = power_table() 
  daily_max = power |> 
    summarize(.by=c(year, month, day), 
              max_power = max(value),
              date=min(date)) |> 
    collect()

  ggplot(daily_max, aes(max_power)) +
    geom_histogram(binwidth=500, boundary=6000) +
    scale_y_continuous(breaks=~seq(0, .x[2], 5), minor_breaks=NULL) +
    labs(x='Maximum power (W)', y='Number of days',
         title='Daily maximum power generation') +
    facet_wrap(~month, ncol=1, labeller=as_labeller(month.abb |> set_names(1:12))) +
    theme_minimal()
}

#' Daily energy (kWh) for the year
#' @export
daily_energy_chart = function() {
  energy = energy_table()

  daily = energy |> 
    collect()
  
  ggplot(daily, aes(date, value/1000))  +
    geom_col(just=0, aes(fill=value>40000)) +
    scale_x_date(minor_breaks=NULL) +
    scale_fill_manual(values=c(`TRUE`='darkred', `FALSE`='grey40'),
                      guide='none') +
    labs(x='Date', y='kWh',
         title='Daily energy generation (kWh)',
         subtitle='Days with 40kWh or more in red') +
    theme_minimal()
}

#' Histogram of daily energy (kWh) for the year
#' @export
daily_energy_histogram = function() {
  energy = energy_table()

  daily = energy |> 
    collect()
  
  ggplot(daily, aes(value/1000))  +
    geom_histogram(binwidth=5, boundary=0) +
    labs(x='Daily energy generation (kWh)', y='Number of days',
         title=str_glue('Daily energy generation (kWh) ')) +
    facet_wrap(~month, ncol=1, labeller=as_labeller(month.abb |> set_names(1:12))) +
    theme_minimal()
}

#' Estimated daily usage vs generation
#' @export
daily_usage_chart = function() {
  energy = energy_summary() |> 
    slice_tail(n=-1) # First date has no usage, only an end date
  
  ggplot(energy) +
    geom_step(aes(start_date, daily_use, color='Usage')) +
    geom_segment(aes(x=start_date, xend=end_date, y=daily_use, yend=daily_use, color='Usage')) +
    geom_step(aes(start_date, daily_generation, color='Generation')) +
    geom_segment(aes(x=start_date, xend=end_date, y=daily_generation, yend=daily_generation, color='Generation')) +
    scale_x_date(date_breaks='month', date_labels='%b %y', minor_breaks=NULL) +
    scale_color_manual('Average daily power', values=c(Usage='darkred', Generation='darkgreen')) +
    ylim(0, NA) +
    labs(x='End date', y='Average daily use (kWh)',
         title='Average daily energy generation and use (kWh)') +
    theme_minimal()
}