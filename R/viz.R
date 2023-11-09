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
    mutate(time=hms::hms(as.numeric(time))) |> 
    group_by(year, month, day) |> 
    mutate(daily_max = max(value))
    
  energy = energy_table()
  energy = energy |> 
    filter(year==year(start_date), month==month(start_date)) |> 
    collect() |> 
    mutate(kwh = value / 1000,
           label=str_glue('{round(kwh, 1)} kWh'))
  
  total_energy = sum(energy$kwh)
  days = diff(range(daily$day)) + 1
  daily_average = total_energy / days
  
  month_days = as.double(end_date-start_date)
  prediction = monthly_predictions()$kWh[month(start_date)]
  
  # Should we show the monthly estimate?
  monthly_estimate = 
    if_else(days==month_days, '',
            str_glue('monthly est {round(month_days*daily_average, 0)} kWh, '))
  
  ggplot(daily, aes(time, value/1000)) +
    geom_line(linewidth=0.4, aes(group=day, color=daily_max>=6000)) +
    # geom_line(data=daily |> rename(group=day), aes(group=group),
    #           linewidth=0.1, color='lightgrey') +
    geom_text(data=energy, aes(label=label), x=0, y=4,
              hjust=-0.1, vjust=1.2) +
    scale_x_time(breaks=scales::date_breaks('6 hours'),
                 labels = hour) +
    scale_y_continuous(minor_breaks=NULL) +
    scale_color_manual(values=c(`TRUE`='darkred', `FALSE`='grey40'),
                      guide='none') +
    labs(x='Time of day', y='kW',
         title=str_glue('Daily power generation (kW) ',
         '{format(start_date, "%B %Y")}'),
         subtitle=str_glue('Total energy generation: ',
         '{round(total_energy, 0)} kWh ({round(daily_average, 0)} kWh/day), {monthly_estimate}',
         'predicted {prediction} kWh',
         '\nDays with peak power of 6kW in red')) +
    facet_wrap(~day) +
    theme_minimal() +
    theme(plot.title=element_text(face='bold'))
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
    scale_x_date(date_breaks='month', date_labels='%b', minor_breaks=NULL) +
    scale_fill_manual(values=c(`TRUE`='darkred', `FALSE`='grey40'),
                      guide='none') +
    labs(x='Date', y='Maximum power (W)',
         title='Daily maximum power generation',
         subtitle=('Peak days of 6kW in red')) +
    theme_minimal() +
    theme(axis.text.x=element_text(hjust=-0.2, vjust=10))
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
    facet_wrap(~month, ncol=1, labeller=as_labeller(month.name |> set_names(1:12))) +
    theme_minimal() +
    theme(plot.title=element_text(face='bold', size=rel(1.5)),
          strip.text=element_text(face='bold', size=rel(1.1)))
}

#' Monthly energy (kWh) compared to predicted
#' @export
monthly_energy_chart = function() {
  energy = energy_table() |> 
    group_by(year, month) |> 
    summarize(kWh = sum(value)/1000, .groups='drop') |> 
    collect() |> 
    rename(Month=month)
  
  prediction = monthly_predictions()
  
  ggplot(energy, aes(Month, kWh)) +
    geom_col(fill='darkgreen') +
    geom_crossbar(data=prediction, aes(ymin=kWh, ymax=kWh),
                  color='darkred', width=1) +
    facet_wrap(~year, ncol=1) +
    scale_x_continuous(breaks=1:12, labels=month.abb) +
    labs(title='Monthly energy generation (kWh)',
     subtitle='Monthly generation in green, prediction in red') +
    theme_minimal()+
    theme(plot.title=element_text(face='bold', size=rel(1.5)),
          strip.text=element_text(face='bold', size=rel(1.1)))

}

#' Daily energy (kWh) for the year
#' @export
daily_energy_chart = function() {
  energy = energy_table()

  daily = energy |> 
    collect()
  
  daily = daily |> 
    mutate(avg7=slider::slide_mean(value, before=3, after=3))
  
  ggplot(daily, aes(date, value/1000))  +
    geom_col(just=0, aes(fill=value>40000)) +
#    geom_line(aes(y=avg7/1000), color='green3', linewidth=1) +
    scale_x_date(date_breaks='month', date_labels='%b', minor_breaks=NULL) +
    scale_fill_manual(values=c(`TRUE`='darkred', `FALSE`='grey40'),
                      guide='none') +
    labs(x='Date', y='kWh',
         title='Daily energy generation (kWh)',
         subtitle='Days with 40kWh or more in red') +
    theme_minimal() +
    theme(axis.text.x=element_text(hjust=-0.2, vjust=10))
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
    facet_wrap(~month, ncol=1, labeller=as_labeller(month.name |> set_names(1:12))) +
    theme_minimal() +
    theme(plot.title=element_text(face='bold', size=rel(1.5)),
          strip.text=element_text(face='bold', size=rel(1.1)))
}

#' Estimated daily usage vs generation
#' @export
daily_usage_chart = function() {
  energy = energy_summary() |> 
    slice_tail(n=-1) # First date has no usage, only an end date
  
  # Get predicted daily production
  years = tibble(Year=range(year(energy$Date)))
  gen_start = energy |> filter(generation>0) |> pull(start_date) |> min()
  predictions = monthly_predictions() |> 
    cross_join(years) |> 
    mutate(start_date=as.Date(ISOdate(Year, Month, 1, tz='UTC')),
           end_date=as.Date(start_date + dmonths(1)),
           days_in_month=days_in_month(start_date),
           est_daily_generation=kWh/days_in_month) |> 
    filter(end_date>=gen_start, start_date<= max(energy$end_date))
  
  lw = 1
  slw = 0.2
  ggplot(energy) +
    geom_step(data=predictions,
              aes(start_date, est_daily_generation, color='Estimate'),
              linewidth=slw) +
    geom_segment(data=predictions,
              aes(x=start_date, xend=end_date, 
                     y=est_daily_generation, yend=est_daily_generation, 
                     color='Estimate'), linewidth=lw) +
    geom_step(aes(start_date, daily_use, color='Usage'), linewidth=slw) +
    geom_segment(aes(x=start_date, xend=end_date, 
                     y=daily_use, yend=daily_use, color='Usage'),
                 linewidth=lw) +
    geom_step(aes(start_date, daily_generation, color='Generation'),
              linewidth=slw) +
    geom_segment(aes(x=start_date, xend=end_date, 
                     y=daily_generation, yend=daily_generation, 
                     color='Generation'),
                 linewidth=lw) +
    scale_x_date(date_breaks='month', date_labels="%b ’%y", minor_breaks=NULL) +
    scale_color_manual('Average daily power', 
                       values=c(Usage='darkred', 
                                Generation='darkgreen',
                                Estimate='darkblue')) +
    ylim(0, NA) +
    labs(x='End date', y='Average daily use (kWh)',
         title='Average daily energy generation and use (kWh)') +
    theme_minimal()
}