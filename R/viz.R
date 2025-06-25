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
    
  last_full_day = daily |> 
    filter(time==as_hms('23:45:00')) |> 
    pull(day) |> 
    max()
  
  energy = energy_table()
  energy = energy |> 
    filter(year==year(start_date), month==month(start_date), day <= last_full_day) |> 
    collect() |> 
    mutate(kwh = value / 1000,
           label=str_glue('{round(kwh, 1)} kWh'))
  
  total_energy = sum(energy$kwh)
  days = last_full_day
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
    geom_text(data=energy, aes(label=label), x=0, 
              y=floor(max(daily$value/1000)),
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
         'predicted {round(prediction, 0)} kWh',
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
    collect() |> 
    mutate(year_day=yday(date))

  breaks = tibble(
    date=seq.Date(ymd('2023-01-01'), ymd('2023-12-1'), by='month'),
    year_day=yday(date),
    label=format(date, '%b'))
  
  ggplot(daily_max, aes(year_day, max_power)) +
    geom_col(aes(fill=max_power>6000), just=0, width=1) +
#    scale_x_date(date_breaks='month', labels=scales::label_date_short(), minor_breaks=NULL) +
    scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, minor_breaks=NULL) +
    scale_fill_manual(values=c(`TRUE`='darkred', `FALSE`='grey40'),
                      guide='none') +
    facet_wrap(~year, ncol=1) +
    labs(x='Date', y='Maximum power (W)',
         title='Daily maximum power generation',
         subtitle=('Peak days of 6kW in red')) +
    theme_minimal() +
    theme(axis.text.x=element_text(hjust=-0.2))
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

#' Average daily power generation by month.
#' @export
average_daily_power = function() {
  by_time = power_table() |> 
  group_by(month, time) |> 
  summarize(value=mean(value), .groups='drop') |>
  collect() |> 
  mutate(time=hms::hms(as.numeric(time)))

  # Create a named vector for month labels
  month_labels <- setNames(
    month.name,
    1:12
  )

  ggplot(by_time, aes(time, value/1000)) +
    geom_line() +
      scale_x_time(breaks=scales::date_breaks('6 hours'),
                   labels = hour) +
      scale_y_continuous(minor_breaks=NULL) +
    facet_wrap(~month, labeller = as_labeller(month_labels)) +
    labs(x='Time of day', y='kW',
         title='Average daily power generation per month (kW) ') +
    theme_minimal() +
    theme(plot.title=element_text(face='bold'),
          strip.text = element_text(face = "bold"),)
}

#' Difference between monthly energy (kWh) and predicted
#' as a percent of predicted.
#' @export
monthly_discrepancy_chart = function() {
  energy = energy_table() |> 
    collect() |> 
    clip_partial_month() |> 
    group_by(year, month) |> 
    summarize(kWh = sum(value)/1000,
              date=min(date),
              .groups='drop') |> 
    collect() |> 
    rename(Month=month)
  
  prediction = monthly_predictions()
  
  diffs = energy |> 
    left_join(prediction, by='Month', suffix=c('', '.pred')) |> 
    mutate(diff = kWh-kWh.pred, 
           diff_pct = diff/kWh.pred,
           color=case_when(
             diff_pct >=0 ~ 'darkgreen',
             diff_pct < -0.1 ~ 'red3',
             .default = 'darkred'
           ))
  
  subtitle = str_glue('{sum(diffs$diff_pct< -0.1)} out of {nrow(diffs)} months ',
                      'were at least 10% below the predicted generation.')
  ggplot(diffs, aes(date, diff_pct, fill=color)) +
    geom_hline(yintercept=-0.1, color='lightgray', linetype=2) +
    geom_col(position='dodge') +
    scale_fill_identity(guide=NULL) +
    scale_x_date(date_breaks='month', labels=scales::label_date_short(), minor_breaks=NULL) +
    scale_y_continuous(labels=scales::label_percent(accuracy=1),
                       minor_breaks=NULL) +
    labs(title='Monthly energy generation vs predictions',
         subtitle=subtitle,
         x='',
         y='Percent difference') +
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
    mutate(year_day = yday(date))

  # Make break points and labels
  breaks = tibble(
    date=seq.Date(ymd('2023-01-01'), ymd('2023-12-1'), by='month'),
    year_day=yday(date),
    label=format(date, '%b'))
  
  ggplot(daily, aes(year_day, value/1000)) +
    geom_col(just=0, position='dodge', fill='steelblue', width=1) +
    scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, minor_breaks=NULL) +
    scale_fill_brewer(palette='Set1') +
    facet_wrap(~year, ncol=1) +
    labs(x='', y='kWh',
         title='Daily energy generation (kWh)') +
    theme_minimal() +
    theme(axis.text.x=element_text(hjust=-0.2),
          plot.title=element_text(face='bold', size=rel(1.5)),
          strip.text=element_text(face='bold', size=rel(1.1)))
  
}

#' Histogram of daily energy (kWh) for the year
#' @export
daily_energy_histogram = function() {
  energy = energy_table()

  daily = energy |> 
    collect() |> 
    mutate(month_name = factor(month.abb[month], levels=month.abb))
  
  ggplot(daily, aes(value/1000))  +
    geom_histogram(binwidth=5, boundary=0) +
    labs(x='Daily energy generation (kWh)', y='Number of days',
         title=str_glue('Daily energy generation (kWh) ')) +
    facet_grid(month_name ~ year) +
    theme_minimal() +
    theme(plot.title=element_text(face='bold', size=rel(1.5)),
          strip.text=element_text(face='bold', size=rel(1.1)))
}

#' Actual daily usage vs actual and estimated generation
#' @export
daily_usage_vs_generation = function() {
  # Actual generation and usage
  energy = energy_summary() |> 
    slice_tail(n=-1) # First date has no usage, only an end date
  generation = energy |> filter(generation>0)
  gen_start = generation |> pull(start_date) |> min()
  
  # Get predicted daily production
  years = tibble(Year=seq(min(year(energy$Date)), max(year(energy$Date))))
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
    geom_step(data=generation, aes(start_date, daily_generation, color='Generation'),
              linewidth=slw) +
    geom_segment(data=generation, aes(x=start_date, xend=end_date, 
                     y=daily_generation, yend=daily_generation, 
                     color='Generation'),
                 linewidth=lw) +
    scale_x_date(date_breaks='month', labels=scales::label_date_short(), minor_breaks=NULL) +
    scale_color_manual('Average daily power', 
                       values=c(Usage='darkred', 
                                Generation='darkgreen',
                                Estimate='darkblue')) +
    ylim(0, NA) +
    labs(x='End date', y='Average daily use (kWh)',
         title='Average daily energy generation and use (kWh)') +
    theme_minimal() +
    theme(axis.text.x=element_text(hjust=-0.2))
}



#' Actual daily usage by year
#' @export
daily_usage = function() {
  # Actual usage
  energy = energy_summary() |> 
    mutate(year=factor(year(start_date)),
           year_start = yday(start_date),
           year_end = yday(end_date),
           year_end = if_else(year_end < year_start, year_end+365, year_end)) |> 
    slice_tail(n=-1) # First date has no usage, only an end date

  # Make break points and labels
  breaks = tibble(
    date=seq.Date(ymd('2023-01-01'), ymd('2023-12-1'), by='month'),
    year_day=yday(date),
    label=format(date, '%b %d'))
  
  lw = 1
  slw = 0.2
  ggplot(energy) +
    geom_step(aes(year_start, daily_use, color=year, group=year)) +
    geom_segment(aes(x=year_start, xend=year_end,
                     y=daily_use, yend=daily_use, color=year),
                 linewidth=lw) +
    scale_x_continuous(breaks=breaks$year_day, labels=breaks$label, minor_breaks=NULL) +
    scale_color_brewer(palette='Set1') +
    ylim(0, NA) +
    labs(x='Date', y='Average daily use (kWh)', color='Year',
         title='Average daily energy use (kWh)') +
    theme_minimal() +
    theme(axis.text.x=element_text(hjust=-0.2))
}
