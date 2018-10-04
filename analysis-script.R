


# load the required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# import the data
ed_data <- read_csv('simulated_ed_data.csv') %>% 
  print()


# calculate arrival volumes per hour --------------------------------------

volumes_hour <- ed_data %>% 
  group_by(year = year(arrival_times),
           month = month(arrival_times),
           day = day(arrival_times),
           hour = hour(arrival_times)) %>% 
  summarise(volume = n()) %>% 
  mutate(timestamp = ymd_h(paste(year, month, day, hour))) %>% 
  print()

# plot patient arrivals per hour
volumes_hour %>% 
  ggplot(aes(timestamp, volume)) + 
  geom_line() + 
  xlab('date') +
  ggtitle('Emergent Department Arrivals Per Hour') +
  coord_cartesian(ylim = c(0, 20)) +
  scale_y_continuous(breaks = seq(0,20, 2))



# calculate arrival volumes per minute ------------------------------------

volumes_minute <- ed_data %>% 
  group_by(year = year(arrival_times),
           month = month(arrival_times),
           day = day(arrival_times),
           hour = hour(arrival_times),
           minute = minute(arrival_times)) %>% 
  summarise(volume = n()) %>% 
  mutate(timestamp = ymd_hm(paste(year, month, day, hour, minute))) %>% 
  ungroup() %>% 
  select(timestamp, volume) %>% 
  print()


# create a sequence of times from the start to end of your available data
start <- min(volumes_minute$timestamp)
end   <- max(volumes_minute$timestamp)
full_time_window <- tibble(timestamp = seq(start, end,
                                           by = 'mins'))

# do a right join to get all of the timestamps

volumes_minute <- volumes_minute %>% 
  right_join(full_time_window, by = 'timestamp') %>% 
  mutate(volume = ifelse(is.na(volume), 0, volume)) %>%  
  print()

# plot arrival volumes per minute
volumes_minute %>% 
  ggplot(aes(timestamp, volume)) + 
  geom_line() + 
  ggtitle('Emergent Department Arrivals Per Minute') +
  coord_cartesian(ylim = c(0, 3)) +
  xlab('date')



# calculating census volumes ----------------------------------------------

ed_data <- ed_data %>% 
  mutate(arrival_times = floor_date(arrival_times, unit = 'minute'),
         depart_times = floor_date(depart_times, unit = 'minute'))
arrivals <- ed_data %>% 
  select(timestamp = arrival_times) %>% 
  mutate(counter = 1)

departures <- ed_data %>% 
  select(timestamp = depart_times) %>% 
  mutate(counter = -1)


census_volumes <- arrivals %>% 
  bind_rows(departures) %>% 
  arrange(timestamp, counter) %>%  # arrange by time
  mutate(volume = cumsum(counter)) # take cumulative sum of the counter

# create a sequence of times from the start to end of your available data
start <- min(census_volumes$timestamp)
end   <- max(census_volumes$timestamp)
full_time_window <- tibble(timestamp = seq(start, end,
                                           by = 'mins'))
census_volumes <- census_volumes %>% 
  right_join(full_time_window, 
             by = 'timestamp') %>% # bind with the full time window to fill gaps
  arrange(timestamp) %>% 
  fill(volume, .direction = 'down') # take last observation carried forward

# plot census volumes
census_volumes %>% 
  ggplot(aes(timestamp, volume)) + 
  geom_line() + 
  xlab('Date') + 
  ggtitle('Emergency Department Census',
          subtitle = 'In one minute intervals')


# calculating census volumes with a begin timestamp -----------------------

# set a start time
start_time <- ymd_hms('2018-10-02 00:00:00')


# get the base count
# This is the number of people who arrived to the ED before the start time
# but are still in the ED after the start time
pre <- ed_data %>%
  filter(arrival_times < start_time &
           depart_times >= start_time)

# Anyone who leaves the ED after the start_time
post <- ed_data %>%
  filter(depart_times >= start_time)


# The arrivals
arrivals <- post %>% 
  select(timestamp = arrival_times) %>% 
  mutate(counter = 1)%>%
  filter(timestamp >= start_time)

# The departures
departures <- post %>% 
  select(timestamp = depart_times) %>% 
  mutate(counter = -1)


census_volumes <- arrivals %>% 
  bind_rows(departures) %>% 
  arrange(timestamp, counter) 

# Add the starting number of people to the first row of the data
census_volumes$counter[1] <- census_volumes$counter[1] + nrow(pre)

census_volumes <- census_volumes %>%  # arrange by time
  mutate(volume = cumsum(counter)) 

# create a sequence of times from the start to end of your available data
start <- min(census_volumes$timestamp)
end   <- max(census_volumes$timestamp)
full_time_window <- tibble(timestamp = seq(start, end,
                                           by = 'mins'))
census_volumes <- census_volumes %>% 
  right_join(full_time_window, 
             by = 'timestamp') %>% # bind with the full time window to fill gaps
  arrange(timestamp) %>% 
  fill(volume, .direction = 'down') # take last observation carried forward

# plot census volumes
census_volumes %>% 
  ggplot(aes(timestamp, volume)) + 
  geom_line() + 
  xlab('Date') + 
  ggtitle('Emergency Department Census',
          subtitle = 'In one minute intervals')


# create a function -------------------------------------------------------

calc_census_volume <- function(data, start_var, end_var, 
                               begin_time = '2018-10-02 00:00:00') {
  sv <- rlang::enquo(start_var)
  ev <- rlang::enquo(end_var)
  
  if(!inherits(begin_time, 'POSIXct')) {
    begin_time <- lubridate::ymd_hms(begin_time)
  }
  
  
  # keep track of timestamp issues
  incorrect_times <- data %>%
    dplyr::filter(!!ev < !!sv | is.na(!!ev) | is.na(!!sv)) %>%
    dplyr::mutate(type = ifelse( is.na(!!ev) ,
                                 'missing end timestamp',
                                 ifelse(is.na(!!sv), 
                                        'missing start timestamp', 
                                        'end timestamp before start timestamp')))
  # remove timestamp problems
  filtered_data <- data %>%
    dplyr::filter(!!sv <= !!ev) %>%   # no end timestamps after start timestapms
    dplyr::filter(!is.na(!!ev)) %>%  # no missing end times
    dplyr::filter(!is.na(!!sv))  # no missing start times
  
  
  # get the base count
  pre <- filtered_data %>%
    dplyr::filter(!!sv < begin_time &
                    !!ev >= begin_time)
  
  post <- filtered_data %>%
    dplyr::filter(!!ev >= begin_time)
  
  # The arrivals
  arrivals <- post %>% 
    dplyr::select(timestamp = !!sv) %>% 
    dplyr::mutate(counter = 1) %>%
    dplyr::filter(timestamp >= start_time)
  
  # The departures
  departures <- post %>% 
    dplyr::select(timestamp = !!ev) %>% 
    dplyr::mutate(counter = -1)
  
  # bind the arrivals and departures
  census_volumes <- arrivals %>% 
    dplyr::bind_rows(departures) %>% 
    dplyr::arrange(timestamp, counter) 
  
  # Add the starting pre volume to the first row
  census_volumes$counter[1] <- census_volumes$counter[1] + nrow(pre)
  
  census_volumes <- census_volumes %>%  # arrange by time
    dplyr::mutate(volume = cumsum(counter)) 
  
  # create a sequence of times from the start to end of your available data
  start <- min(census_volumes$timestamp)
  end   <- max(census_volumes$timestamp)
  full_time_window <- dplyr::tibble(timestamp = seq(start, end,
                                                    by = 'mins'))
  census_volumes <- census_volumes %>% 
    right_join(full_time_window, 
               by = 'timestamp') %>% # bind with the full time window to fill gaps
    arrange(timestamp) %>% 
    fill(volume, .direction = 'down') # take last observation carried forward
  
  if(nrow(incorrect_times) > 0) {
    incorrect_times_sum <- incorrect_times %>%
      dplyr::count(type)
    
    msg <- paste0(paste(incorrect_times_sum$type,
                        incorrect_times_sum$n, sep = ': n = '),
                  collapse = '\n')
    msg <- paste0('Potential Data Issues:\n',
                  msg, 
                  '\nThese timestamp errors have been removed from the 
                  data prior to volume calculation')
    message(msg)
  }
  
  return(census_volumes)
  
}

# try the function

ed_data %>% 
  calc_census_volume(start_var = arrival_times,
                     end_var = depart_times)





