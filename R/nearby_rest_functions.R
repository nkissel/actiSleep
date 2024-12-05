#' Find naps from rest actigraphy data
#'
#' This function creates a new variable called `interval_match` which indicates
#' the row index of rest event should be merged with another. Rest intervals
#' get merged if they're within a certain number of minutes together.
#'
#' @param mydata a data.frame of start/end rest that has a `dayno` column
#' @param nmin number of minutes in-between intervals
#' @return a data.frame with added `interval_match` variable denoting the row
#' index of the rest interval within `nmin` minutes of the current row.
#' @export
find_nearby_rest <- function(mydata, nmin = 30){
  mydata %>%
    arrange(Start.Date.Time.f) %>%
    mutate(ind = row_number(),
           diff_lead = difftime(lead(Start.Date.Time.f), End.Date.Time.f, units = "mins")) %>%
    group_by(dayno) %>%
    mutate(interval_match = ifelse(diff_lead <= nmin, lead(ind), 0),
           interval_match = ifelse(is.na(interval_match), 0, interval_match))
}

#' Find nocturnal from rest actigraphy data
#'
#' A function that identifies rest intervals that are separated by a nocturnal
#' awakening. Rather than defining nocturnal statically, this function uses the
#' median start/end time of the longest rest intervals to determine night time.
#'
#' @param mydata a data.frame of start/end rest that has a `dayno` column
#' @return a data.frame with added `interval_match` variable denoting the row
#' index of the rest interval within `nmin` minutes of the current row.
#' @export
find_nocturnal_awakening <- function(mydata){

  # Find longest rest per day
  mdtemp <- mydata %>%
    mutate(start = to_dec(updated.Start),
           end = to_dec(updated.End, add=F),
           end = ifelse(end < start, end + 24, end),
           mid = (start + end) / 2) %>%
    group_by(dayno) %>%
    mutate(rest_obs = max(row_number()),
           ind = row_number(),
           main_or_nap = ifelse(updated.Duration == max(updated.Duration,na.rm=T), "MAIN", "NAP"))

  # find the median start, mid, and end times for longest rests
  avg <- mdtemp %>%
    filter(main_or_nap == 'MAIN') %>% ungroup() %>%
    summarize(avg_start = median(start,na.rm=T),
              avg_end = median(end,na.rm=T),
              avg_mid = median(mid,na.rm=T))

  mdtemp$avg_start <- avg$avg_start
  mdtemp$avg_end <- avg$avg_end

  # Identify nocturnal awakenings that occur within the median start/end time
  ret <- mdtemp %>%  ungroup() %>%
    mutate(ind = row_number(),
           Start.Date.Time.f = updated.Start,
           End.Date.Time.f = updated.End,
           Duration = updated.Duration,
           Interval. = Interval.f,
           ind_lead = lead(ind),
           start = to_dec(updated.Start),
           end = to_dec(updated.End, add=F),
           end = ifelse(end < start, end + 24, end)) %>%
    mutate(interval_match = ifelse(
      end < avg_end & end > avg_start & dayno == lead(dayno) &
        lead(start) < avg_end & lead(start) > avg_start, ind_lead, 0))

  return(ret)
}

#' Update rest interval
#'
#' This function updates the start and end times of rest intervals as determined
#' by `find_nearby_rest()`.
#'
#' @param mydata a data.frame of start/end rest that has a `dayno` column
#' @return a data.frame with updated start and end times of rest intervals
#' @export
update_sleep <- function(mydata){
  mydata %>% mutate(
    updated.Start = if_else(
      nap_head_tail == "head" & to_be_merged == 1, Start.Date.Time.f, if_else(
        nap_head_tail == "tail" & to_be_merged == 1, start_lag, Start.Date.Time.f)),
    updated.End = if_else(
      nap_head_tail == "head" & to_be_merged == 1, end_lead, if_else(
        nap_head_tail == "tail" & to_be_merged == 1, End.Date.Time.f, End.Date.Time.f))) %>%
    # filter(!(Interval. %in% interval_match)) %>% # dropping the original "MAINS" #THIS COULD BE A PROBLEM!!!?!?!?!?!?!?!?!?!?!??!?!!?!?!?!?!?!??!?!?!?!!?!?
    mutate(Type = if_else(main_or_nap != "MAIN" & to_be_merged == 1, "MAIN", main_or_nap),
           Interval.f = row_number()) %>%
    dplyr::select(ID, dayno, Interval.f, updated.Start, updated.End, Type) %>%
    mutate(updated.Duration = difftime(updated.End, updated.Start, units = "mins") %>% as.numeric) %>%
    group_by(dayno, Type) %>% # HERE DOWN IS NEW CODE MAY NEED TO REMOVE
    mutate(is_real_max = updated.Duration == max(updated.Duration) & Type == 'MAIN') %>%
    ungroup() %>% filter(is_real_max | Type == 'NAP') %>% select(-is_real_max)
}

#' Update rest interval
#'
#' This function updates the start and end times of rest intervals as determined
#' by `find_nearby_rest()`.
#'
#' @param mydata a data.frame of start/end rest that has a `dayno` column
#' @return a data.frame with updated start and end times of rest intervals
#' @export
update_sleep2 <- function(mydata){
  to_update1 <- mydata %>% filter(interval_match != 0) %>% pull(interval_match)
  to_update2 <- mydata %>% filter(interval_match != 0) %>% pull(ind)
  to_update <- sort(unique(c(to_update1,to_update2)))

  if(length(to_update) > 0 ) {
    mydata_to_update <- mydata %>% ungroup() %>% slice(to_update)

    mydata_to_update <-  mydata_to_update %>%
      mutate(ind_match_d = interval_match - ind,
             group_marker = as.numeric(ind_match_d < 0),
             group_marker = rev(cumsum(rev(group_marker))),
             ind_match_d_lag = lag(ind_match_d)) %>%
      select(ID, ind, dayno, interval_match, Start.Date.Time.f, End.Date.Time.f, group_marker) %>%
      group_by(group_marker, ID) %>%
      summarize(updated.Start = min(Start.Date.Time.f),
                updated.End = max(End.Date.Time.f),
                dayno = min(dayno)) %>%
      mutate(updated.Duration = difftime(updated.End, updated.Start, units = "mins") %>% as.numeric) %>%
      ungroup() %>% select(ID, dayno, updated.Start, updated.End, updated.Duration)

    data_to_keep <- mydata %>% ungroup() %>% slice(-to_update) %>%
      select(ID, dayno, updated.Start = Start.Date.Time.f,
             updated.End = End.Date.Time.f, updated.Duration = Duration)

    merged_data <- rbind(mydata_to_update, data_to_keep) %>%
      arrange(updated.Start) %>%
      mutate(Interval.f = 1:n()) %>%
      select(ID, dayno, Interval.f, updated.Start, updated.End, updated.Duration)

  } else {
    merged_data <- mydata %>% ungroup() %>%
      select(ID, dayno, updated.Start = Start.Date.Time.f,
             updated.End = End.Date.Time.f, updated.Duration = Duration) %>%
      arrange(updated.Start) %>%
      mutate(Interval.f = 1:n()) %>%
      select(ID, dayno, Interval.f, updated.Start, updated.End, updated.Duration)

  }

  return(merged_data)
}
