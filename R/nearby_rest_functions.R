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
           # end_lag = lag(End.Date.Time.f),
           # start_lag = lag(Start.Date.Time.f),
           start_lead = lead(Start.Date.Time.f),
           # end_lead = lead(End.Date.Time.f),
           # ind_lag = lag(ind),
           ind_lead = lead(ind),
           # diff_lag = difftime(Start.Date.Time.f, end_lag, units = "mins"),
           diff_lead = difftime(start_lead, End.Date.Time.f, units = "mins")) %>%
    group_by(dayno) %>%
    mutate(interval_match = ifelse(diff_lead <= nmin, ind_lead,0),
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

  mdtemp <- mydata %>%
    mutate(start = to_dec(updated.Start),
           end = to_dec(updated.End, add=F),
           end = ifelse(end < start, end + 24, end),
           mid = (start + end) / 2) %>%
    group_by(dayno) %>%
    mutate(rest_obs = max(row_number()),
           ind = row_number(),
           main_or_nap = ifelse(updated.Duration == max(updated.Duration,na.rm=T), "MAIN", "NAP"))

  avg <- mdtemp %>%
    filter(main_or_nap == 'MAIN') %>% ungroup() %>%
    summarize(avg_start = median(start,na.rm=T),
              avg_end = median(end,na.rm=T),
              avg_mid = median(mid,na.rm=T))

  mdtemp$avg_start <- avg$avg_start
  mdtemp$avg_end <- avg$avg_end

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

#' @export
update_sleep2 <- function(mydata){

  # mydata <- data.frame(ind = 1:6, interval_match = c(2,3,0,5,6,0), Start.Date.Time.f = c(5:7, 9:11),
  #                      End.Date.Time.f = c(5.5, 6.5, 7.5, 9.5, 10.5, 11.5)) %>%
  #   mutate(start_lead = lead(Start.Date.Time.f),
  #         ind_lead = lead(ind))

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
