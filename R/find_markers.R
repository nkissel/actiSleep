find_markers <- function(updated_sleep_df, marker_df, window = 30) {

  marker_df <- marker_df %>% select(-dayno)

  difftime2 <- function(x, y, units) {
    ifelse(is.na(difftime(x, y, units = units)), Inf, difftime(x, y, units = units))
  }

  starts <- full_join(updated_sleep_df, marker_df) %>%
    arrange(ID, dayno, updated.Start) %>%
    ungroup() %>%
    # Fill in NAs
    mutate(updated.Start1 = if_else(is.na(updated.Start), diary.Start,  updated.Start),
           diary.Start1 = if_else(is.na(diary.Start), updated.Start,  diary.Start)) %>%
    # only grab the markers that are within window minutes of the rest interval start time
    filter((abs(as.numeric(difftime(updated.Start1, Marker.Date.Time.f, units="mins"))) <= window |
              abs(as.numeric(difftime(diary.Start1, Marker.Date.Time.f, units="mins"))) <= window)) %>%
    group_by(ID, dayno, updated.Start1, diary.Start1) %>%
    # Grab the LATEST marker within this interval
    filter(Marker.Date.Time.f == max(Marker.Date.Time.f, na.rm=T)) %>% ungroup() %>%
    # select(ID, dayno, Marker.Date.Time.f, Interval.f) %>%
    rename(marker.Start = Marker.Date.Time.f)


  ends <- full_join(updated_sleep_df, marker_df) %>%
    arrange(ID, dayno, updated.Start) %>%
    ungroup() %>%
    # fill nas
    mutate(updated.End1 = if_else(is.na(updated.End), diary.Stop,  updated.End),
           diary.Stop1 = if_else(is.na(diary.Stop), updated.End,  diary.Stop)) %>%
    # only grab the markers that are within window minutes of the rest interval end time
    filter((abs(as.numeric(difftime(updated.End1, Marker.Date.Time.f, units="mins"))) <= window |
              abs(as.numeric(difftime(diary.Stop1, Marker.Date.Time.f, units="mins"))) <= window)) %>%
    group_by(ID, dayno, updated.End1, diary.Stop1) %>%
    # Grab the LATEST marker within this interval
    filter(Marker.Date.Time.f == max(Marker.Date.Time.f, na.rm=T)) %>% ungroup() %>%
    # select(ID, dayno, Marker.Date.Time.f, Interval.f) %>%
    rename(marker.Stop = Marker.Date.Time.f)

  res <- full_join(starts, ends) %>% ungroup() %>%
    mutate(dt_start = abs(difftime(marker.Start, updated.Start1, units = 'mins')),
           dt_stop = abs(difftime(marker.Stop, updated.End1, units = 'mins')),
           marker.Start = if_else(mapply(identical, marker.Start, marker.Stop) & (dt_start > dt_stop),
                                  NA, marker.Start),
           marker.Stop = if_else(mapply(identical, marker.Start, marker.Stop) & (dt_start <= dt_stop),
                                 NA, marker.Stop)) %>%
    select(ID, dayno, Interval.f, marker.Start, marker.Stop) %>%
    arrange(dayno, marker.Start)

  return(res)
}

find_markers_acdl <- function(updated_sleep_df, marker_df, window = 30){

  marker_df <- marker_df %>% select(-dayno)
  updated_sleep_df<-updated_sleep_df %>% mutate(Interval.f=1:n())
  starts <- full_join(updated_sleep_df, marker_df) %>%
    arrange(ID, dayno, actigraphy.Start) %>%
    ungroup() %>%
    # Fill in NAs
    mutate(actigraphy.Start1 = if_else(is.na(actigraphy.Start), diary.Start,  actigraphy.Start),
           diary.Start1 = if_else(is.na(diary.Start), actigraphy.Start,  diary.Start),
           light.Start1 = if_else(is.na(light.Start), actigraphy.Start,  light.Start)) %>%
    # only grab the markers that are within window minutes of the rest interval start time
    filter(abs(as.numeric(difftime(actigraphy.Start1, Marker.Date.Time.f, units="mins"))) <= window |
             abs(as.numeric(difftime(diary.Start1, Marker.Date.Time.f, units="mins"))) <= window |
             abs(as.numeric(difftime(light.Start1, Marker.Date.Time.f, units="mins"))) <= window) %>%
    group_by(ID, dayno, actigraphy.Start1, diary.Start1, light.Start1) %>%
    # Grab the LATEST marker within this interval
    filter(Marker.Date.Time.f == max(Marker.Date.Time.f, na.rm=T))  %>%
    ungroup() %>%
    # dplyr::select(ID, dayno, Marker.Date.Time.f, Interval.f) %>%
    rename(marker.Start = Marker.Date.Time.f)

  ends <- full_join(updated_sleep_df, marker_df) %>%
    arrange(ID, dayno, actigraphy.Start) %>%
    ungroup() %>%
    # fill nas
    mutate(actigraphy.Stop1 = if_else(is.na(actigraphy.Stop), diary.Stop,  actigraphy.Stop),
           diary.Stop1 = if_else(is.na(diary.Stop), actigraphy.Stop,  diary.Stop),
           light.Stop1 = if_else(is.na(light.Stop), actigraphy.Stop,  light.Stop)) %>%
    # only grab the markers that are within window minutes of the rest interval end time
    filter(abs(as.numeric(difftime(actigraphy.Stop1, Marker.Date.Time.f, units="mins"))) <= window |
             abs(as.numeric(difftime(diary.Stop1, Marker.Date.Time.f, units="mins"))) <= window |
             abs(as.numeric(difftime(light.Stop1, Marker.Date.Time.f, units="mins"))) <= window) %>%
    group_by(ID, dayno, actigraphy.Stop1, diary.Stop1, light.Stop1) %>%
    # Grab the LATEST marker within this interval
    filter(Marker.Date.Time.f == max(Marker.Date.Time.f, na.rm=T))  %>%
    ungroup() %>%
    # dplyr::select(ID, dayno, Marker.Date.Time.f, Interval.f) %>%
    rename(marker.Stop = Marker.Date.Time.f)

  res <- full_join(starts, ends) %>% ungroup() %>%
    mutate(dt_start = abs(difftime(marker.Start, updated.Start1, units = 'mins')),
           dt_stop = abs(difftime(marker.Stop, updated.End1, units = 'mins')),
           marker.Start = if_else(mapply(identical, marker.Start, marker.Stop) & (dt_start > dt_stop),
                                  NA, marker.Start),
           marker.Stop = if_else(mapply(identical, marker.Start, marker.Stop) & (dt_start <= dt_stop),
                                 NA, marker.Stop)) %>%
    select(ID, dayno, Interval.f, marker.Start, marker.Stop) %>%
    arrange(dayno, marker.Start)

  return(res)
}
