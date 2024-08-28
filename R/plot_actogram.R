#' Plot actogram
#'
#' use after actiSleep
#'
#' @param sd_data epoch level data.frame
#' @param all_markers marker data.frame
#' @param dmin minimum day number
#' @param dmax maximum day number
#' @param filter_naps true or false on filtering naps
#' @param db default `'60 min'` tick amount on x-axis
#' @param flags flag data.frame
#' @param epoch_length epoch length in minutes
#' @return a list of data.frames corresponding to properties, statistics, markers,
#' epochs-level.
#' @import ggplot2
#' @export
plot_actogram <- function(
    sd_data, all_markers, dmin, dmax, filter_naps =F,
    db = '60 min', flags, epoch_length = 1){
  quiet1 <- function(x) suppressMessages(suppressWarnings(x))
  na <- as.POSIXct(NA, tz = 'UTC')

  all_markers <- as.data.frame(all_markers)
  anchor_date <- ap(paste0(as.Date(min(sd_data$Epoch.Date.Time.f)), ' 12:00:00'), tz = 'UTC')

  add_marker_col <- function(col1, col2) {
    ec <- rep(na, nrow(sd_data))
    if(!col1 %in% colnames(sd_data)) {
      sd_data <- cbind(cbind(sd_data, ec), ec)
      colnames(sd_data)[(ncol(sd_data) - 1):ncol(sd_data)] <- c(col1, col2)
    }
    wcol1 <- which(colnames(all_markers) == col1)
    wcol2 <- which(colnames(all_markers) == col2)
    wcol1e <- which(colnames(sd_data) == col1)
    wcol2e <- which(colnames(sd_data) == col2)
    for(j in 1:nrow(all_markers)) {
      rw1 <- which(sd_data$Epoch.Date.Time.f == ap(all_markers[j, wcol1]))
      rw2 <- which(sd_data$Epoch.Date.Time.f == ap(all_markers[j, wcol2]))
      crw <- which(sd_data$Epoch.Date.Time.f >= ap(all_markers[j, wcol1]) &
                     sd_data$Epoch.Date.Time.f <= ap(all_markers[j, wcol2]))
      if(length(crw) > 0) {
        sd_data[crw, wcol1e] <- ap(all_markers[j, wcol1])
        sd_data[crw, wcol2e] <- ap(all_markers[j, wcol2])
        if(col1 == 'algo.Start') {
          sd_data$dayno[crw] <- all_markers$dayno[j]
          sd_data$TYPE[crw] <- all_markers$Type[j]
          sd_data$invalid_flag[crw] <- all_markers$invalid_flag[j]
        }
      }
      sd_data[rw1, wcol1e] <- ap(all_markers[j, wcol1])
      sd_data[rw2, wcol2e] <- ap(all_markers[j, wcol2])
    }
    return(sd_data)
  }
  sd_data$TYPE <- sd_data$invalid_flag <- NA
  sd_data <- add_marker_col('algo.Start', 'algo.Stop')
  sd_data <- add_marker_col('actigraphy.Start', 'actigraphy.Stop')
  sd_data <- add_marker_col('diary.Start', 'diary.Stop')
  sd_data <- add_marker_col('marker.Start', 'marker.Stop')
  sd_data <- add_marker_col('light.Start', 'light.Stop')

  marker_press <- sd_data %>% filter(Marker != 0) %>%
    select(time = Epoch.Date.Time.f, Marker)
  marker_press <- marker_press[
    !((marker_press$time %in% sd_data$marker.Start) |
        (marker_press$time %in% sd_data$marker.Stop)), ]
  wm <- which(sd_data$Epoch.Date.Time.f %in% marker_press$time)
  sd_data$marker.Start[wm] <- marker_press$time
  sd_data$press_flag <- 0
  sd_data$press_flag[wm] <- 1

  plt_d <- sd_data %>% filter(dayno >= dmin - 1, dayno <= dmax + 1) %>%
    mutate(algo.Start = ap(algo.Start), algo.Stop = ap(algo.Stop)) %>% arrange(Epoch.Date.Time.f)
  plt_d$eff_dayno <- plt_d$dayno; plt_d$dayno <- NULL
  flags <- flags %>%  filter(dayno >= dmin, dayno <= dmax)

  pass_noon <- plt_d %>% select(algo.Stop, algo.Start) %>% unique() %>% na.omit
  if(nrow(pass_noon) == 0) return(NULL)
  pass_noon$ref <- if_else(as.numeric(substr(tochr(pass_noon$algo.Start), 12, 13)) > 12,
                           as.Date(pass_noon$algo.Start) + 1, as.Date(pass_noon$algo.Start))
  pass_noon$ref <- as.POSIXct(paste0(pass_noon$ref, ' 12:00:01'), tz='UTC')
  pass_noon <- pass_noon %>% filter(ref > algo.Start & ref < algo.Stop)
  if(nrow(pass_noon) > 0) {
    for(i in seq_along(pass_noon$algo.Stop)) {
      plt_d$eff_dayno[plt_d$Epoch.Date.Time.f >= pass_noon$algo.Start[i] &
                        plt_d$Epoch.Date.Time.f <= pass_noon$algo.Stop[i]] =
        min(plt_d$eff_dayno[plt_d$Epoch.Date.Time.f >= pass_noon$algo.Start[i] &
                              plt_d$Epoch.Date.Time.f <= pass_noon$algo.Stop[i]])
    }
  }

  plt_d <- add_dayno(plt_d, "Epoch.Date.Time.f", anchor_date)
  plt_d$real_dayno <- plt_d$dayno

  t <- plt_d %>% select(real_dayno, Epoch.Date.Time.f) %>% group_by(real_dayno) %>%
    summarize(m = min(Epoch.Date.Time.f)) %>%
    mutate(start_weekday = weekdays(m),
           disp_date = as.Date(m),
           start_weekday = paste0(start_weekday, ', ', disp_date)) %>%
    select(real_dayno, start_weekday)
  plt_d <- left_join(plt_d, t, by = 'real_dayno')

  plt_d$dayno <- plt_d$eff_dayno; plt_d$eff_dayno  <- NULL

  # adjust marker ends when the algorithm doesnt span more than 1 day
  aenaod <- function(df, col1) {
    wc <- colnames(df) == col1
    b4_noon <- as.numeric(substr(df$algo.Stop, 12, 13)) < 12
    hours <- as.numeric(substr(df[,wc], 12, 13))
    mins <- as.numeric(substr(df[,wc], 15, 16))
    after_noon_marker <- (hours + (mins / 60)) > 12

    times_over <- unique(df[,wc][which(after_noon_marker & b4_noon)])

    for(z in seq_along(times_over)) {
      ref_time <- ap(paste0( as.Date(times_over[z]), ' 12:01:00'))
      times_to_fix <- seq(ref_time, ap(times_over[z]), 60)
      df[df$Epoch.Date.Time.f %in% times_to_fix, wc] <- times_over[z]
    }
    df[, wc]
  }

  ac_sz <- m_sz <- l_sz <- d_sz <- 1

  ynm <- 'Lux'
  lg_nm <- 'log10(Light)'
  ynm <- 'log10(Activity)'


  mint <- substr(min(plt_d$Epoch.Date.Time.f), 12, 16)
  maxt <- substr(max(plt_d$Epoch.Date.Time.f), 12, 16)

  ynm <- 'Lux'
  lg_nm <- 'log10(Light)'

  plt_d <- plt_d %>% ungroup() %>% mutate(
    White.Light = log10(White.Light + 1),
    light1lux = log10(1 + 1),
    light1lux = light1lux / max(White.Light, na.rm = T) * max(Activity, na.rm = T),
    White.Light = White.Light / max(White.Light, na.rm = T) * max(Activity, na.rm = T)
  )

  ynm <- 'Activity'


  # mx <- plt_d %>% group_by(real_dayno) %>% summarise(mx = max((Activity), White.Light, na.rm = T))
  mx <- plt_d %>% summarise(mx = max((Activity), White.Light, na.rm = T))
  mx$max_point <- mx$mx + mx$mx*0.05; mx$mx <- NULL
  # plt_d <- plt_d %>% left_join(mx, by = 'real_dayno')
  plt_d$max_point <- mx$max_point


  ac_sz <-  m_sz <- l_sz <- d_sz <- 1
  ac_sz2 <-  m_sz2 <- l_sz2 <- d_sz2 <- 1

  to_mark <- plt_d #%>% filter(Type == 'MAIN')

  to_mark <- to_mark %>%
    mutate(actigraphy.Stop = aenaod(., 'actigraphy.Stop'),
           diary.Stop = aenaod(., 'diary.Stop'),
           marker.Stop = aenaod(., 'marker.Stop'),
           light.Stop = aenaod(., 'light.Stop'))


  to_mark$orig_Epoch.Date.Time.f <- to_mark$Epoch.Date.Time.f

  new_t <- function(df, c1) {

    df[,colnames(df)== c1] <- as.POSIXct(df[,colnames(df)== c1], tz = 'UTC')

    ref_time_df <- df %>% group_by(dayno) %>%
      summarise(min_time = min(orig_Epoch.Date.Time.f)) %>%
      mutate(hour = as.numeric(substr(as.character(min_time), 12, 13)),
             ref_time = if_else(hour >= 12,
                                as.POSIXct(paste0(as.Date(min_time), ' 12:01:00'), tz = 'UTC'),
                                as.POSIXct(paste0(as.Date(min_time)-1, ' 12:01:00'), tz = 'UTC'))) %>%
      select(dayno, ref_time, min_time) %>% ungroup()

    df_new <- df %>% left_join(ref_time_df, by = 'dayno') %>%
      mutate(time_d = (difftime(!!sym(c1), ref_time, units='secs')),
             new_ref_time = as.POSIXct(paste0(Sys.Date(), ' 12:01:00'), tz = 'UTC'),
             new_time = new_ref_time + time_d)


    return(df_new$new_time)
  }

  to_mark <- to_mark %>% mutate(Epoch.Date.Time.f = new_t(.,'Epoch.Date.Time.f'))


  to_mark$algo_start  <- to_mark$algo.Start
  to_mark$algo_stop <- to_mark$algo.Stop
  to_mark$algo_start <- new_t(to_mark, 'algo_start')
  to_mark$algo_stop <- new_t(to_mark, 'algo_stop')

  # to_mark %>% select(dayno, real_dayno, algo_start, algo_stop) %>% unique()


  slp_int <- to_mark %>% ungroup() %>% #filter(dayno == 1, Epoch.Date.Time.f >= as.POSIXct('2023-03-23 17:25:00', tz='UTC')) %>%
    group_by(dayno, algo_start) %>%
    summarize(sleep_start = min(Epoch.Date.Time.f[Interval.Status == 'REST-S' & Epoch.Date.Time.f >= algo_start], na.rm=T),
              sleep_stop = max(Epoch.Date.Time.f[Interval.Status == 'REST-S' & Epoch.Date.Time.f <= algo_stop], na.rm=T)) %>%
    quiet1()
  slp_int <- slp_int[!is.na(slp_int$algo_start),]
  slp_int <- slp_int[nchar(slp_int$sleep_start) >= 10 | nchar(slp_int$sleep_stop) >= 10,]
  # slp_int
  # slp_int <- slp_int %>%
  #   mutate(algo_start = if_else(algo_start <= as.POSIXct(paste0(Sys.Date(), ' 12:00:01'), tz='UTC'),
  #                                                    sleep_start, algo_start))

  # to_mark %>% filter(Epoch.Date.Time.f >= slp_int$algo_start[1], dayno == 1) %>%
  #   select(Epoch.Date.Time.f, Interval.Status, Interval.Status.OLD, algo.Start)

  natime <- as.POSIXct(NA,tz='UTC')
  fix_stop_m <- function(x,l,r) {
    if_else(x < l, natime, if_else(x > r, natime, x))
  }
  fix_stop_int <- function(x,l,r) {
    if_else(x < l, natime, if_else(x > r, r, x))
  }
  fix_start_int <- function(x,l,r) {
    if_else(x < l, natime, if_else(x > r, r, x))
  }

  temp_datat <- to_mark %>%
    mutate(actigraphy.Start = new_t(.,'actigraphy.Start'),
           marker.Start = new_t(.,'marker.Start'),
           light.Start = new_t(.,'light.Start'),
           diary.Start = new_t(.,'diary.Start'),
           actigraphy.Stop = new_t(.,'actigraphy.Stop'),
           marker.Stop =  new_t(.,'marker.Stop'),
           light.Stop =  new_t(.,'light.Stop'),
           diary.Stop =  new_t(.,'diary.Stop')
    ) %>%
    left_join(slp_int, by = c('dayno', 'algo_start')) %>%
    mutate(right_edge = as.POSIXct('12:00:00', format = '%H:%M:%S', tz='UTC') + 24*60*60,
           left_edge = as.POSIXct('12:01:00', format = '%H:%M:%S', tz='UTC'),
           Epoch.Date.Time.f = if_else(dayno == real_dayno & Epoch.Date.Time.f > right_edge,
                                       natime, Epoch.Date.Time.f),
           Epoch.Date.Time.f = if_else(dayno < real_dayno & Epoch.Date.Time.f > right_edge,
                                       Epoch.Date.Time.f - 24*60*60, Epoch.Date.Time.f),
           actigraphy.Stop = if_else(dayno != real_dayno, actigraphy.Stop - 24*60*60, actigraphy.Stop),
           marker.Stop = if_else(dayno != real_dayno, marker.Stop - 24*60*60, marker.Stop),
           light.Stop = if_else(dayno != real_dayno, light.Stop - 24*60*60, light.Stop),
           diary.Stop = if_else(dayno != real_dayno, diary.Stop - 24*60*60, diary.Stop),
           algo_stop = if_else(dayno != real_dayno, algo_stop - 24*60*60, algo_stop),
           sleep_stop = if_else(dayno != real_dayno, sleep_stop - 24*60*60, sleep_stop),

           algo_start = if_else(dayno != real_dayno, left_edge, algo_start),
           sleep_start = if_else(dayno != real_dayno, left_edge, sleep_start),

           actigraphy.Stop = fix_stop_m(actigraphy.Stop, left_edge, right_edge),
           marker.Stop = fix_stop_m(marker.Stop, left_edge, right_edge),
           light.Stop = fix_stop_m(light.Stop, left_edge, right_edge),
           diary.Stop = fix_stop_m(diary.Stop, left_edge, right_edge),
           algo_stop = fix_stop_int(algo_stop, left_edge, right_edge),
           sleep_stop = fix_stop_int(sleep_stop, left_edge, right_edge),

           actigraphy.Start = if_else(dayno != real_dayno, natime, actigraphy.Start),
           marker.Start = if_else(dayno != real_dayno, natime, marker.Start),
           light.Start = if_else(dayno != real_dayno, natime, light.Start),
           diary.Start = if_else(dayno != real_dayno, natime, diary.Start)
    )


  # temp_datat %>% select(dayno, real_dayno, algo_start, algo_stop, sleep_start, sleep_stop) %>% unique()

  filter_marker <- function(df, x)  {
    if_else(is.na(df$sleep_start) & is.na(df$sleep_stop) &
              !is.na(df[,colnames(df)==x]), df$sleep_start, df[,colnames(df)==x])
  }
  if(filter_naps) {
    print('FILTERING')
    temp_datat <- temp_datat %>%
      mutate(algo_start = filter_marker(.,'algo_start'),
             algo_stop = filter_marker(.,'algo_stop'),
             actigraphy.Start = filter_marker(.,'actigraphy.Start'),
             actigraphy.Stop = filter_marker(.,'actigraphy.Stop'),
             marker.Start = filter_marker(.,'marker.Start'),
             marker.Stop = filter_marker(.,'marker.Stop'),
             light.Stop = filter_marker(.,'light.Stop'),
             light.Start = filter_marker(.,'light.Start'),
             diary.Start = filter_marker(.,'diary.Start'),
             diary.Stop = filter_marker(.,'diary.Stop'))
  }

  if(sum(colnames(temp_datat) == 'Off.Wrist.Status') == 0) {
    temp_datat$Off.Wrist.Status <- 0
  }
  temp_datat <- temp_datat %>%
    mutate(off_wrist = ifelse(Off.Wrist.Status == 1 & ## SHOULD THIS BE HERE? ID 120 day 11
                                Interval.Status == 'EXCLUDED'  , 'OFF WRIST', 'OW'),
           na_entries = ifelse(is.nan(Activity) | is.nan(White.Light) , 'NAEN', 'OW')) %>%
    group_by(real_dayno) %>%
    mutate(n_on_wrist = sum(off_wrist != 'OFF WRIST' | na_entries != 'NAEN')) %>%
    ungroup() %>% filter(n_on_wrist > 0) %>%
    arrange(real_dayno, Epoch.Date.Time.f)

  owrle <- rle(temp_datat$off_wrist)
  woff <- which(owrle$values == 'OFF WRIST')
  ow_end <- cumsum(owrle$lengths)[woff]
  if(length(ow_end) > 0) ow_end <- ifelse(ow_end < nrow(temp_datat), ow_end + 1, ow_end) # added plus one??
  ow_start <- cumsum(owrle$lengths)[woff - 1] + 1
  if(length(ow_start) < length(ow_end)) ow_start <- c(1, ow_start)
  temp_datat$ow_start <- temp_datat$ow_stop <- na
  for(z in seq_along(ow_start)) {
    # print(paste('dayno:', temp_datat$dayno[ow_start[z]],
    #       ' starting: ', temp_datat$orig_Epoch.Date.Time.f[ow_start[z]],
    #       ' ending: ',  temp_datat$orig_Epoch.Date.Time.f[ow_end[z]]))
    temp_datat$ow_start[ow_start[z]:ow_end[z]] <- temp_datat$orig_Epoch.Date.Time.f[ow_start[z]]
    temp_datat$ow_stop[ow_start[z]:ow_end[z]] <- temp_datat$orig_Epoch.Date.Time.f[ow_end[z]]
  }

  owrle <- rle(temp_datat$na_entries)
  woff <- which(owrle$values == 'NAEN')
  ow_end <- cumsum(owrle$lengths)[woff]
  if(length(ow_end) > 0) ow_end <- ifelse(ow_end < nrow(temp_datat), ow_end + 1, ow_end) # added plus one??
  ow_start <- cumsum(owrle$lengths)[woff - 1] + 1
  if(length(ow_start) < length(ow_end)) ow_start <- c(1, ow_start)
  temp_datat$na_start <- temp_datat$na_stop <- na
  for(z in seq_along(ow_start)) {
    temp_datat$na_start[ow_start[z]:ow_end[z]] <- temp_datat$orig_Epoch.Date.Time.f[ow_start[z]]
    temp_datat$na_stop[ow_start[z]:ow_end[z]] <- temp_datat$orig_Epoch.Date.Time.f[ow_end[z]]
  }

  temp_datat <- temp_datat %>%
    # filter(orig_Epoch.Date.Time.f > ap('2021-02-28 12:00:00'),
    #                     orig_Epoch.Date.Time.f < ap('2021-03-01 12:00:00')) %>%
    as.data.frame() %>%
    mutate(stop_hr = as.numeric(substr(ow_stop, 12, 13)),
           start_hr = as.numeric(substr(ow_start, 12, 13)),
           ow_stop = new_t(.,'ow_stop'),
           ow_start = new_t(.,'ow_start'),
           na_stop = new_t(.,'na_stop'),
           na_start = new_t(.,'na_start'),
           # ow_stop = if_else(stop_hr >= 12 & start_hr < 12, ow_stop - 24*60*60, ow_stop),
           # ow_start = if_else(stop_hr >= 12 & start_hr < 12, left_edge, ow_start),
           ow_start = if_else(ow_start < left_edge, left_edge, ow_start),
           ow_stop = if_else(ow_stop > right_edge , right_edge, ow_stop),
           na_start = if_else(na_start < left_edge, left_edge, na_start),
           na_stop = if_else(na_stop > right_edge , right_edge, na_stop))# %>%
  # ow_stop = if_else(ow_stop < ow_start, ow_stop + 24*60*60, ow_stop)) %>%
  # select(dayno, ow_start, ow_stop, start_hr, stop_hr, left_edge, right_edge)# %>% unique()

  temp_datat %>% arrange(real_dayno, Epoch.Date.Time.f) %>%
    select(Date, Time, Epoch.Date.Time.f, Activity, dayno,real_dayno) %>%
    group_by(real_dayno) %>% slice(1) %>% ungroup


  col_vals <- c('black'='black', 'blue' = 'blue', "chocolate4"="chocolate4",
                "green" ="green", 'orange'=rgb(250/255, 204/255, 2/255),
                'red'='red')
  col_labels = c('Activity', "Marker Push", "Diary Marker", "Light Marker",
                 'Light', "Activity Marker")
  temp_datat <- temp_datat %>% filter(real_dayno >= dmin, real_dayno <= dmax) %>% unique()
  min_date_str <- temp_datat$Date[1]
  max_date_str <- temp_datat$Date[nrow(temp_datat)]
  if(nrow(temp_datat) == 0) return(NULL)

  {temp_datat %>%
      ggplot() +
      theme_bw() +
      facet_wrap(~real_dayno, ncol = 1, scales = 'free_x', strip.position = "right") -> pt1#, scales = 'free_y') -> pt1
    if(nrow(flags) > 0) {
      flags_dx <- flags %>% filter(day_excl == 1)
      if(flags %>% filter(flag == 1) %>% nrow()) {
        pt1 <- pt1 + geom_rect(
          aes(xmin = as.POSIXct('12:01:00', format = '%H:%M:%S', tz='UTC'),
              xmax = as.POSIXct('12:00:00', format = '%H:%M:%S', tz='UTC') + 60*60*24,
              ymin = -Inf, ymax = Inf),
          data.frame(real_dayno = flags %>% filter(flag == 1) %>% pull(dayno)),
          fill = rgb(234/255, 234/255, 180/255))
      }
      if(nrow(flags_dx) > 0) {
        pt1 <- pt1 + geom_rect(
          aes(xmin = as.POSIXct('12:01:00', format = '%H:%M:%S', tz='UTC'),
              xmax = as.POSIXct('12:00:00', format = '%H:%M:%S', tz='UTC') + 60*60*24,
              ymin = -Inf, ymax = Inf),
          data.frame(real_dayno = flags_dx$dayno), fill = rgb(255/255, 182/255, 193/255))
      }
    }


    pt1 +
      geom_vline(xintercept = seq(as.POSIXct('12:00:00', format = '%H:%M:%S', tz='UTC'),
                                  as.POSIXct('12:00:00', format = '%H:%M:%S', tz='UTC')+60*60*24,by=30*60),
                 col = rgb(216/255, 216/255, 216/255), lwd = 0.5)+
      geom_vline(xintercept = seq(as.POSIXct('12:15:00', format = '%H:%M:%S', tz='UTC'),
                                  as.POSIXct('11:45:00', format = '%H:%M:%S', tz='UTC')+60*60*24,by=30*60),
                 col = rgb(216/255, 216/255, 216/255), lwd = 0.25) +
      geom_rect(data = temp_datat %>% filter(TYPE == 'MAIN'), aes(xmin=algo_start, xmax=algo_stop, ymin=-Inf, ymax=Inf),
                fill = rgb(185/255, 246/255, 247/255)) +
      geom_rect(data = temp_datat %>% filter(TYPE == 'MAIN'), aes(xmin=sleep_start, xmax=sleep_stop, ymin=-Inf, ymax=Inf),
                fill = rgb(142/255, 221/255, 241/255)) +
      geom_rect(data = temp_datat %>% filter(TYPE == 'NAP'), aes(xmin=algo_start, xmax=algo_stop, ymin=-Inf, ymax=Inf),
                fill = rgb(199/255, 104/255, 255/255)) +
      geom_rect(data = temp_datat %>% filter(TYPE == 'NAP'), aes(xmin=sleep_start, xmax=sleep_stop, ymin=-Inf, ymax=Inf),
                fill = rgb(139/255, 73/255, 178/255)) +
      geom_rect(aes(xmin=na_start, xmax=na_stop, ymin=-Inf, ymax=Inf), fill = rgb(131/255, 131/255, 131/255)) +
      geom_rect(aes(xmin=ow_start, xmax=ow_stop, ymin=-Inf, ymax=Inf), fill = rgb(5/255, 1/255, 144/255)) +
      geom_col(aes(x = Epoch.Date.Time.f + 60*epoch_length/2, y = Activity, color = "black"), show.legend = F,
               width = 60*epoch_length) +
      geom_line(aes(x = Epoch.Date.Time.f, y = White.Light, color = "orange"), show.legend = F) +
      geom_hline(aes(yintercept = light1lux), color = "grey20", lty = 3) +
      geom_point(aes(x = (actigraphy.Start), y = max_point, color = "red", fill = "red"), pch = 25, size = 3, show.legend = F) +
      geom_point(aes(x = (marker.Start), y = max_point*3/6, color = "blue", fill = "blue", alpha = as.character(press_flag)), pch = 25, size = 3, show.legend = F) +
      scale_alpha_manual(values = c('1' = 0.25, '0' = 1)) +
      geom_point(aes(x = (light.Start), y = max_point*5/6, color = "green", fill = "green"), pch = 25, size = 3, show.legend = T) +
      geom_point(aes(x = (diary.Start), y = max_point*4/6, color = "chocolate4", fill = "chocolate4"), pch = 25, size = 3, show.legend = T) +
      geom_point(aes(x = (actigraphy.Stop), y = max_point, color = "red", fill = "red"), pch = 25, size = 3, show.legend = T) +
      geom_point(aes(x = (marker.Stop), y = max_point*3/6, color = "blue", fill = "blue"), pch = 25, size = 3, show.legend = T) +
      geom_point(aes(x = (light.Stop), y = max_point*5/6, color = "green", fill = "green"), pch = 25, size = 3, show.legend = T) +
      geom_point(aes(x = (diary.Stop), y = max_point*4/6, color = "chocolate4", fill = "chocolate4"), pch = 25, size = 3, show.legend = T) +
      geom_text(aes(x = as.POSIXct('12:01:00', format = '%H:%M:%S', tz='UTC'),
                    y = max_point*5.5/6, label = start_weekday), size = 4, hjust = 0, vjust = 0, col = "chocolate4") +
      labs(y = ynm, x= "Time", title = paste0("ID = ", sd_data$ID[1], '; ', min_date_str, ' to ', max_date_str)) +
      scale_color_manual(name = '', values = col_vals, labels = col_labels, na.value=rgb(1,1,1,0),
                         guide = guide_legend(override.aes = list(size=10, pch = 15))) +
      scale_fill_manual(name = '', values = col_vals, labels = col_labels, na.value=rgb(1,1,1,0),
                        guide = FALSE) +
      scale_x_datetime(date_breaks = c(db), date_labels = "%H:%M",
                       expand = expansion(mult = 0.01), timezone='UTC',
                       limits = c(as.POSIXct('12:01:00', format = '%H:%M:%S', tz='UTC'),
                                  as.POSIXct('12:00:00', format = '%H:%M:%S', tz='UTC') + 24*60*60)) +
      theme(legend.position = "bottom") + guides(alpha = "none") } -> start_main_fig

  return(start_main_fig)
}





