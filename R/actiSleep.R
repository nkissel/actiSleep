
#' actiSleep algorithm
#'
#' This function implements the actiSleep algorithm. All of the data.frame
#' arguments are coming in similar format as output by `read_actigraphy()`.
#'
#' @param epoch_df a data frame of epoch-level data
#' @param diary_df a data frame of diary data
#' @param stats_df a data frame of summary statistics
#' @param marker_df a data frame of button pushes
#' @param time_start a `POSIXct` object of the start time
#' @param time_stop a `POSIXct` object of the stop time
#' @return
#'
actiSleep <- function(
    epoch_df, diary_df, stats_df, marker_df, time_start = NULL, time_stop = NULL) {

  span1 <- 0.1
  epochs <- epoch_df

  # clean epoch level data
  epochs$Interval.Status <- ifelse(is.na(epochs$Interval.Status), 'EXCLUDED', epochs$Interval.Status)

  # Making sure to remove duplicate rows. These can appear if you're merging
  # data from multiple actigraphy files.
  keepfunc <- function(x) {
    ret <- rep(0, length(x))
    if(length(x) > 1) {
      if('REST-S' %in% x) {
        ret[x == 'REST-S'][1] <- 1
      } else {
        if('REST' %in% x) {
          ret[x == 'REST'][1] <- 1
        } else {
          if('ACTIVE' %in% x) {
            ret[x == 'ACTIVE'][1] <- 1
          } else {
            ret[x == 'EXCLUDED'][1] <- 1
          }
        }

      }
    } else {
      ret <- 1
    }
    return(ret)
  }

  epochs <- epochs %>% arrange(Epoch.Date.Time.f, Interval.Status)
  rw2keep <- epochs %>% select(Epoch.Date.Time.f, Interval.Status) %>%
    group_by(Epoch.Date.Time.f) %>%
    reframe(Interval.Status = Interval.Status, keep = keepfunc(Interval.Status)) %>%
    pull(keep)
  epochs <- epochs[rw2keep == 1,] %>% unique()

  # Mark invalid epochs and remove any duplicates (there shouldn't be any)
  epochs <- epochs %>% arrange(Epoch.Date.Time.f) %>%
    mutate(is_invalid = is.nan(Activity) | Interval.Status == 'EXCLUDED')
  epochs <- epochs[!duplicated(epochs),] %>% as.data.frame()

  min_tz <- format(as.POSIXct(as.character(min(epochs$Epoch.Date.Time.f)), tz = 'America/New_York'), "%Z")

  ##############################################################################
  #  4) Get dayno for epoch data
  ##############################################################################
  acti_start_date <- epochs$Epoch.Date.Time.f
  if(length(acti_start_date) == 0) {
    return(NULL)
  }
  # acti_start_date <- as.POSIXct(acti_start_date, format = '%m/%d/%Y %I:%M:%S %p', tz='UTC')

  acti_start_date <- as.POSIXct(acti_start_date, format = '%Y-%m-%d %H:%M:%S', tz='UTC')
  acti_start_date <- min(acti_start_date, na.rm = T)
  anchor_date <- acti_start_date #- (24*60*60) # start the day before, because diary reports night before
  anchor_date <- paste0(substr(anchor_date, 1, 10), ' 12:00:00 UTC')
  epochs <- add_dayno(epochs, "Epoch.Date.Time.f", anchor_date)


  ##############################################################################
  #  5) Clean Sleep Diary
  ##############################################################################

  cleaned_sleep_diary <- add_dayno(diary_df, "diary_start", anchor_date) %>%
    mutate(diary.Duration = as.vector(difftime(diary_end, diary_start, units = 'mins')),
           Type = NA) %>%
    rename(diary.Start = diary_start, diary.Stop = diary_end)

  markers <- marker_df %>% select(ID, Marker.Date.Time.f) %>% unique() %>%
    as.data.frame()
  if(ncol(markers) == 1) {
    markers <- markers %>% mutate(Marker.Date.Time.f = NA)
  }

  ##############################################################################
  #  6) Clean actigraphy defined rest
  ##############################################################################
  sub_set_cols <- c("ID", "Interval.", "Start.Date.Time.f", "End.Date.Time.f", "Duration")
  stats <- stats_df %>% as.data.frame()

  stats_f <- add_dayno(stats, "Start.Date.Time.f", anchor_date)
  stats_f <- stats_f %>% group_by(dayno) %>%
    mutate(duration_custom = difftime(End.Date.Time.f, Start.Date.Time.f, units='mins'),
           Inv.Time.AC2 = ifelse(is.na(Inv.Time.AC), duration_custom, Inv.Time.AC)) %>%
    summarise(tot_inv = sum(Inv.Time.AC2, na.rm = T)) %>%
    right_join(stats_f, by = 'dayno') %>%
    mutate(invalid_flag = ifelse(Inv.Time.AC >= 15 | tot_inv >= 60*4, 1, 0))

  # add invalid flag to summary info
  rest_data <- stats_f %>%
    mutate(invalid_flag = ifelse(Inv.Time.AC >= 15 | tot_inv >= 60*4, 1, 0)) %>%
    filter(Interval.Type == "REST") %>%
    dplyr::select(all_of(c(sub_set_cols, 'invalid_flag'))) %>% as.data.frame()

  if(nrow(rest_data) == 0) {
    return(NULL)
  }

  # remove days if not in time interval
  if(!is.null(time_start)) {
    if(!is.null(time_stop)) {
      rest_data <- rest_data %>% filter(Start.Date.Time.f >= time_start,
                                        Start.Date.Time.f < time_stop)
    } else {
      rest_data <- rest_data %>% filter(Start.Date.Time.f >= time_start)
    }
  }
  if(is.null(time_start) & !is.null(time_stop)) {
    rest_data <- rest_data %>% filter(Start.Date.Time.f < time_stop)
  }

  # get invalid day information
  invalid_info <- stats_f %>%  select(dayno, tot_inv) %>% unique()
  invalid_info_slp <- stats_f %>% filter(Interval.Type == "SLEEP") %>% group_by(dayno) %>%
    summarize(sleep_invalid = sum(Inv.Time.AC, na.rm = T))
  invalid_info <- invalid_info %>% left_join(invalid_info_slp, by = 'dayno') %>%
    mutate(sleep_invalid = ifelse(is.na(sleep_invalid), 0, sleep_invalid))
  invalid_info <- invalid_info %>%
    mutate(invalid_flag = ifelse(tot_inv >= 60*4 | sleep_invalid >= 15, 1, 0)) %>%
    select(dayno, invalid_flag)
  invalid_info <- invalid_info

  initial_rest <- add_dayno(rest_data, "Start.Date.Time.f", anchor_date)

  epochs_f <- add_dayno(epochs, "Epoch.Date.Time.f", anchor_date)
  epochs_f <- fix_afternoon_rest(epochs_f, initial_rest, "End.Date.Time.f")
  epoch_length <- table(difftime(lead(epochs_f$Epoch.Date.Time.f), epochs_f$Epoch.Date.Time.f, units = 'mins'))
  epoch_length <- as.numeric(names(epoch_length)[which.max(epoch_length)])


  rm(epochs_f)

  rest_data <- initial_rest %>%
    mutate(Duration = as.numeric(difftime(End.Date.Time.f, Start.Date.Time.f, units = 'mins')))

  # 6.a) Making day number index
  rest <- add_dayno(rest_data, "Start.Date.Time.f", anchor_date) %>%
    relocate(ID, dayno) %>% arrange(Start.Date.Time.f)

  ### Engines and Cabooses
  # 6.b) find naps, and mark if they should be merged into any MAIN sleeps
  nap_merges <- find_nearby_rest(rest, 60)

  # 6.c) update the sleep / merge in NAPs that need merging and merge in cleaned sleep diary data
  # + find nocturnal awakening and merge
  updated_sleep <- nap_merges %>% update_sleep2() %>% find_nocturnal_awakening() %>%
    update_sleep2() %>% left_join(invalid_info, by = 'dayno') %>% unique()
  # Updated actigraphy rest-intervals where naps are merged in if they should be.



  #########################################
  #### 7) Merge Actigraphy & sdAM/sdPM ####
  #########################################
  # 7. merging in the cleaned sleep diary data into the updated actigraphy data
  # make sure there aren't any repeat MAIN diary entries

  match_diary_act <- function(df1, df2) {
    tmid <- function(x, y) {
      x + abs(difftime(y, x, units = 'sec')/2)
    }
    index_tib <- df1 %>% ungroup() %>% mutate(rwn_act = seq_len(n())) %>%
      select(dayno, rwn_act) %>%
      full_join(
        df2 %>% ungroup() %>% mutate(rwn_dia = seq_len(n())) %>%
          select(dayno, rwn_dia), by = 'dayno')
    df3 <- data.frame(
      ID = df1$ID[1], dayno = index_tib$dayno,
      updated.Start = df1$updated.Start[index_tib$rwn_act],
      updated.End = df1$updated.End[index_tib$rwn_act],
      diary.Start = df2$diary.Start[index_tib$rwn_dia],
      diary.End = df2$diary.Stop[index_tib$rwn_dia]) %>%
      mutate(
        updated.Mid = tmid(updated.Start, updated.End),
        diary.Mid = tmid(diary.Start, diary.End),
        abs_mid_diff = as.numeric(abs(difftime(updated.Mid, diary.Mid, units = 'hours'))),
        abs_mid_diff = ifelse(is.na(abs_mid_diff), 10000, abs_mid_diff))

    df3 <- df3 %>% group_by(updated.Start, updated.End) %>%
      mutate(winning_match1 = abs_mid_diff == min(abs_mid_diff)) %>% ungroup() %>%
      group_by(diary.Start, diary.End) %>%
      mutate(winning_match2 = abs_mid_diff == min(abs_mid_diff)) %>% ungroup()

    na <- as.POSIXct(NA, tz = 'UTC')
    df3 <- df3 %>% mutate(
      updated.Start = if_else(winning_match1, updated.Start, na),
      updated.End = if_else(winning_match1, updated.End, na),
      diary.Start = if_else(winning_match2, diary.Start, na),
      diary.End = if_else(winning_match2, diary.End, na)
    )
    narows = df3 %>% select(updated.Start, updated.End, diary.Start, diary.End) %>%
      is.na() %>% rowSums == 4
    df3 <- df3[!narows,] %>% select(ID, dayno, updated.Start, updated.End,
                                    diary.Start, diary.Stop = diary.End)
    df3 <- df3 %>%
      mutate(inter1 = diary.Start >= updated.Start & diary.Start <= updated.End |
               diary.Stop <= updated.End  & diary.Stop >= updated.Start |
               diary.Start <= updated.Start & diary.Stop >= updated.End) %>%
      mutate(diary.Start = if_else(!inter1, na, diary.Start),
             diary.Stop = if_else(!inter1, na, diary.Stop))
    return(df3)
  }

  if(nrow(cleaned_sleep_diary) > 0) {
    sleep_w_diary_short <- match_diary_act(updated_sleep, cleaned_sleep_diary)
  } else {
    sleep_w_diary_short <- updated_sleep %>%
      select(ID, dayno, updated.Start, updated.End) %>%
      mutate(diary.Start = as.POSIXct(NA, tz='UTC'), diary.Stop = as.POSIXct(NA, tz='UTC'))
  }
  sleep_w_diary_short <- sleep_w_diary_short %>% filter(!is.na(diary.Start) | !is.na(updated.Start))


  # no longer using full join that uses TYPE
  na <- as.POSIXct(NA, tz = 'UTC')
  updated_sleep_w_diary <- updated_sleep %>% select(-invalid_flag) %>%
    left_join(sleep_w_diary_short, by = c('ID', 'dayno', 'updated.Start', 'updated.End')) %>%
    arrange(dayno, updated.Start) %>%
    mutate(diary.Duration = as.numeric(abs(difftime(diary.Stop, diary.Start, units = 'mins'))))
  updated_sleep_w_diary <- updated_sleep_w_diary %>%
    left_join(invalid_info, by = 'dayno')

  #####################################
  ####### 8)  CLEAN MARKERS     #######
  #####################################
  # 8.a) Making day number index
  markers2 <- add_dayno(markers, "Marker.Date.Time.f", anchor_date)
  markers2$ID <- as.numeric(markers2$ID)
  updated_sleep_w_diary$ID <- as.numeric(updated_sleep_w_diary$ID)
  # 8.b) Find markers
  # Function to extract markers that fall into 30 minutes window around the rest
  # interval start times identified in updated_sleep_w_diary
  # Note that this function needs sleep diary data as well just in case we
  # do not have actigraphy rest interval time (e.g. for naps)
  marker_data <- suppressWarnings(suppressMessages(
    unique(find_markers(updated_sleep_w_diary, markers2, window = 60))))

  ########################################
  ### 9) Merge Actigraphy & sdAM/sdPM ####
  ########################################
  # Merge Actigraphy rests, sleep diary, and marker data
  # This is important because we'll use start times based on actigraphy,
  # sleep diary, and markers to get light data
  actigraphy_diary <- suppressMessages(join_actigraphy_diary_marker(updated_sleep_w_diary, marker_data))
  actigraphy_diary$Type <- NA

  #####################################
  ######## 10)   WHITE LIGHT    #######
  #####################################
  #### Getting White Light Readings, which live in epoch-level data
  # 10.a) First get day number for epoch data
  epochs_f <- add_dayno(epochs, "Epoch.Date.Time.f", anchor_date)
  # browser()
  epochs_f <- fix_afternoon_rest(epochs_f, updated_sleep, "updated.End")

  # 10.b) get light markers
  all_day_with_markers <- function(epochs_f, actigraphy_diary) {
    # sd_data <- left_join(epochs_f, actigraphy_diary, by = c('ID', 'dayno'))
    na <-  as.POSIXct(NA, tz = 'UTC')
    sd_data <- epochs_f
    ccols <- ncol(sd_data)
    wcadd <- !colnames(actigraphy_diary) %in% colnames(sd_data)
    sd_data_add <- as.data.frame(matrix(nrow = nrow(sd_data), ncol = sum(wcadd)))
    colnames(sd_data_add) <- colnames(actigraphy_diary)[wcadd]
    sd_data <- cbind(sd_data, sd_data_add) %>%
      mutate(actigraphy.Start = na, actigraphy.Stop = na,
             marker.Start = na, marker.Stop = na,
             diary.Start = na, diary.Stop = na)
    for(j in 1:nrow(actigraphy_diary)) {
      temp <- actigraphy_diary[j,]
      wc <- which(sd_data$Epoch.Date.Time.f >= temp$actigraphy.Start &
                    sd_data$Epoch.Date.Time.f <= temp$actigraphy.Stop)
      sd_data[wc, (ccols+1):ncol(sd_data)] <- temp[,-(1:2)]
    }

    sd_data <- sd_data %>% arrange(Epoch.Date.Time.f)

    return(sd_data)
  }
  sd_data <- all_day_with_markers(epochs_f, actigraphy_diary)

  actigraphy_diary_light_marker <- find_light(
    epochs_f, actigraphy_diary, epoch_length, n = 30,
    search_window = 120, span1 = span1)

  marker_data <- suppressMessages(suppressWarnings(
    find_markers_acdl(actigraphy_diary_light_marker %>%
                        select(-marker.Start, -marker.Stop), markers2, 60)))
  marker_data <- unique(marker_data)

  actigraphy_diary_light_marker <- actigraphy_diary_light_marker %>% mutate(Interval.f = seq_len(n())) %>%
    select(-marker.Start, -marker.Stop) %>%
    full_join(marker_data) %>% suppressMessages()

  gtr <- function(x, y) {
    ifelse(is.na(x) & !is.na(y), F,
           ifelse(is.na(y) & !is.na(x), F, x > y))
  }
  ep_factor <- epoch_length

  all_markers <- actigraphy_diary_light_marker %>% #filter(Type == 'MAIN') %>%
    select(dayno, Type, actigraphy.Start, actigraphy.Stop,
           diary.Start, diary.Stop,
           marker.Start, marker.Stop,
           light.Start, light.Stop, invalid_flag) %>% unique()
  return(list(all_markers = all_markers, epochs_f = epochs_f,
              ep_factor = ep_factor,
              invalid_info = invalid_info))

  # return(list(epochs = epochs, markers = markers, cleaned_sleep_diary = cleaned_sleep_diary))
}
