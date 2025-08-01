
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
#' @param max_invalid_time maximum number of invalid hours per day; if over, the
#' data for a given day will be flagged as being "invalid."
#' @return list of data.frames for marker start/stop times, epoch-level data,
#' epoch length, and invalid/removed day information.
#' @import dplyr
#' @import lubridate
#' @export
actiSleep <- function(
    epoch_df, diary_df, stats_df, marker_df, time_start = NULL,
    time_stop = NULL, max_invalid_time = 4, add_na_tails = F,
    light_window = 120, push_window = 60) {

  stopifnot(
    "epoch_df must be a data.frame" = is.data.frame(epoch_df),
    "diary_df must be a data.frame" = is.data.frame(diary_df),
    "stats_df must be a data.frame" = is.data.frame(stats_df),
    "marker_df must be a data.frame" = is.data.frame(marker_df),
    "time_start must be POSIXct or NULL" = is.data.frame(time_start),
    "time_stop must be POSIXct or NULL" = is.data.frame(time_stop)
  )

  filter_time <- function(df, time_start, time_stop, var_name) {
    if(!is.null(time_start)) {
      if(!is.null(time_stop)) {
        df <- df %>% filter(!!sym(var_name) >= time_start,
                            !!sym(var_name) < time_stop)
      } else {
        df <- df %>% filter(!!sym(var_name) >= time_start)
      }
    }
    if(is.null(time_start) & !is.null(time_stop)) {
      df <- df %>% filter(!!sym(var_name) < time_stop)
    }
    return(df)
  }

  span1 <- 0.1
  epochs <- epoch_df

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

  epochs <- epochs %>% arrange(Epoch.Date.Time.f, Interval.Status) %>%
    mutate(Interval.Status = ifelse(is.na(Interval.Status), 'EXCLUDED', Interval.Status))
  rw2keep <- epochs %>% select(Epoch.Date.Time.f, Interval.Status) %>%
    group_by(Epoch.Date.Time.f) %>%
    reframe(Interval.Status = Interval.Status, keep = keepfunc(Interval.Status)) %>%
    pull(keep)
  epochs <- epochs %>% slice(which(rw2keep == 1)) %>% unique()

  # Mark invalid epochs and remove any duplicates (there shouldn't be any)
  epochs <- epochs %>% arrange(Epoch.Date.Time.f) %>%
    mutate(is_invalid = is.nan(Activity) | Interval.Status == 'EXCLUDED')
  epochs <- epochs %>% slice(which(!duplicated(.))) %>% as.data.frame()
  epochs <- filter_time(epochs, time_start, time_stop, 'Epoch.Date.Time.f')

  if(nrow(epochs) == 0) return(NULL)
  min_tz <- format(as.POSIXct(as.character(min(epochs$Epoch.Date.Time.f)), tz = 'America/New_York'), "%Z")

  ##############################################################################
  #  1) Get dayno for epoch data
  ##############################################################################
  acti_start_date <- epochs$Epoch.Date.Time.f
  if(length(acti_start_date) == 0) {
    return(NULL)
  }
  acti_start_date <- as.POSIXct(acti_start_date, format = '%Y-%m-%d %H:%M:%S', tz='UTC')
  acti_start_date <- min(acti_start_date, na.rm = T)
  anchor_date <- acti_start_date #- (24*60*60) # start the day before, because diary reports night before
  anchor_date <- paste0(substr(anchor_date, 1, 10), ' 12:00:00 UTC')
  epochs_f <- add_dayno(epochs, "Epoch.Date.Time.f", anchor_date)

  ##############################################################################
  #  2) Clean diary and marker data + add dayno to diary
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
  #  3) Add data to tails to populate full day + fill in gaps with missing data
  ##############################################################################
  epoch_length <- table(difftime(lead(epochs$Epoch.Date.Time.f), epochs$Epoch.Date.Time.f, units = 'mins'))
  epoch_length <- as.numeric(names(epoch_length)[which.max(epoch_length)])

  if(add_na_tails) {
    epochs_f <- epochs_f %>% arrange(Epoch.Date.Time.f)

    # FILL INSIDE
    all_time <- seq(min(epochs_f$Epoch.Date.Time.f, na.rm = T),
                    max(epochs_f$Epoch.Date.Time.f, na.rm = T), epoch_length*60)
    time_fill <- all_time[!(all_time %in% epochs_f$Epoch.Date.Time.f)]
    if(length(time_fill) > 0) {
      time_fill <- data.frame(ID = epochs_f$ID[1],
                              Epoch.Date.Time.f = time_fill, Off.Wrist.Status = 1, Activity = NaN,
                              Sleep.Wake = NaN, White.Light = NaN, Interval.Status = 'EXCLUDED')
      epochs_f <- bind_rows(epochs_f, time_fill) %>% arrange(Epoch.Date.Time.f)
      epochs_f <- add_dayno(epochs_f, "Epoch.Date.Time.f", anchor_date)

    }

    # FILL TAILS
    min_time <- epochs_f %>% filter(Epoch.Date.Time.f == min(Epoch.Date.Time.f)) %>% slice(1)
    max_time <- epochs_f %>% filter(Epoch.Date.Time.f == max(Epoch.Date.Time.f)) %>% slice(1)
    new_min <- as.Date(min_time$Epoch.Date.Time.f) + hours(12) + minutes(1)
    new_max <- as.Date(max_time$Epoch.Date.Time.f) + hours(12)
    if(min_time$Epoch.Date.Time.f < new_min) {
      new_min <- new_min - days(1)
    }
    if(max_time$Epoch.Date.Time.f > new_max) {
      new_max <- new_max + days(1)
    }


    all_time <- seq(new_min, new_max, epoch_length*60)
    time_fill <- all_time[!(all_time %in% epochs_f$Epoch.Date.Time.f)]
    if(length(time_fill) > 0) {
      time_fill <- data.frame(ID = epochs_f$ID[1],
                              Epoch.Date.Time.f = time_fill, Off.Wrist.Status = 1, Activity = NaN,
                              Sleep.Wake = NaN, White.Light = NaN, Interval.Status = 'EXCLUDED')
      epochs_f <- bind_rows(epochs_f, time_fill) %>% arrange(Epoch.Date.Time.f)
      epochs_f <- add_dayno(epochs_f, "Epoch.Date.Time.f", anchor_date)

    }
  }

  ##############################################################################
  #  4) Generate invalid falg
  ##############################################################################
  # first invalid because of max_invalid_time
  if('invalidepoch' %in% colnames(epochs_f)) { # for GGIR compatibility
    old_activity <- epochs_f %>% pull(Activity)
    old_sw <- epochs_f %>% pull(Sleep.Wake)
    epochs_f <- epochs_f %>% mutate(
      Sleep.Wake = ifelse(invalidepoch == 1, NA, Sleep.Wake),
      Activity = ifelse(invalidepoch == 1, NA, Activity)
    )
  }

  invalid_info_day <- epochs_f %>% group_by(dayno) %>% summarize(
    total_time = n(),
    invalid_activity = sum(is.na(Activity)) * epoch_length,
    invalid_sw = sum(is.na(Sleep.Wake)) * epoch_length,
    invalid_4hr = ifelse(invalid_activity >= 60*max_invalid_time, 1, 0))

  # second because 15 minutes of invalid sw
  stats <- stats_df %>% as.data.frame()
  stats <- filter_time(stats, time_start, time_stop, 'Start.Date.Time.f')
  stats_f <- add_dayno(stats, "Start.Date.Time.f", anchor_date) %>%
    filter(Interval.Type %in% c('ACTIVE', 'REST', 'SLEEP', 'EXCLUDED')) %>%
    mutate(dayno = as.numeric(dayno))
  if(!'Inv.Time.SW' %in% colnames(stats_f)) {
    sleep_stats <- stats_f %>% filter(Interval.Type == "SLEEP")
    slp_days <- sort(unique(sleep_stats$dayno))
    sleep_invalidv <- NULL
    for(i in seq_along(unique(slp_days))) {
      sleep_statst <- sleep_stats %>% filter(dayno == slp_days[i])
      invt <- NULL
      for(j in seq_len(nrow(sleep_statst))) {
        invt[j] <- epochs_f %>% filter(Epoch.Date.Time.f >= sleep_statst$Start.Date.Time.f[j],
                            Epoch.Date.Time.f < sleep_statst$End.Date.Time.f[j]) %>%
          summarize(si = sum(is.na(Sleep.Wake))) %>% pull(si)
      }
      sleep_invalidv[i] <- sum(invt)*epoch_length
    }
    invalid_info_slp <- data.frame(dayno = slp_days, sleep_invalid = sleep_invalidv)
  } else {
    invalid_info_slp <- stats_f %>% filter(Interval.Type == "SLEEP") %>% group_by(dayno) %>%
      summarize(sleep_invalid = sum(Inv.Time.SW, na.rm = T))
  }

  # merge invalid data
  invalid_info <- invalid_info_day %>% select(dayno, invalid_4hr) %>%
    full_join(invalid_info_slp) %>% mutate(
      sleep_invalid = ifelse(is.na(sleep_invalid), 0, sleep_invalid),
      sleep_invalid = sleep_invalid > 15,
      invalid_flag = ifelse(invalid_4hr + sleep_invalid > 0, 1, 0)) %>%
    select(dayno, invalid_flag)

  # attach invalid flag to statistics data
  stats_f <- left_join(stats_f, invalid_info, by = 'dayno')

  if('invalidepoch' %in% colnames(epochs_f)) { # for GGIR compatibility
    epochs_f <- epochs_f %>% mutate(Sleep.Wake = old_sw, Activity = old_activity)
  }

  ##############################################################################
  #  5) Extract rest data
  ##############################################################################
  sub_set_cols <- c("ID", "Interval.", "Start.Date.Time.f", "End.Date.Time.f", "Duration")
  rest_data <- stats_f %>% filter(Interval.Type == "REST") %>%
    select(all_of(c(sub_set_cols, 'invalid_flag'))) %>% as.data.frame()
  initial_rest <- add_dayno(rest_data, "Start.Date.Time.f", anchor_date)

  if(nrow(rest_data) == 0) {
    return(NULL)
  }

  rest_data <- initial_rest %>%
    mutate(Duration = as.numeric(difftime(End.Date.Time.f, Start.Date.Time.f, units = 'mins')))

  ##############################################################################
  #  6) Merge nearby rest(s)
  ##############################################################################
  # 6.a) Making day number
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

  ##############################################################################
  #### 7) Merge Actigraphy and diary
  ##############################################################################
  # 7. merging in the cleaned sleep diary data into the updated actigraphy data

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
  updated_sleep_w_diary <- updated_sleep_w_diary %>% left_join(invalid_info, by = 'dayno')

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
    unique(find_markers(updated_sleep_w_diary, markers2, window = push_window))))

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
    search_window = light_window, span1 = span1)

  marker_data <- suppressMessages(suppressWarnings(
    find_markers_acdl(actigraphy_diary_light_marker %>%
                        select(-marker.Start, -marker.Stop), markers2, push_window)))
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

  all_markers2 <- all_markers %>%
    mutate(diary.Start = remove_if_EXCLUDED(diary.Start, epochs_f),
           diary.Stop = remove_if_EXCLUDED(diary.Stop, epochs_f),
           marker.Start = remove_if_EXCLUDED(marker.Start, epochs_f),
           marker.Stop = remove_if_EXCLUDED(marker.Stop, epochs_f),
           light.Start = remove_if_EXCLUDED(light.Start, epochs_f),
           light.Stop = remove_if_EXCLUDED(light.Stop, epochs_f))

  all_markers2 <- algo_column(all_markers2[!is.na(all_markers2$actigraphy.Start), ])
  all_markers$algo.Start <- all_markers2$algo.Start
  all_markers$algo.Stop <- all_markers2$algo.Stop; rm(all_markers2)
  all_markers <- add_dayno(all_markers, 'algo.Start', anchor_date) # must do this in case onset crosses noon after selection!
  all_markers <- find_main(all_markers, 'algo.Start', 'algo.Stop') %>% ungroup() %>% as.data.frame()# redo MAIN/NAP

  # calculate stats
  auto_stats <- calculate_stats(all_markers, epochs_f,  ep_factor)
  id <- unique(epochs_f$ID)[1]

  ## GETTING FLAGS
  max_on_diff <- function(x){
    x <- abs(x)
    x <- na.omit(x)
    if(length(x) > 0) {
      max(x)
    } else {
      NA
    }
  }
  diffs1_start <- find_algo_times(
    diff_df_only = T, ac = all_markers$actigraphy.Start, m = all_markers$marker.Start,
    l = all_markers$light.Start, d = all_markers$diary.Start, s = 'start') %>%
    suppressMessages() %>% group_by(row) %>% summarize(start_md = max_on_diff(diff))
  diffs1_stop <- find_algo_times(
    diff_df_only = T, ac = all_markers$actigraphy.Stop, m = all_markers$marker.Stop,
    l = all_markers$light.Stop, d = all_markers$diary.Stop, s = 'stop', end_correction = T) %>%
    suppressMessages() %>% group_by(row) %>% summarize(stop_md = max_on_diff(diff))
  diff1 <-  full_join(diffs1_start, diffs1_stop, by = 'row') %>%
    mutate(max_diff = pmax(start_md, stop_md, na.rm = T)) %>% pull(max_diff)

  all_markers %>% select(algo.Start, algo.Stop)
  flag1 <- all_markers %>% ungroup() %>%
    mutate(max_diff = ifelse(is.na(diff1), 1e10, diff1)) %>%
    filter(Type == 'MAIN') %>%
    mutate(
      algo_d = difftime(algo.Stop, algo.Start, units = 'hours'),
      long = algo_d > 14 & max_diff > algo_d/3*60, # algo_d is in hours
      short = algo_d < 3 & max_diff > 60, # algo_d is in hours
    ) %>%
    left_join(auto_stats %>% filter(INTERVAL == 'REST') %>%  select(start, end, ACSL) %>%
                mutate(start = ap(start, tz = 'UTC'),
                       end = ap(end, tz = 'UTC')),
              by = c('algo.Start' = 'start', 'algo.Stop' = 'end')) %>%
    mutate(high_sl = ACSL >= 180) %>%
    select(
      dayno, Type, algo.Stop, algo.Start, short, long, high_sl)

  summary_flags <- all_markers %>% group_by(dayno) %>% arrange(algo.Start) %>% mutate(
    bd = difftime(lead(algo.Start), algo.Stop, units = 'hours'),
    bd = ifelse(is.na(bd), Inf, bd),
    md = if_else(is.finite(bd), algo.Stop + bd*60*60/2, as.POSIXct(NA, tz = 'UTC')),
    md = ifelse(is.na(md), 12, hour(md))) %>%
    # is_min = bd == min(bd, na.rm = T)) %>%
    select(dayno, Type, algo.Start, algo.Stop, bd, md) %>%
    # filter(is_min) %>%
    mutate(close_bouts = bd < 2 & (md >= 22 | md < 6) ) %>%
    select(dayno, close_bouts) %>% summarize(close_bouts = sum(close_bouts) > 0) %>% ungroup()

  get_mean_time <- function(df1, nm) {
    df1 %>% ungroup() %>% filter(Type == 'MAIN') %>%
      arrange(!!sym(nm)) %>% mutate(
        c1 = as.POSIXct(substr(!!sym(nm), 12, 19), tz = 'UTC', format = '%H:%M:%S'),
        md_st = mean(c1),
        dt_st = difftime(c1, md_st, units = 'hours'),
        c1 = if_else(
          dt_st < -12, c1 + hours(24),if_else(
            dt_st > 12, c1 - hours(24), c1)),
        dt_st = difftime(c1, md_st, units = 'hours'),
        mean_st = mean(c1),
        dt_st = difftime(c1, mean_st, units = 'mins'),
      ) %>% select(-c1, -md_st, -mean_st)
  }

  missing_sleep_flag <- epochs_f %>% select(dayno) %>% unique() %>%
    mutate(missing_sleep = !dayno %in% all_markers$dayno)

  flags <- full_join(flag1, summary_flags, by = 'dayno') %>%
    full_join(missing_sleep_flag, by = 'dayno') %>%
    full_join(invalid_info, by = 'dayno') %>%
    relocate(.after = missing_sleep, invalid_flag) %>%
    mutate(
      flag = short | long  | high_sl | close_bouts | missing_sleep,
      day_excl = invalid_flag == 1, across(5:12, as.numeric),
      across(5:12, function(x) ifelse(is.na(x), 0, x)), ID = id) %>%
    relocate(ID, dayno, Type, algo.Start, algo.Stop, flag) %>%
    arrange(ID, dayno)
  flags <- flags %>% mutate(
    flag = if_else(algo.Stop > time_stop, 1, flag),
    xnoon = if_else(algo.Stop > time_stop, 1, 0)
  ) %>% relocate(xnoon, .before = invalid_flag)
  # write.csv(flags, paste0(summary_save_dir, '/stats_', id, '.csv'))

  # EDIT EPOCHS_F INTERVAL STATUSES
  df <- epochs_f
  df$Interval.Status.OLD <- df$Interval.Status
  for(j in seq_len(nrow(all_markers))) {
    df$Interval.Status[(df$Epoch.Date.Time.f >= (all_markers$algo.Start[j])) &
                         (df$Epoch.Date.Time.f <= (all_markers$algo.Stop[j])) & (
                           (df$Interval.Status == 'ACTIVE') |
                             (df$Interval.Status == 'EXCLUDED')
                         )] <- 'REST'
  }
  for(j in seq_len(nrow(all_markers)+1)) {
    start1 <- min(df$Epoch.Date.Time.f) - 60
    stop1 <- max(df$Epoch.Date.Time.f) + 60
    if(j > 1) {
      start1 <- all_markers$algo.Stop[j-1]
    }
    if(j <= nrow(all_markers)) {
      stop1 <- all_markers$algo.Start[j]
    }
    df$Interval.Status[(df$Epoch.Date.Time.f > start1) &
                         (df$Epoch.Date.Time.f < stop1) &
                         (df$Interval.Status %in% c('REST', 'REST-S'))] <- 'ACTIVE'
  }
  epochs_f <- df


  return(list(all_markers = all_markers, epochs_f = epochs_f,
              stats = auto_stats, flags = flags,
              invalid_info = invalid_info,
              ep_factor = ep_factor))

  # return(list(epochs = epochs, markers = markers, cleaned_sleep_diary = cleaned_sleep_diary))
}
