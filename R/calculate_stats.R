# all_markers, epochs_f,  ep_factor, watch, dir = 'summary_dataV/'

calculate_stats <- function(all_markers, epochs_f, ep_factor) {

  get_starts_ends <- function(df) {
    # browser()
    df$rwn <- 1:nrow(df)
    rle1 <- rle(df$Interval.Status)
    data.frame(rle1$lengths, rle1$values)

    r_which_a <- which(rle1$values == 'ACTIVE')

    r_which_ae <- which(rle1$values %in% c('ACTIVE', 'EXCLUDED'))
    row_rest_start <- cumsum(rle1$lengths)[r_which_ae] + 1
    row_rest_start <- na.omit(ifelse(row_rest_start > nrow(df), NA, row_rest_start))
    row_rest_end <- cumsum(rle1$lengths)[r_which_ae - 1]
    if(length(row_rest_start) > length(row_rest_end)) row_rest_end = c(row_rest_end, nrow(df))

    we_start <- which(df$Interval.Status[row_rest_start] %in% c('EXCLUDED', 'ACTIVE'))
    if(length(we_start) > 0) {
      row_rest_start <- row_rest_start[-we_start]
    }

    we_end <- which(df$Interval.Status[row_rest_end] %in% c('EXCLUDED', 'ACTIVE'))
    if(length(we_end) > 0) {
      row_rest_end <- row_rest_end[-we_end]
    }

    # r_which_s <- which(rle1$values == 'REST-S')
    # row_sleep_start <- cumsum(rle1$lengths)[r_which_s - 1] + 1
    # row_sleep_end <- cumsum(rle1$lengths)[r_which_s]
    # browser()
    row_sleep_start <- row_sleep_end <- NULL
    for(i in seq_along(row_rest_start)) {
      df$rwn[row_rest_start[i]:row_rest_end[i]]
      df_slp <- df %>% slice(row_rest_start[i]:row_rest_end[i]) %>%
        filter(Interval.Status == 'REST-S')
      if(nrow(df_slp) > 0) {
        df_slp <- df_slp %>%
          summarise(slp_start_i = min(rwn),
                    slp_end_i = max(rwn))
        row_sleep_start[i] <- df_slp$slp_start_i
        row_sleep_end[i] <- df_slp$slp_end_i
      } else {
        row_sleep_start[i] <- NA
        row_sleep_end[i] <- NA
      }

    }

    if(length(r_which_a) > 0) {
      if(r_which_a[1] == 1) {
        row_active_start <- c(1, cumsum(rle1$lengths)[r_which_a - 1] + 1)
      } else {
        row_active_start <- cumsum(rle1$lengths)[r_which_a - 1] + 1
      }
      row_active_end <- cumsum(rle1$lengths)[r_which_a]
      r2 <- data.frame(row_active_start, row_active_end)
    } else {
      r2 <- data.frame(matrix(nrow = 0, ncol = 2))
      colnames(r2) <- c('row_active_start', 'row_active_end')
    }

    # rs <- data.frame(dayno = df$dayno[row_rest_start], row_rest_start)
    # re <- data.frame(dayno = df$dayno[row_rest_end], row_rest_end)
    #
    # ss <- data.frame(dayno = df$dayno[row_sleep_start], row_sleep_start)
    # se <- data.frame(dayno = df$dayno[row_sleep_end], row_sleep_end)
    #
    # as <- data.frame(dayno = df$dayno[row_active_start], row_active_start)
    # ae <- data.frame(dayno = df$dayno[row_active_end], row_active_end)


    # if(length(row_rest_start) > length(row_sleep_start)) {
    #   for(i in 1:length(row_rest_start)) {
    #     sum((row_rest_start[i] <= row_sleep_start) &
    #       (row_rest_end[i] >= row_sleep_start), na.rm = T) -> num_sleep_btwn
    #     if(num_sleep_btwn == 0) {
    #       all_b4 <- row_sleep_start[row_sleep_start < row_rest_start[i]]
    #       all_af <- row_sleep_start[row_sleep_start > row_rest_start[i]]
    #       row_sleep_start <- c(all_b4, -1, all_af)
    #
    #       all_b4 <- row_sleep_end[row_sleep_end < row_rest_end[i]]
    #       all_af <- row_sleep_end[row_sleep_end > row_rest_end[i]]
    #       row_sleep_end <- c(all_b4, -1, all_af)
    #
    #       print(i)
    #     }
    #   }
    #   row_sleep_start <- ifelse(row_sleep_start == -1, NA, row_sleep_start)
    #   row_sleep_end <- ifelse(row_sleep_end == -1, NA, row_sleep_end)
    # }

    if(row_rest_end[1] < row_rest_start[1]) {
      row_rest_end <- row_rest_end[-1]
    }

    r1 <- data.frame(row_rest_start, row_rest_end,
                     row_sleep_start, row_sleep_end)
    return(list(r1, r2))
  }

  diff_mins <- function(x) {
    difftime(x[-1], x[-length(x)], units = 'mins')
  }

  # df is epoch level data
  # new_m are the new rest markers
  adjust_ends <- function(df, new_m) {
    df <- unique(df)
    df <- df %>% arrange(Epoch.Date.Time.f)
    df$scored_Interval.Status <- df$Interval.Status

    stendl <- get_starts_ends(df)
    stend <- stendl[[1]]


    # new_m <- new_m %>% mutate(Epoch.Date.Time.f = time_orig) %>%
    #   left_join(df %>% select(Epoch.Date.Time.f, Line), by = 'Epoch.Date.Time.f') %>%
    #   select(-Epoch.Date.Time.f)

    avail_days <- unique(new_m$dayno)
    new_m <- new_m %>% ungroup() %>% arrange(time_orig) %>% filter(pos == 'START') %>%
      mutate(interval_id = seq_len(n())) %>% rbind(
        new_m %>% ungroup() %>% arrange(time_orig) %>% filter(pos == 'STOP') %>%
          mutate(interval_id = seq_len(n()))
      ) %>% arrange(time_orig)
    intid <- unique(new_m$interval_id)

    for(i in seq_along(intid)) {
      # df_days <- df$dayno[stend$row_rest_start]
      # df_days_inds <- which(df_days %in% avail_days)
      ind1 <- i

      cr <- new_m %>% filter(interval_id == intid[i], pos == 'START')
      if(substr(paste(cr$time_orig), 18, 19) == '30') {
        cr$time_orig <- cr$time_orig + 30
      }

      ind_new_start <- which(df$Epoch.Date.Time.f == cr$time_orig)
      # dft <- df[ind_new_start,]
      # ind_new_start <- na.omit(ifelse(dft$day_end_limit < dft$day_begin_limit, NA, ind_new_start))
      # dft <- df[ind_new_start,]
      # ind_new_start <- na.omit(ifelse(difftime(dft$day_end_limit,
      #                                          dft$day_begin_limit, units='hours') < 4 &
      #                                   i != 1 & i != length(intid), NA, ind_new_start))

      stend_ind <- which.min(abs(stend$row_rest_start - ind_new_start))

      if(ind_new_start < stend$row_rest_start[stend_ind]) {
        df$scored_Interval.Status[ind_new_start:(stend$row_rest_start[stend_ind] - 0)] <- 'REST'
      } else if(ind_new_start > stend$row_rest_start[stend_ind]) {
        df$scored_Interval.Status[(stend$row_rest_start[stend_ind]):(ind_new_start) - 1] <- 'ACTIVE'
      }

      cr <- new_m %>% filter(interval_id == intid[i], pos == 'STOP')
      if(substr(paste(cr$time_orig), 18, 19) == '30') {
        cr$time_orig <- cr$time_orig + 30
      }
      ind_new_stop <- which(df$Epoch.Date.Time.f == cr$time_orig)
      # dft <- df[ind_new_stop,]
      # ind_new_stop <- na.omit(ifelse(dft$day_end_limit < dft$day_begin_limit, NA, ind_new_stop))
      # dft <- df[ind_new_stop,]
      # ind_new_stop <- na.omit(ifelse(difftime(dft$day_end_limit,
      #                                          dft$day_begin_limit, units='hours') < 4 &
      #                                   i != 1 & i != length(intid), NA, ind_new_stop))
      if(length(ind_new_stop) == 0) {
        ind_new_stop <- nrow(df) # THIS IS SUPER SKETCHY AND MAYBE THE WRONG THING TO DO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      }

      if(ind_new_stop < stend$row_rest_end[stend_ind]) {
        df$scored_Interval.Status[ind_new_stop:(stend$row_rest_end[stend_ind] - 0)] <- 'ACTIVE'
      } else if(ind_new_start > stend$row_rest_end[stend_ind]) {
        df$scored_Interval.Status[(stend$row_rest_end[stend_ind]):(ind_new_stop - 1)] <- 'REST'
      }
    }
    return(df)
  }

  df <- epochs_f
  df$Interval.Status.OLD <- df$Interval.Status
  for(i in 1:nrow(all_markers)) {
    df$Interval.Status[(df$Epoch.Date.Time.f >= all_markers$algo.Start[i]) &
                         (df$Epoch.Date.Time.f <= all_markers$algo.Stop[i]) & (
                           (df$Interval.Status == 'ACTIVE') | (df$Interval.Status == 'EXCLUDED') )] <- 'REST'
  }

  for(i in 1:(nrow(all_markers)+1)) {
    start1 <- min(df$Epoch.Date.Time.f) - 60
    stop1 <- max(df$Epoch.Date.Time.f) + 60
    if(i > 1) {
      start1 <- all_markers$algo.Stop[i-1]
    }
    if(i <= nrow(all_markers)) {
      stop1 <- all_markers$algo.Start[i]
    }
    df$Interval.Status[(df$Epoch.Date.Time.f > start1) &
                         (df$Epoch.Date.Time.f < stop1) &
                         (df$Interval.Status %in% c('REST', 'REST-S'))] <- 'ACTIVE'
  }
  # table( df$Interval.Status,  df$Interval.Status.OLD)
  # df %>% filter(Interval.Status == 'ACTIVE', Interval.Status.OLD == 'REST-S') %>%
  #   pull(Epoch.Date.Time.f)

  dfo <- df
  new_m1_start <- all_markers %>% select(dayno, algo.Start, Type, invalid_flag) %>%
    rename(time_orig = algo.Start) %>% mutate(pos = 'START')
  new_m1_stop <- all_markers %>% select(dayno, algo.Stop, Type, invalid_flag) %>%
    rename(time_orig = algo.Stop) %>% mutate(pos = 'STOP')
  new_m1 <- rbind(new_m1_start, new_m1_stop) %>% arrange(time_orig)






  to_adjust <- c('DURATION', 'ACTOT', 'ACMNMIN', 'ACINVAL',
                 'ACINVALPERC', 'ACINVSW', 'ACINVSWPERC', 'ACSL',
                 'SNOOZE', 'ACSE', 'ACSE_SLEEP', 'ACWASO', 'ACWKTOT', 'ACMWKDUR',
                 'ACSLPTOT', 'ACBOUTOT', 'ACMNSLBT', 'ACIIMNUM', 'ACIMPCNT',
                 'ACIMMTOT', 'ACMINBT', 'ACMBTOT', 'ACMBPCNT', 'ACMBLNUM',
                 'ACMNBOT', 'ACFRAGID', 'TOTEXP', 'AVGLITE', 'MAXLITE', 'TALT',
                 'INVLITE', 'INVLITEP', 'SLP_ONSET', 'SLP_OFFSET')



  get_m_bouts <- function(ws) {
    wa <- which(rle(ws > 0)$values == TRUE)
    sum(rle(ws > 0)$lengths[wa] >= 1)
  }

  get_im_bouts <- function(ws) {
    wa <- which(rle(ws == 0)$values == TRUE)
    sum(rle(ws == 0)$lengths[wa] >= 1)
  }

  get_wk_bouts <- function(ws) {
    wa <- which(rle(ws)$values == 1)
    sum(rle(ws)$lengths[wa] >= 1)
  }

  get_sl_bouts <- function(ws) {
    wa <- which(rle(ws)$values == 0)
    sum(rle(ws)$lengths[wa] >= 1)
  }

  get_m_bouts_time <- function(ws) {
    wa <- which(rle(ws > 0)$values == TRUE)
    mean(rle(ws > 0)$lengths[wa])
  }

  get_im_bouts_time <- function(ws) {
    wa <- which(rle(ws == 0)$values == TRUE)
    mean(rle(ws == 0)$lengths[wa])
  }

  get_wk_bouts_time <- function(ws) {
    wa <- which(rle(ws)$values == 1)
    mean(rle(ws)$lengths[wa])
  }

  get_sl_bouts_time <- function(ws) {
    wa <- which(rle(ws)$values == 0)
    mean(rle(ws)$lengths[wa])
  }

  get_m_bouts_1min <- function(ws) {
    wa <- which(rle(ws > 0)$values == TRUE)
    sum(rle(ws > 0)$lengths[wa] == 1)
  }

  get_im_bouts_1min <- function(ws) {
    wa <- which(rle(ws == 0)$values == TRUE)
    sum(rle(ws == 0)$lengths[wa] == 1)
  }

  get_all_stats <- function(df){
    # browser()
    # if(watch == 'Spectrum Plus') {
    #   ac_thres <- 800
    #   wake_thres <- 40
    # } else {
    #   print('WATCH NOT SUPPORTED')
    # }

    stendl <- get_starts_ends(df)
    stend <- stendl[[1]]
    ac_stend <- stendl[[2]]
    df$rest_group <- 0
    df$sleep_group <- 0
    df$ac_group <- 0

    rst <- stend$row_rest_start
    ren <- stend$row_rest_end
    sst <- stend$row_sleep_start
    sen <- stend$row_sleep_end
    ast <- ac_stend$row_active_start
    aen <- ac_stend$row_active_end

    # df[ac_stend$row_active_start, ]

    for(i in seq_len(nrow(stend))) {
      df$rest_group[rst[i]:ren[i]] <- i
      if(!is.na(sst[i])) {
        df$sleep_group[sst[i]:sen[i]] <- i
      }
      if(length(ast) > 0) {
        if(length(ast) >= i) {
          df$ac_group[ast[i]:aen[i]] <- i
        }
      } else {
        df$ac_group <- NA
      }
    }

    for(i in seq_len(nrow(ac_stend))) {
      df$ac_group[ast[i]:aen[i]] <- i
    }



    calculate_stats <- function(grp, interval_type) {
      ret1 <- df %>% group_by(across(grp)) %>%
        summarise(dayno = min(dayno),
                  DURATION = difftime(max(Epoch.Date.Time.f), min(Epoch.Date.Time.f), units='mins'),
                  start = min(Epoch.Date.Time.f), end = max(Epoch.Date.Time.f),
                  ACTOT = sum(Activity, na.rm=T),
                  ACINVAL = sum(is.nan(Activity))*ep_factor,
                  ACWASO = sum(Sleep.Wake, na.rm=T)*ep_factor,
                  ACINVSW = sum(is.nan(Sleep.Wake), na.rm=T)*ep_factor,
                  ACWKTOT = get_wk_bouts(Sleep.Wake),
                  ACMWKDUR = get_wk_bouts_time(Sleep.Wake),
                  ACBOUTOT = get_sl_bouts(Sleep.Wake),
                  ACMNSLBT = get_sl_bouts_time(Sleep.Wake),
                  ACIIMNUM = sum(Activity == 0, na.rm=T)*ep_factor,
                  ACIMMTOT = get_im_bouts(Activity),
                  ACMINBT = get_im_bouts_time(Activity),
                  ACMBTOT = sum(Activity > 0)*ep_factor,
                  ACMBLNUM = get_m_bouts(Activity),
                  ACMNBOT = get_m_bouts_time(Activity),
                  ACIMMIN = get_im_bouts_1min(Activity)*ep_factor, #maybe wrong
                  ACMINPCT = get_im_bouts_1min(Activity) * 100 /
                    (get_im_bouts_1min(Activity) + get_m_bouts_1min(Activity)),
                  TOTEXP = sum(White.Light)*ep_factor,
                  AVGLITE = mean(White.Light),
                  MAXLITE = max(White.Light),
                  TALT = sum(White.Light > 1000 | is.na(White.Light))*ep_factor,
                  INVLITE = sum(is.nan(White.Light)))
      ret1 <- ret1 %>%
        mutate(ACIMPCNT = ACIIMNUM / as.vector(DURATION) * 100,
               ACMBPCNT = ACMBTOT / as.vector(DURATION) * 100,
               ACFRAGID = ACMBPCNT + ACIMMIN,
               ACSLPTOT = as.vector(DURATION) - ACWASO,
               ACSLPCNT = ACSLPTOT / (as.vector(DURATION) - ACINVAL) * 100,
               ACMNMIN = ACTOT / as.vector(DURATION) ,
               PERCINVAL = ACINVAL / as.vector(DURATION) * 100,
               PERCINVSW = ACINVSW / as.vector(DURATION) * 100,
               INVLITEP = INVLITE / as.vector(DURATION) * 100,
               INTERVAL = interval_type)
      wrm <- which(ret1[,colnames(ret1) == grp] == 0)
      ret1 <- ret1[-wrm,]
      if(interval_type == 'ACTIVE') {
        ret1$ACWASO <- NULL
        ret1$ACSLPTOT <- NULL
        ret1$ACSLPCNT <- NULL
      } else if(interval_type == 'REST') {
        ret1$ACSLPTOT <- NULL
      }
      return(ret1)
    }

    rest_df <- calculate_stats('rest_group', 'REST')
    sleep_df <- calculate_stats('sleep_group', 'SLEEP')
    active_df <- calculate_stats('ac_group', 'ACTIVE')



    calculate_SE <- function(sleep_df, rest_df) {
      # browser()
      sleep_df$ACSE <- sleep_df$ACSL <- sleep_df$SNOOZE <- sleep_df$ACSE_SLEEP <- NA
      rest_df$ACSLPTOT <- rest_df$ACSE <- rest_df$ACSL <- rest_df$SNOOZE <- rest_df$ACSE_SLEEP <- NA
      rest_df$SLP_ONSET <- rest_df$SLP_OFFSET <- as.POSIXct(NA, tz = 'UTC')
      for(i in 1:nrow(sleep_df)) {
        w <- which(rest_df$start <= sleep_df$start[i] & rest_df$end >= sleep_df$end[i])
        if(length(w) > 0) {
          sleep_df$ACSE[i] <- sleep_df$ACSLPTOT[i] / as.vector(rest_df$DURATION[w] - rest_df$ACINVSW[w]) * 100
          sleep_df$ACSE_SLEEP[i] <- sleep_df$ACSLPTOT[i] / as.vector(sleep_df$DURATION[i] - sleep_df$ACINVSW[i]) * 100
          sleep_df$ACSL[i] <- as.numeric(difftime(sleep_df$start[i], rest_df$start[w], units = 'mins'))
          sleep_df$SNOOZE[i] <- as.numeric(difftime(rest_df$end[w], sleep_df$end[i], units = 'mins'))
          # browser()

          rest_df$ACSE[w] <- sleep_df$ACSLPTOT[i] / as.vector(rest_df$DURATION[w] - rest_df$ACINVSW[w]) * 100
          rest_df$ACSE_SLEEP[w] <- sleep_df$ACSLPTOT[i] / as.vector(sleep_df$DURATION[i] - sleep_df$ACINVSW[i]) * 100
          rest_df$ACSL[w] <- as.numeric(difftime(sleep_df$start[i], rest_df$start[w], units = 'mins'))
          rest_df$SNOOZE[w] <- as.numeric(difftime(rest_df$end[w], sleep_df$end[i], units = 'mins'))
          rest_df$ACWASO[w] <- sleep_df$ACWASO[i]

          rest_df$SLP_ONSET[w] <- sleep_df$start[i]
          rest_df$SLP_OFFSET[w] <- sleep_df$end[i]

          rest_df$ACSLPTOT[w] <- sleep_df$ACSLPTOT[i]
        }
      }
      return(list(sleep_df, rest_df))
    }
    se_list <- calculate_SE(sleep_df, rest_df)
    sleep_df <- se_list[[1]]
    rest_df <- se_list[[2]]

    rest_sleep_df <- dplyr::bind_rows(rest_df, sleep_df)

    # lat <- rest_sleep_df %>% group_by(dayno) %>%
    #   summarise(SNOOZE = difftime(max(end, na.rm=T), min(end, na.rm=T), units='mins'))
    # lat$INTERVAL <- 'SLEEP'
    # rest_sleep_df <- left_join(rest_sleep_df, lat, by = c('dayno', 'INTERVAL'))
    active_rest_sleep_df <- dplyr::bind_rows(active_df, rest_sleep_df)

    return(active_rest_sleep_df)
  }

  # df <- adjust_ends(dfo, new_m1)
  # df$Interval.Status <- df$scored_Interval.Status
  get_all_stats(df) -> df2

  nms <- to_adjust[to_adjust %in% colnames(df2)]
  df2 <- df2 %>% select(c('start', 'end', 'dayno', 'INTERVAL', nms)) %>% arrange(start)
  df2$algorithm <- 1
  df2$version <- 1
  df2$timestamp <- Sys.time()

  df2 <- left_join(df2, new_m1 %>% select(dayno, invalid_flag) %>% unique())
  id <- unique(df$id)[1]
  if(length(id) == 0) {
    id <- unique(df$ID)[1]
  }
  df2 <- df2 %>% mutate(ID = id) %>% relocate(ID)

  return(df2)

}

