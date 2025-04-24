#' Calculate statistics
#'
#' This function calculates statistics from marker and epoch data
#'
#' @param all_markers description
#' @param epochs_f description
#' @param ep_factor description
#' @import dplyr
#' @import lubridate
#' @export
calculate_stats <- function(all_markers, epochs_f, ep_factor) {
  # a comment
  find_chains <- function(intervals, interval_lvls, time) {
    grp = ifelse(intervals %in% interval_lvls, 1, 0)
    grp = ifelse(lag(grp, default = 0) == 1 & grp == 1, 0, grp)
    grp = cumsum(grp)
    grp = ifelse(intervals %in% interval_lvls, grp, NA)
    return(grp)
  }

  get_starts_ends <- function(df) {
    dfn <- df %>% mutate(
      slp_group = find_chains(Interval.Status, c('REST-S'), Epoch.Date.Time.f),
      rest_group = find_chains(Interval.Status, c('REST', 'REST-S'), Epoch.Date.Time.f),
      active_group = find_chains(Interval.Status, c('ACTIVE'), Epoch.Date.Time.f),
      excluded_group = find_chains(Interval.Status, c('EXCLUDED'), Epoch.Date.Time.f))

    # SLEEP INTERVALS MUST BE AT LEAST 15 MINUTES
    df1 <- dfn %>% filter(!is.na(slp_group)) %>% group_by(slp_group) %>%
      summarise(start = min(Epoch.Date.Time.f),
                stop = max(Epoch.Date.Time.f)) %>% mutate(INTERVAL = 'SLEEP') %>%
      mutate(d = difftime(stop, start, units = 'mins'),
             r = d >= 15) %>% filter(r) %>%
      mutate(slp_group = row_number()) %>% select(-d, -r)
    df2 <- dfn %>% filter(!is.na(rest_group)) %>% group_by(rest_group) %>%
      summarise(start = min(Epoch.Date.Time.f),
                stop = max(Epoch.Date.Time.f)) %>% mutate(INTERVAL = 'REST')
    df3 <- dfn %>% filter(!is.na(active_group)) %>% group_by(active_group) %>%
      summarise(start = min(Epoch.Date.Time.f),
                stop = max(Epoch.Date.Time.f)) %>% mutate(INTERVAL = 'ACTIVE')
    df4 <- dfn %>% filter(!is.na(excluded_group)) %>% group_by(excluded_group) %>%
      summarise(start = min(Epoch.Date.Time.f),
                stop = max(Epoch.Date.Time.f)) %>% mutate(INTERVAL = 'EXCLUDED')

    sleep_rest <- df1 %>% mutate(
      linterval_s = interval(start, stop)
    ) %>% select(-slp_group) %>% cross_join(
      df2 %>% mutate(
        linterval_r = interval(start, stop)
      ) %>% select(-rest_group)
    ) %>% mutate(
      overlaps = int_overlaps(linterval_s, linterval_r)
    ) %>% select(-linterval_s, -linterval_r) %>%
      filter(overlaps)

    # WHAT TO DO IF THERE ARE 2 SLEEPS INSIDE ONE REST
    dup_both <- function(x) duplicated(x, fromLast = F) | duplicated(x, fromLast = T)
    wdup <- sleep_rest %>% select(start.y, stop.y, INTERVAL.y) %>% dup_both() %>% which()
    if(length(wdup) > 0) {
      sleep_rest_dup <- sleep_rest %>% slice(wdup)
      new_rows <- sleep_rest_dup %>% group_by(start.y, stop.y, INTERVAL.x, INTERVAL.y, overlaps) %>%
        summarise(start.x = min(start.x), stop.x = max(stop.x)) %>% ungroup()
      sleep_rest <- sleep_rest %>% slice(-wdup) %>% bind_rows(new_rows) %>%
        arrange(start.y, start.x)
    }

    df2_old <- df2 %>% select(-rest_group)

    df1 <- sleep_rest %>% select(start.x, stop.x, INTERVAL.x) %>%
      rename(start = start.x, stop = stop.x, INTERVAL = INTERVAL.x)
    df2 <- sleep_rest %>% select(start.y, stop.y, INTERVAL.y) %>%
      rename(start = start.y, stop = stop.y, INTERVAL = INTERVAL.y)

    bdd <- rbind(df2_old, df2)
    w_del <- which(!(duplicated(bdd, fromLast = T) | duplicated(bdd, fromLast = F)))
    if(length(w_del) > 0) {
      for(it0 in seq_along(w_del)) {
        df$Interval.Status[df$Epoch.Date.Time.f >= df2_old$start[w_del[it0]] &
                             df$Epoch.Date.Time.f <= df2_old$stop[w_del[it0]]] <- 'ACTIVE'
      }
      return(
        get_starts_ends(df)
      )
    }

    ret1 <- bind_rows(df1, df2, df3, df4) %>% select(INTERVAL, start, stop)
    return(ret1)
  }

  diff_mins <- function(x) {
    difftime(x[-1], x[-length(x)], units = 'mins')
  }

  # adjust the interval status for REST intervals, so they agree with markers
  # not doing this for sleep intervals; they get stitched by get_starts_ends()
  df <- epochs_f
  df$Interval.Status.OLD <- df$Interval.Status
  for(i in 1:nrow(all_markers)) {
    df$Interval.Status[(df$Epoch.Date.Time.f >= all_markers$algo.Start[i]) &
                         (df$Epoch.Date.Time.f < all_markers$algo.Stop[i]) & (
                           (df$Interval.Status == 'ACTIVE') | (df$Interval.Status == 'EXCLUDED') )] <- 'REST'
  }

  for(i in 1:(nrow(all_markers)+1)) {
    start1 <- min(df$Epoch.Date.Time.f) - ep_factor*60
    stop1 <- max(df$Epoch.Date.Time.f)
    if(i > 1) {
      start1 <- all_markers$algo.Stop[i-1]
    }
    if(i <= nrow(all_markers)) {
      stop1 <- all_markers$algo.Start[i]
    }
    df$Interval.Status[(df$Epoch.Date.Time.f >= start1) &
                         (df$Epoch.Date.Time.f < stop1) &
                         (df$Interval.Status %in% c('REST', 'REST-S'))] <- 'ACTIVE'
  }

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
    start_end_df <- get_starts_ends(df)

    make_longer <- function(x, df) {
      start <- df$start[x]
      end <- df$stop[x]
      if(ep_factor == 1) {
        bym <- '60 sec'
      } else if(ep_factor == 0.5) {
        bym <- '30 sec'
      }
      data.frame(
        time = seq(from = start, to = end, by = bym),
        row_id = x
      )
    }
    safe_rename <- function(old, new) {
      function(x) {
        if (length(x) == 0) return(character())
        ifelse(x == old, new, x)
      }
    }

    start_end_df_s <- bind_rows(
      lapply(seq_len(nrow(start_end_df %>% filter(INTERVAL == 'SLEEP'))),
             make_longer, start_end_df %>% filter(INTERVAL == 'SLEEP'))) %>%
      rename_with(safe_rename("row_id", "sleep_group"), everything())
    start_end_df_r <- bind_rows(
      lapply(seq_len(nrow(start_end_df %>% filter(INTERVAL == 'REST'))),
             make_longer, start_end_df %>% filter(INTERVAL == 'REST'))) %>%
      rename_with(safe_rename("row_id", "rest_group"), everything())
    start_end_df_a <- bind_rows(
      lapply(seq_len(nrow(start_end_df %>% filter(INTERVAL == 'ACTIVE'))),
             make_longer, start_end_df %>% filter(INTERVAL == 'ACTIVE'))) %>%
      rename_with(safe_rename("row_id", "ac_group"), everything())

    if(nrow(start_end_df_r) == 0) {
      start_end_df_r <- data.frame(time = as.POSIXct(NA, tz = 'UTC'), rest_group = NA)[-1,]
      start_end_df_s <- data.frame(time = as.POSIXct(NA, tz = 'UTC'), sleep_group = NA)[-1,]
    }

    group_df <- start_end_df_a %>% full_join(start_end_df_r, by = 'time') %>% full_join(start_end_df_s) %>%
      rename(Epoch.Date.Time.f = time)

    df <- left_join(df, group_df, by = 'Epoch.Date.Time.f') %>%
      mutate(sleep_group = ifelse(is.na(sleep_group), 0, sleep_group),
             rest_group = ifelse(is.na(rest_group), 0, rest_group),
             ac_group = ifelse(is.na(ac_group), 0, ac_group))

    calculate_stats <- function(grp, interval_type) {
      ret1 <- df %>% group_by(across(grp)) %>%
        summarise(dayno = min(dayno),
                  DURATION = difftime(max(Epoch.Date.Time.f) + ep_factor*60, min(Epoch.Date.Time.f), units='mins'),
                  start = min(Epoch.Date.Time.f), end = max(Epoch.Date.Time.f) + ep_factor*60,
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
        # ret1$ACSLPTOT <- NULL
      }
      return(ret1)
    }

    rest_df <- calculate_stats('rest_group', 'REST')
    sleep_df <- calculate_stats('sleep_group', 'SLEEP')
    active_df <- calculate_stats('ac_group', 'ACTIVE')

    calculate_SE <- function(sleep_df, rest_df) {
      # browser()
      sleep_df$ACSE <- sleep_df$ACSL <- sleep_df$SNOOZE <- sleep_df$ACSE_SLEEP <- NA
      rest_df$ACSE <- rest_df$ACSL <- rest_df$SNOOZE <- rest_df$ACSE_SLEEP <- NA
      rest_df$SLP_ONSET <- rest_df$SLP_OFFSET <- as.POSIXct(NA, tz = 'UTC')
      for(i in 1:nrow(sleep_df)) {
        w <- which(rest_df$start <= sleep_df$start[i] & rest_df$end >= sleep_df$end[i])
        if(length(w) > 0) {
          sleep_df$ACSE[i] <- sleep_df$ACSLPTOT[i] / as.vector(rest_df$DURATION[w] - rest_df$ACINVSW[w]) * 100
          sleep_df$ACSE_SLEEP[i] <- sleep_df$ACSLPTOT[i] / as.vector(sleep_df$DURATION[i] - sleep_df$ACINVSW[i]) * 100
          sleep_df$ACSL[i] <- as.numeric(difftime(sleep_df$start[i], rest_df$start[w], units = 'mins'))
          sleep_df$SNOOZE[i] <- as.numeric(difftime(rest_df$end[w], sleep_df$end[i], units = 'mins'))

          sleep_df$Type[i] <- rest_df$Type[w]

          rest_df$ACSE[w] <- sleep_df$ACSLPTOT[i] / as.vector(rest_df$DURATION[w] - rest_df$ACINVSW[w]) * 100
          rest_df$ACSE_SLEEP[w] <- sleep_df$ACSLPTOT[i] / as.vector(sleep_df$DURATION[i] - sleep_df$ACINVSW[i]) * 100
          rest_df$ACSL[w] <- as.numeric(difftime(sleep_df$start[i], rest_df$start[w], units = 'mins'))
          rest_df$SNOOZE[w] <- as.numeric(difftime(rest_df$end[w], sleep_df$end[i], units = 'mins'))
          rest_df$ACWASO[w] <- sleep_df$ACWASO[i]

          rest_df$SLP_ONSET[w] <- sleep_df$start[i]
          rest_df$SLP_OFFSET[w] <- sleep_df$end[i]

          # rest_df$ACSLPTOT[w] <- sleep_df$ACSLPTOT[i]
        }
      }
      return(list(sleep_df, rest_df))
    }
    se_list <- calculate_SE(sleep_df, rest_df)
    sleep_df <- se_list[[1]]
    rest_df <- se_list[[2]]

    rest_sleep_df <- dplyr::bind_rows(rest_df, sleep_df)
    active_rest_sleep_df <- dplyr::bind_rows(active_df, rest_sleep_df)

    return(active_rest_sleep_df)
  }

  df2 <- df %>% mutate(Activity = ifelse(is.na(Activity), 0, Activity)) %>% get_all_stats()
  df2 <- df2 %>% left_join(
    all_markers %>% select(Type, algo.Start, algo.Stop) %>% mutate(INTERVAL = 'REST'),
    by = c('start' = 'algo.Start', 'end' = 'algo.Stop', 'INTERVAL')
  ) %>% arrange(start)

  wr_m <- which(!is.na(df2$Type) & df2$INTERVAL == 'REST')
  for(it0 in wr_m) {
    sl <- which(df2$start >= df2$start[it0] & df2$end <= df2$end[it0] & df2$INTERVAL == 'SLEEP')
    df2$dayno[sl] <- df2$dayno[it0]
    df2$Type[sl] <- df2$Type[it0]
  }

  nms <- to_adjust[to_adjust %in% colnames(df2)]
  df2 <- df2 %>% select(c('start', 'end', 'dayno', 'INTERVAL', 'TYPE'='Type', nms)) %>% arrange(start)
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
