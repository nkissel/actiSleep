find_algo_times <- function(
    ac, m, l, d, s = 'start', end_correction = F, diff_df_only = F,
    exception_window = 120) {
  d <- ifelse(nchar(as.character(d)) == 10, paste0(d, " 00:00:00"), as.character(d))
  l <- ifelse(nchar(as.character(l)) == 10, paste0(l, " 00:00:00"), as.character(l))
  ac <- ifelse(nchar(as.character(ac)) == 10, paste0(ac, " 00:00:00"), as.character(ac))
  m <- ifelse(nchar(as.character(m)) == 10, paste0(m, " 00:00:00"), as.character(m))

  d <- as.POSIXct(d, tz = 'UTC')
  l <- as.POSIXct(l, tz = 'UTC')
  ac <- as.POSIXct(ac, tz = 'UTC')
  m <- as.POSIXct(m, tz = 'UTC')

  f <- function(x, y) {
    difftime(x, y, units = 'mins')
  }
  df <- data.frame(ac, m, l, d)
  df$row <- 1:nrow(df)
  diff_df <- data.frame(matrix(ncol = 4, nrow = 4*3*nrow(df)))
  colnames(diff_df) <- c('row', 'diff', 'm1', 'm2')
  diff_df[, 1] <- rep(1:nrow(df), 12)
  diff_df[, 2] <- c(f(df[,1], df[,2]), f(df[,2], df[,1]),
                    f(df[,1], df[,3]), f(df[,3], df[,1]),
                    f(df[,1], df[,4]), f(df[,4], df[,1]),
                    f(df[,2], df[,3]), f(df[,3], df[,2]),
                    f(df[,2], df[,4]), f(df[,4], df[,2]),
                    f(df[,3], df[,4]), f(df[,4], df[,3]))
  diff_df[, 3] <- c(rep('ac', nrow(df)), rep('m', nrow(df)),
                    rep('ac', nrow(df)), rep('l', nrow(df)),
                    rep('ac', nrow(df)), rep('d', nrow(df)),
                    rep('m', nrow(df)), rep('l', nrow(df)),
                    rep('m', nrow(df)), rep('d', nrow(df)),
                    rep('l', nrow(df)), rep('d', nrow(df)))
  diff_df[, 4] <- c(rep('m', nrow(df)), rep('ac', nrow(df)),
                    rep('l', nrow(df)), rep('ac', nrow(df)),
                    rep('d', nrow(df)), rep('ac', nrow(df)),
                    rep('l', nrow(df)), rep('m', nrow(df)),
                    rep('d', nrow(df)), rep('m', nrow(df)),
                    rep('d', nrow(df)), rep('l', nrow(df)))
  if(diff_df_only) {
    return(diff_df)
  }

  if(s == 'start') {
    marker_rank <- c('m', 'd', 'l', 'ac')
  } else  {
    marker_rank <- c('ac', 'l', 'm', 'd')
  }
  winpick <- function(x, y) {
    if(sum(x > 0 & y == marker_rank[1]) > 0) return(marker_rank[1])
    if(sum(x > 0 & y == marker_rank[2]) > 0) return(marker_rank[2])
    if(sum(x > 0 & y == marker_rank[3]) > 0) return(marker_rank[3])
    if(sum(x > 0 & y == marker_rank[4]) > 0) return(marker_rank[4])
    return(NA)
  }

  win_df <- diff_df %>% group_by(row, m1) %>%
    summarize(within15 = sum(abs(diff) <= 15, na.rm = T),
              within30 = sum(abs(diff) <= 30, na.rm = T),
              withinInf = sum(abs(diff) <= Inf, na.rm = T)) %>%
    group_by(row) %>%
    mutate(win15 = winpick(within15, m1),
           win30 = winpick(within30, m1),
           winInf = winpick(withinInf, m1),
           win = ifelse(!is.na(win15), win15,
                        ifelse(!is.na(win30), win30, winInf))) %>%
    select(row, win) %>% unique()

  df_short <- df[rowSums(!is.na(df[,-5])) == 1, -5]
  if(nrow(df_short) > 0) {
    df_short_t <- matrix(rep(colnames(df_short), each = nrow(df_short)), ncol = 4, byrow = F)
    win_df$win[is.na(win_df$win)]<- df_short_t[!is.na(df_short)]
  }

  win <- win_df$win

  if(s == 'start') {

    win_times <- if_else(win == 'ac', df$ac,
                         if_else(win == 'l', df$l,
                                 if_else(win == 'm', df$m, df$d)))
    pmax2 <- function(x, y) {
      ifelse(is.na(x) & !is.na(y), y,
             ifelse(is.na(y) & !is.na(x), x, pmax(x, y)))
    }
    pmin2 <- function(x, y) {
      ifelse(is.na(x) & !is.na(y), y,
             ifelse(is.na(y) & !is.na(x), x, pmin(x, y)))
    }
    marker_after <- pmin2(difftime(df$m, df$ac, units = 'mins'), difftime(df$m, df$l, units = 'mins'))
    marker_after <- ifelse(is.na(marker_after), 0, as.numeric(marker_after))
    light_after <- as.numeric(difftime(df$l, win_times, units = 'mins'))
    light_after <- ifelse(is.na(light_after), 0, as.numeric(light_after))
    # chng2marker <- marker_after >= 30 & marker_after <= exception_window
    # chng2light <- light_after >= 30 & light_after <= exception_window
    chng2marker <- marker_after > 0 & marker_after <= exception_window & 0 <= marker_after
    chng2light <- light_after > 0 & light_after <= exception_window
    win <- ifelse(chng2light, 'l', win)
    win <- ifelse(chng2marker, 'm', win)
  } else if(end_correction) {
    # acl_tf <- difftime(df$ac, df$l, units = 'mins') < -15
    # acm_tf <- difftime(df$ac, df$m, units = 'mins') < -15
    # acd_tf <- difftime(df$ac, df$d, units = 'mins') < -15
    #
    # tf_mat <- data.frame(acl_tf, acm_tf, acd_tf)
    # tf_mat$ntrue <- rowSums(tf_mat, na.rm=T)
    # tf_mat$non_na <- 3 - rowSums(is.na(tf_mat))
    # to_adj <- which((tf_mat$ntrue == tf_mat$non_na) & (tf_mat$non_na > 0))
    #
    # roll_avg <- zoo::rollmean(epochs_f$Activity, k = 10)
    # inds <- 1:(nrow(epochs_f)-9)
    #
    # epochs_f_temp <- epochs_f; epochs_f_temp$roll_avg <- NA
    # epochs_f_temp$roll_avg[inds] <- roll_avg
    #
    # for(i in seq_along(to_adj)) {
    #
    #   rw_temp <- df[to_adj[i], ]; v <- rw_temp[1,1:4]
    #   v <- as.POSIXct(as.matrix(v), tz = 'UTC')
    #   mintime1 <- min(v, na.rm = T)
    #   maxtime1 <- max(v, na.rm = T) + 90*60
    #
    #   high_act_time <- epochs_f_temp %>%
    #     filter(Epoch.Date.Time.f >= mintime1,
    #            Epoch.Date.Time.f <= maxtime1, roll_avg > 200) %>%
    #     pull(Epoch.Date.Time.f) %>% min()
    #
    #   match1 <- which(abs(difftime(v, high_act_time)) ==
    #                     min(abs(difftime(v, high_act_time)), na.rm = T))
    #
    #   adjusted_marker <- winpick(T, colnames(df)[match1])
    #   win[to_adj[i]] <- adjusted_marker
    # }
  }

  return(win)

}
