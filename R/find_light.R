find_light <- function(
    epochs_f, actigraphy_diary, epoch_length, n = 30, search_window = 60,
    napfilter = T, span1 = 0.1) {

  get_light_under_thres <- function(thres = 1, spike = F) {
    # tf <- epochs_f$White.Light < thres
    # tf <- ifelse(is.na(tf), FALSE, tf)
    max_atleastk_start <- function(x, k=10/epoch_length){
      if(sum(!is.na(x)) >= k){
        if(is.na(x[1])) {
          NA
        } else {
          max(x,na.rm=T)
        }
        # max(x,na.rm=T)
      } else {
        NA
      }
    }
    max_atleastk_stop <- function(x, k=10/epoch_length){
      if(sum(!is.na(x)) >= k){
        if(is.na(x[n/epoch_length])) {
          NA
        } else {
          max(x,na.rm=T)
        }
      } else {
        NA
      }
    }
    min_atleastk_stop <- function(x, k=10/epoch_length){
      x <- ifelse(is.nan(x), NA, x)
      if(sum(!is.na(x)) >= k){
        if(is.na(x[1])) {
          NA
        } else {
          mean(na.omit(x) > thres)
        }
      } else {
        NA
      }
    }
    # under1 <- zoo::rollapply(epochs_f$White.Light, (1/epoch_length)*n, max_atleastk_start, align = 'left') < thres
    nextn <- zoo::rollapply(epochs_f$White.Light, (1/epoch_length)*n, max_atleastk_start, align = 'left', fill=NA) < thres

    # under1_stop <- zoo::rollapply(epochs_f$White.Light, (1/epoch_length)*n, max_atleastk_stop, align = 'right') < thres
    prevn <- zoo::rollapply(epochs_f$White.Light, (1/epoch_length)*n, max_atleastk_stop, align = 'right', fill=NA) < thres
    lights_stay <- zoo::rollapply(epochs_f$White.Light, (1/epoch_length)*n, min_atleastk_stop, align = 'left', fill=NA)

    # nextn <- rep(FALSE, nrow(epochs_f))
    # nextn[1:(length(nextn)-(n/epoch_length)+1)] <- under1 #== 1
    # prevn <- rep(FALSE, nrow(epochs_f))
    # prevn[(n/epoch_length):length(prevn)] <- under1_stop #== 1
    # lights_stay <- c(light_avg_continue, rep(NA, (1/epoch_length)*n - 1))

    nextn_rle <- rle(nextn)
    nextn_begin_rle_index <- which(nextn_rle$values & lag(!is.na(nextn_rle$values))) - 1
    if(length(nextn_begin_rle_index) == 0) {
      nextn_begin_index <- 1 # HUGE HACK!!!
    } else {
      nextn_begin_index <- cumsum(nextn_rle$lengths)[nextn_begin_rle_index] + 1
      if(nextn_begin_rle_index[1] == 0) nextn_begin_index = c(1, nextn_begin_index)
    }
    nextn <- nextn_begin_index

    prevn_rle <- rle(prevn)
    prevn_end_rle_index <- which(prevn_rle$values & lead(!is.na(prevn_rle$values)))
    if(length(prevn_end_rle_index) == 0) {
      prevn <- NA
    } else {
      prevn <- cumsum(prevn_rle$lengths)[prevn_end_rle_index]
    }



    dt <- function(x,y) {
      ret <- as.numeric(difftime(x, y, units = 'mins'))
      return(ret)
    }
    na <- as.POSIXct(NA, tz = 'UTC')
    lights_off <- lights_on <- rep(na, nrow(actigraphy_diary))

    # epochs_f %>% filter(Epoch.Date.Time.f >= ap('2018-02-03 02:52:00'),
    #                     Epoch.Date.Time.f <= ap('2018-02-03 03:22:00')) %>%
    #   select(Epoch.Date.Time.f, White.Light)

    # browser()

    for(rec in 1:nrow(actigraphy_diary)) {
      # currently time closest to activity marker
      lights_off[rec] <- actigraphy_diary[rec,] %>%
        mutate(actigraphy.duration = dt(actigraphy.Stop, actigraphy.Start)) %>%
        select(ends_with('Start'), actigraphy.duration) %>%
        cbind(data.frame(nextn = epochs_f$Epoch.Date.Time.f[nextn])) %>%
        cbind(data.frame(light = epochs_f$White.Light[nextn])) %>%
        mutate(search_window1 = ifelse(
          actigraphy.duration/2 < search_window & napfilter,
          (actigraphy.duration / 2), search_window),
          mint = pmin(actigraphy.Start, diary.Start, marker.Start, na.rm=T) - (search_window1*60),
          maxt = pmax(actigraphy.Start, diary.Start, marker.Start, na.rm=T) + (search_window1*60),
          ad = abs(dt(actigraphy.Start, nextn))) %>%
        filter(nextn <= maxt & mint <= nextn, !is.nan(light)) %>%
        mutate(pick = ifelse(!is.na(ad), ad == min(ad),FALSE)) %>%
        filter(pick) %>% slice(1) %>% pull(nextn)

      lo_temp <- actigraphy_diary[rec,] %>%
        mutate(actigraphy.duration = dt(actigraphy.Stop, actigraphy.Start)) %>%
        select(ends_with('Stop'), actigraphy.duration) %>%
        cbind(data.frame(prevn = epochs_f$Epoch.Date.Time.f[prevn])) %>%
        cbind(data.frame(light = epochs_f$White.Light[prevn])) %>%
        cbind(data.frame(lights_stay = lights_stay[prevn])) %>%
        mutate(search_window1 = ifelse(
          actigraphy.duration/2 < search_window & napfilter,
          (actigraphy.duration / 2), search_window),
          mint = pmin(actigraphy.Stop, diary.Stop, marker.Stop, na.rm=T) - (search_window*60),
          maxt = pmax(actigraphy.Stop, diary.Stop, marker.Stop, na.rm=T) + (search_window*60),
          ad = abs(dt(actigraphy.Stop, prevn))) %>%
        filter(prevn <= maxt & mint <= prevn, !is.nan(light))

      # lights_on[rec] <- lo_temp %>%
      #   filter(lights_stay >= 1/3) %>%
      #   mutate(pick = ifelse(!is.na(ad), ad == min(ad),FALSE)) %>%
      #   filter(pick) %>% slice(1) %>% pull(prevn)
      if(nrow(lo_temp) == 1) {
        lights_on[rec] <- lo_temp %>%
          mutate(pick = ifelse(!is.na(ad), ad == min(ad),FALSE)) %>%
          filter(pick) %>% slice(1) %>% pull(prevn)
      } else {
        lights_on[rec] <- lo_temp %>%
          filter(lights_stay >= 1/3) %>%
          mutate(pick = ifelse(!is.na(ad), ad == min(ad),FALSE)) %>%
          filter(pick) %>% slice(1) %>% pull(prevn)
        if(spike & is.na(lights_on[rec])) {
          lights_on[rec] <- lo_temp %>%
            mutate(pick = ifelse(!is.na(ad), ad == min(ad),FALSE)) %>%
            filter(pick) %>% slice(1) %>% pull(prevn)
        }
      }

    }
    actigraphy_diary_light <- actigraphy_diary
    actigraphy_diary_light$light.Start <- lights_off
    actigraphy_diary_light$light.Stop <- lights_on
    return(actigraphy_diary_light)
  }

  actigraphy_diary_light <- get_light_under_thres(1)
  actigraphy_diary_light %>% select(dayno, light.Start, actigraphy.Start, light.Stop, actigraphy.Stop)

  thres_df <- epochs_f %>% group_by(dayno) %>%
    summarize(pslp = mean(Interval.Status %in% c('REST', 'REST-S'))) %>%
    right_join(epochs_f, by = 'dayno') %>%
    mutate(time_step = cumsum(as.numeric(difftime(lead(Epoch.Date.Time.f),Epoch.Date.Time.f,units='mins'))),
           light_smooth = ifelse(!(is.na(time_step) | is.na(White.Light)),
                                 loess(log2(White.Light + 1) ~ time_step, span = span1)$fitted,
                                 NA)) %>%
    group_by(dayno, pslp) %>%
    reframe(light_thres1 = quantile(log2(White.Light + 1), pslp, na.rm = T),
            light_thres2 = quantile(light_smooth, pslp, na.rm = T)) %>%
    unique() %>%
    mutate(light_thres1 = 2^light_thres1-1,
           light_thres2 = 2^light_thres2-1) %>% ungroup() %>%
    select(dayno, light_thres = light_thres2) %>%
    arrange(dayno) %>% na.omit

  missing_start <- which(is.na(actigraphy_diary_light$light.Start))
  missing_stop <- which(is.na(actigraphy_diary_light$light.Stop))

  missing_light <- unique(c(missing_start, missing_stop))
  for(rec in seq_along(missing_light)) {
    new_thres_i <- thres_df$dayno == actigraphy_diary_light$dayno[missing_light[rec]]
    new_thres <- thres_df$light_thres[new_thres_i]
    if(length(new_thres) > 0 ) {
      if(new_thres <= 1){
        new_thres <- 1
      }
      actigraphy_diary_light_temp <- get_light_under_thres(new_thres, spike = T) %>% slice(missing_light[rec]) %>%
        select(starts_with('light'))
      if(missing_light[rec] %in% missing_start) {
        actigraphy_diary_light$light.Start[missing_light[rec]] <- actigraphy_diary_light_temp$light.Start
      }
      if(missing_light[rec] %in% missing_stop) {
        actigraphy_diary_light$light.Stop[missing_light[rec]] <- actigraphy_diary_light_temp$light.Stop
      }

    }

  }

  return(actigraphy_diary_light)


}
