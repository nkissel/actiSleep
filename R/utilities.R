
#' @export
to_dec <- function(x, add = T) {
  w <- which(!is.na(x))
  h <- m <- rep(-1, length(x))
  h[w] <- substr(x[w], 12, 13)
  m[w] <- substr(x[w], 15, 16)
  h <- as.numeric(h); m <- as.numeric(m)
  if(add) {
    h[w] <- ifelse(h[w] < 12, h[w] + 24, h[w])
  }
  rt <- h + m/60
  ifelse(nchar(as.character(x)) == 10, 24, rt)

}

#' @export
to_dec2 <- function(x, tz = 'UTC', grid_length = 100, ...) {
  if(grid_length < 3) {
    grid_length <- 3
  }
  if('character' %in% class(x)) {
    x <- if_else(nchar(x) == 10, as.POSIXct(x, '%Y-%m-%d', tz = tz),
                 as.POSIXct(x, '%Y-%m-%d %H:%M:%S', tz = tz))
  }
  dt <- function(x, y) difftime(x, y, unit = 'mins')

  time_anchored_on_center <- function(center_time, x, y, z) {
    to_add <- abs(dt(center_time, x)) > abs(dt(center_time, y))
    to_add <- ifelse(is.na(to_add), F, to_add)
    to_sub <- abs(dt(center_time, x)) > abs(dt(center_time, z))
    to_sub <- ifelse(is.na(to_sub), F, to_sub)

    h <- ms <- rep(NA, length(x))
    h <- hour(x)
    ms <- minute(x) + second(x) / 60
    h <- ifelse(to_add, h + 24,
                ifelse(to_sub, h - 24, h))

    rt <- h + ms/60
    return(rt)
  }

  x <- as.POSIXct(paste(Sys.Date(), format(x, '%H:%M:%S')), format = '%Y-%m-%d %H:%M:%S', tz = tz)
  y <- x + hours(24)
  z <- x - hours(24)
  center_opts <- seq(min(x, na.rm = T), max(x, na.rm = T), len = grid_length)
  candidate_list <- lapply(center_opts, time_anchored_on_center, x, y, z)
  fin <- candidate_list[[which.min(lapply(candidate_list, sd))]]
  if(max(fin) >= 48) {
    fin <- fin - 24
  }
  if(min(fin) <= -24) {
    fin <- fin + 24
  }
  return(fin)
}

#' @export
to_time <- function(x) {
  midnight <- as.POSIXct('00:00:00', format = '%H:%M:%S', tz='UTC')

  dec_to_midn <- x - 24 # or could subtract 0, i don't think this matters
  h <- floor(dec_to_midn)
  minsec <- (dec_to_midn - h) * 60
  min <- floor(minsec)
  sec <- round((minsec - min) * 60)

  midnight + hours(h) + minutes(min) + seconds(sec)
}

#' @export
date2noon <- function(x, format, tz = 'UTC') {
  if_else(is.na(x), NA,
         as.POSIXct(paste0(x, ' 12:00:00'), format = format, tz = tz))
}

# Marking invalid epochs
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

custom_bind_row1 <- function(x, y, s = F) {
  # print(nrow(x))
  # print(nrow(y))
  if(nrow(x) == 0 & nrow(y) != 0) {
    return(y)
  } else if(nrow(y) == 0 & nrow(x) != 0) {
    return(x)
  } else if(nrow(x) != 0 & nrow(y) != 0) {
    if(s) {
      minx <- min(x$Start.Date.Time.f, na.rm = T)
      miny <- min(y$Start.Date.Time.f, na.rm = T)
      if(miny < minx) {
        z <- x; x <- y; y <- z; rm(z)
      }

      x_noint <- x %>% select(-Interval.)
      y_noint <- y %>% select(-Interval.)
      dv <- dplyr::bind_rows(x_noint, y_noint) %>% mutate(d = duplicated(.)) %>% pull(d)
      x_new <- x[!dv[seq_len(nrow(x))],]
      y_new <- y[!dv[(nrow(x) + 1):length(dv)],]
      maxint <- suppressWarnings(max(as.numeric(x_new$Interval.), na.rm = T))
      y_new$Interval. <- ifelse(!is.na(as.numeric(y_new$Interval.)),
                                as.character(as.numeric(y_new$Interval.) + maxint), y_new$Interval.)
      return(dplyr::bind_rows(x_new, y_new))
    } else {
      return(dplyr::bind_rows(x, y))
    }
  }
}

custom_bind_row <- function(x, y, s = F) {
  # print(nrow(x))
  # print(nrow(y))
  if(!is.null(y) & is.null(x)) {
    return(y)
  } else if(!is.null(x) & is.null(y)) {
    return(x)
  } else if(!is.null(x) & !is.null(y)) {
    return(custom_bind_row1(x,y,s))
  } else {
    return(NULL)
  }
}


algo_column <- function(all_markers) {
  # browser()
  all_markers %>% filter(is.na(actigraphy.Start),
                         is.na(actigraphy.Stop)) -> nap2remove
  all_markers <- nap2remove %>% rbind(all_markers)

  rw1 <- all_markers %>% duplicated(fromLast=T) %>% which()
  rw2 <- all_markers %>% duplicated(fromLast=F) %>% which()
  if(length(c(rw1, rw2)) > 0) {
    all_markers <- all_markers[-c(rw1, rw2),]
  }
  find_algo_times(ac = all_markers$actigraphy.Start,
             m = all_markers$marker.Start,
             l = all_markers$light.Start,
             d = all_markers$diary.Start, s = 'start') -> start_algo_win
  find_algo_times(ac = all_markers$actigraphy.Stop,
             m = all_markers$marker.Stop,
             l = all_markers$light.Stop,
             d = all_markers$diary.Stop, s = 'stop') -> stop_algo_win
  all_markers <- all_markers %>%
    mutate(algo.Start = if_else(start_algo_win == 'ac', actigraphy.Start,
                                if_else(start_algo_win == 'm', marker.Start,
                                        if_else(start_algo_win == 'd', diary.Start, light.Start)))) %>%
    mutate(algo.Stop = if_else(stop_algo_win == 'ac', actigraphy.Stop,
                               if_else(stop_algo_win == 'm', marker.Stop,
                                       if_else(stop_algo_win == 'd', diary.Stop, light.Stop))))
  return(all_markers)
}

remove_if_EXCLUDED <- function(x, epochs_f) {
  # browser()
  remdf <- epochs_f %>% select(Epoch.Date.Time.f, Interval.Status) %>%
    arrange(Epoch.Date.Time.f) %>%
    filter(Epoch.Date.Time.f %in% x) %>%
    mutate(rem = ifelse(Interval.Status == 'EXCLUDED', 1, 0))
  xdf <- data.frame(Epoch.Date.Time.f = x)
  xdf %>% left_join(remdf, by = 'Epoch.Date.Time.f') %>%
    mutate(
      rem = ifelse(is.na(rem), 0, rem),
      Epoch.Date.Time.f = if_else(rem == 1, NA, Epoch.Date.Time.f)) %>%
    pull(Epoch.Date.Time.f)
}

#' @export
tochr <- function(x) {
  x = as.character(x)
  x = ifelse(nchar(x) == 10, paste0(x, ' 00:00:00'), x)
  x
}

#' @export
ap <- function(z, tz = 'UTC') {
  if(class(z)[1] == 'character') {
    z <- tochr(z)
    as.POSIXct(z, tz = tz)
  } else if(class(z)[1] %in% c("POSIXct", "POSIXt")) {
    z
  }
}
