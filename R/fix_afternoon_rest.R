
#' Fix afternoon rest
#'
#' This function adjusts the day number of rest data that passes noon.
#'
#' @param df data.frame with epoch-level data
#' @param updated_rest data.frame with start and end times of rest intervals.
#' If some of these pass noon, the day number will be adjusted.
#' @param end_nm character string with the name of the end time column in
#' `updated_rest`
#' @return a data.frame of epoch-level data with updated tracking day numbers.
#' If rest passes noon, then the day number are adjusted to the day in which the
#' rest began.
#'
fix_afternoon_rest <- function(df, updated_rest, end_nm) {
  dayno_limits <- df %>% group_by(dayno) %>% summarize(min_time = min(Epoch.Date.Time.f)) %>% na.omit
  # dayno_limits$min_time <- as.POSIXct(paste0(as.Date(dayno_limits$min_time), ' 12:01:00'), tz = 'UTC')
  dayno_limits$max_time <- as.POSIXct(paste0(as.Date(dayno_limits$min_time) + 1, ' 12:00:00'), tz = 'UTC')

  dayno_updated_ends <- updated_rest %>% unique() %>%
    # filter(Type == 'MAIN') %>%
    select(dayno, !!sym(end_nm)) %>%
    group_by(dayno) %>% summarize(updated.End = max(!!sym(end_nm))) %>% ungroup() %>%
    mutate(day_end_limit = updated.End + 1*60*60,
           updated.End = NULL)
  dayno_limits <- dayno_limits %>% left_join(dayno_updated_ends, by = 'dayno')
  dayno_limits$day_end_limit <- if_else(is.na(dayno_limits$day_end_limit),
                                        dayno_limits$max_time, dayno_limits$day_end_limit)

  adj_begin <- which(dayno_limits$day_end_limit > dayno_limits$max_time)
  adj_begin <- adj_begin[adj_begin < nrow(dayno_limits)]
  dayno_limits$day_begin_limit <- dayno_limits$min_time
  dayno_limits$day_begin_limit[adj_begin + 1] <- dayno_limits$day_end_limit[adj_begin] + 60
  dayno_limits$day_end_limit <- pmax(dayno_limits$max_time, dayno_limits$day_end_limit)

  dayno_limits <- dayno_limits %>% select(dayno, day_begin_limit, day_end_limit) %>% unique()
  df <- df %>% left_join(dayno_limits, by = 'dayno')

  df$dayno <- if_else(df$Epoch.Date.Time.f < df$day_begin_limit, df$dayno - 1, df$dayno)
  df$dayno <- if_else(df$Epoch.Date.Time.f > df$day_end_limit, df$dayno + 1, df$dayno)
  return(df)
}
