#' Find main/nap
#'
#' find main/nap
#'
#' @param mydata epoch level data.frame
#' @param col1 start column name string
#' @param col2 stop column name string
#' @import dplyr
#' @export
find_main <- function(mydata, col1, col2){

  # wind_thres <- 4
  mdtemp <- mydata %>%
    mutate(startc = to_dec(!!sym(col1)),
           endc = to_dec(!!sym(col2), add=F),
           endc = ifelse(endc < startc, endc + 24, endc),
           mid = (startc + endc) / 2,
           duration = abs(difftime(!!sym(col1), !!sym(col2), units = 'mins'))) %>%
    group_by(dayno) %>%
    mutate(rest_obs = max(row_number()),
           ind = row_number(),
           main_or_nap = ifelse(duration == max(duration,na.rm=T), "MAIN", "NAP"))

  avg <- mdtemp %>%
    filter(main_or_nap == 'MAIN') %>% ungroup() %>%
    summarize(avg_startc = median(startc,na.rm=T),
              avg_endc = median(endc,na.rm=T),
              avg_mid = median(mid,na.rm=T))

  mdtemp$avg_mid <- avg$avg_mid

  # closest
  mdtemp2 <- mdtemp %>%
    mutate(td = abs(mid - avg_mid)) %>%
    group_by(dayno) %>%
    mutate(main_or_nap = ifelse( td == min(td,na.rm=T), 'MAIN', 'NAP')) %>%
    select(c(colnames(mydata), 'main_or_nap')) %>%
    mutate(Type = main_or_nap) %>% select(-main_or_nap)

  return(mdtemp2)
}
