#' Create tracking day number
#'
#' This function creates a new column `dayno` that represents a day index
#' increment from noon-to-noon. A known start time must be supplied.
#'
#' @param mydata a dataframe with a `POSIXct` column and an `ID` column
#' @param date_time_column character string corresponding to the datetime
#' column of `mydata`. Must be class `POSIXct`.
#' @param known_start start datetime of the observation period for which day
#' number is anchored. Must be class `POSIXct`.
#' @return a data.frame with additional column `dayno`
#' @import dplyr
#' @import lubridate
#' @export
add_dayno <- function(mydata, date_time_column, known_start) {
  if(!(date_time_column %in% colnames(mydata))) {
    stop("date_time_column not found in the supplied data.frame")
  }
  mydata <- as.data.frame(mydata)

  if('ID' %in% colnames(mydata)) {
    remove_ID <- F
  } else {
    mydata$ID <- 0
    remove_ID <- T
  }
  # mydata <- mydata %>% arrange(!!sym(date_time_column))
  ids <- unique(mydata$ID)

  mydata <- mydata %>% mutate(ID = factor(ID, levels = ids)) %>%
    arrange(!!sym('ID'), !!sym(date_time_column))

  # minimum date starting at noon
  if(nchar(as.character(known_start)) == 10) {
    ref_date1 <- as.Date(ymd(known_start)) + hours(12)
  } else {
    ref_date1 <- as.Date(ymd_hms(known_start)) + hours(12)
  }

  # # is minimum start date after the start of the reference point?
  # which_timeb4 <- difftime(known_start, ref_date1) < 0
  # # if yes, set it back 24 hours to get the start of that same day (noon to noon)
  # ref_date1[which_timeb4] <- ref_date1[which_timeb4] - days(1)

  ref_date2 <- as.POSIXct(character(0), tz = "UTC")
  for(i in seq_along(ids)) {
    ref_date2 <- c(ref_date2, rep(ref_date1[i], length(which(mydata$ID == ids[i]))))
  }

  # assigning day numbers
  dayno <- NULL
  for(i in seq_along(ids)) {
    inx <- which(mydata$ID == ids[i]) # grab rows corresponding to each ID
    dayno <- c(dayno, ceiling(difftime(
      mydata[inx, date_time_column], ref_date2[inx], units = "days")))
  }

  if(length(ids) == 0) {
    mydata$dayno <- mydata$ID
  } else {
    mydata$dayno <- dayno
  }

  if(remove_ID) {
    mydata$ID <- NULL
  } else {
    mydata <- mydata %>% mutate(ID = as.character(ID))
  }
  return(mydata)
}
