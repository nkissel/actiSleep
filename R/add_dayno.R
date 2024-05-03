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

add_dayno <- function(mydata, date_time_column, known_start) {
  if(!(date_time_column %in% colnames(mydata))) {
    stop("date_time_column not found in the supplied data.frame")
  }

  mydata$oldid <- mydata$ID
  mydata$ID <- paste0(mydata$ID)
  mydata$ID <- as.numeric(as.factor(mydata$ID))

  # mydata <- mydata %>% arrange(!!sym(date_time_column))
  mydata <- mydata %>% arrange(pick(matches(c(date_time_column))))

  # ID changepoints (probably aren't any)
  changepoints <- c(1, which(diff(mydata$ID) != 0) + 1)
  ids <- mydata$ID[changepoints]

  # setting reference/ dates
  mindate <- known_start

  # minimum date starting at noon
  ref_date1 <- date(lubridate::ymd_hms(mindate)) + hours(12)

  # is minimum start date after the start of the reference point?
  which_timeb4 <- as.numeric(difftime(mindate, ref_date1) < 0)
  ref_date1[which_timeb4] <- ref_date1[which_timeb4] - days(1) # if yes, set it back 24 hours to get the start of that same day (noon to noon)

  ref_date2 <- as.POSIXct(character(0), tz = "UTC")
  for(i in seq_along(ids)) {
    ref_date2 <- c(ref_date2, rep(ref_date1[i], length(which(mydata$ID == ids[i]))))
  }

  # assigning daynumbers
  dayno <- c()
  for(i in seq_along(ids)) {
    inx <- which(mydata$ID == ids[i]) # grab rows corresponding to each ID
    dayno <- c(dayno, ceiling(difftime(mydata[inx, date_time_column],
                                       ref_date2[inx], units = "days")))
  }
  mydata$dayno <- dayno
  mydata$ID  <- mydata$oldid
  mydata$oldid <- NULL
  return(mydata)
}
