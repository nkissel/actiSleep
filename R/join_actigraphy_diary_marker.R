#' Joins cleaned actigraphy, sleep diary, and marker rest intervals
#'
#' This function takes the cleaned actigraphy rest dataset returned from
#' `update_sleep()`, cleaned sleep diary data from `sleep_diary_formatting`,
#' cleaned marker data from `find_markers` and joins the datasets together
#'
#' @param updated_sleep_df dataframe that is outputted from `update_sleep()`--
#' this dataset should have already had engines/cabooses taken care of
#' @param cleaned_marker_df dataframe that is outputted from from `find_markers`
#' -- this dataset should already have been run through `add_dayno`
#' @return a dataframe with columns: ID, dayno, Type (Main vs. Nap),
#' actigraphy.Start, diary.Start, marker.Start, actigraphy.Stop, diary.Stop,
#' marker.Stop
#'
join_actigraphy_diary_marker <- function(updated_sleep_df, cleaned_marker_df){

  res <- updated_sleep_df %>%
    # mutate(CDATE_date = if_else(is.na(CDATE_date), as.Date(diary.Start), CDATE_date)) %>%
    select(ID, dayno, everything()) %>%
    arrange(ID, dayno) %>%
    rename(#Actigraphy.Interval = Interval.f,
      actigraphy.Start = updated.Start,
      actigraphy.Stop = updated.End) %>%
    full_join(., cleaned_marker_df) %>%
    select(ID, dayno, contains("Start"), contains("Stop"), invalid_flag)

  res <- res %>%
    mutate(diary_overlap = (diary.Start <= actigraphy.Stop & diary.Start >= actigraphy.Start) |
             (diary.Stop <= actigraphy.Stop & diary.Stop >= actigraphy.Start) |
             (diary.Start <= actigraphy.Start & diary.Stop >= actigraphy.Start),
           diary.Start = if_else(!diary_overlap, as.POSIXct(NA, tz = 'UTC'), diary.Start),
           diary.Stop = if_else(!diary_overlap, as.POSIXct(NA, tz = 'UTC'), diary.Stop)) %>%
    select(ID, dayno, contains("Start"), contains("Stop"), invalid_flag)

  tokeepres <- res %>% select(ends_with('.Start'), ends_with('.Stop')) %>% is.na() %>% rowSums() != 6
  res <- res[tokeepres,] %>% unique()
  return(res)
}
