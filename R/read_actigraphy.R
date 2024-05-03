
#' Read actigraphy data
#'
#' This function takes in an actigraphy file and returns a list of data.frames
#' that include the properties, summary statistics, button presses (marker),
#' and epoch-level data.
#'
#' @param file_name character string of the file name. It should be a .csv file.
#' @return a list of data.frames corresponding to properties, statistics, markers,
#' epochs-level.
#'
read_actigraphy <- function(file_name) {
  if(missing(file_name)) stop('Actigraphy file missing')
  if(!file.exists(file_name)) stop('Actigraphy file not found')
  d <- readLines(file_name, skipNul = T) # read in messy file
  sm <- function(x) suppressWarnings(suppressMessages(x))

  # GET PROPERTIES
  properties <- paste("Actiwatch Data Properties",  "Analysis Inputs", sep="|")
  indeces <- grep(properties, d)
  a_props <- d[c((indeces[[1]]+2): (indeces[[2]]-3))] # removing white spaces
  name_split <- unlist(str_split(file_name, "/"))
  name_split <- name_split[length(name_split)]
  id <- unlist(str_split(name_split, "_"))[1]
  props <- read_csv(I(a_props), col_names = F) %>% sm()
  props$ID <- id

  # GET EPOCHS
  epoch <- paste("Epoch-by-Epoch Data")

  epoch_indeces <- grep(epoch, d)[[1]]
  empty_indeces <- which(d == '')
  empty_indeces <- empty_indeces[empty_indeces > epoch_indeces]

  epochs <- d[c((empty_indeces[2] + 1),
                (empty_indeces[2] + 3):length(d))] # removing white spaces

  # author note: the code for removing white spaces requires me to use these
  # `empty_indeces` vectors because there's an inconsistent number of rows between
  # the header and the data, but there's a consistent number white spaces. I
  # think the inconsistency is due to export settings, but I'm not sure.

  epochs <- read_csv(I(epochs), col_names = T) %>% sm()
  # epochs[,13] <- NULL
  colnames(epochs) <- gsub(' |-|/|#', '.', colnames(epochs))
  epochs <- epochs %>%
    mutate(ID = id, Date.f = mdy(Date),
           Epoch.Date.Time.f = mdy_hms(paste0(Date, " ", Time)))

  # GET STATISTICS
  summary_stats <- paste("Statistics",  "Marker/Score List", sep="|")
  grep(summary_stats, d) -> stats_indeces
  stats <- d[c((stats_indeces[[1]]+2),
               (stats_indeces[[1]]+5):(stats_indeces[[2]]-3))] # removing white spaces
  stats_df <- read_csv(I(stats)) %>% sm()
  stats_df[,ncol(stats_df)] <- NULL
  colnames(stats_df) <- gsub(' |-|/', '.', colnames(stats_df))
  colnames(stats_df) <- gsub('%|#', 'X.', colnames(stats_df))
  stats_df <- stats_df[,!duplicated(colnames(stats_df))]
  stats_df <- stats_df %>% rename(Interval. = IntervalX.)
  stats_df <- stats_df %>% mutate(
    ID = id, Start.Date.f = mdy(Start.Date),
    Start.Date.Time.f = mdy_hms(paste0(Start.Date, " ", Start.Time)),
    End.Date.f = mdy(End.Date),
    End.Date.Time.f = mdy_hms(paste0(End.Date, " ", End.Time))) %>% sm()
    # mutate(across(c(9:36), as.numeric))

  # Get marker pushes
  markers <- paste("Epoch-by-Epoch Data",  "Marker/Score List", sep="|")

  marker_indeces <- grep(markers, d)
  empty_indeces <- which(d == '')
  empty_indeces <- empty_indeces[empty_indeces > marker_indeces[1]]

  marks <- d[c((empty_indeces[2] + 1),
               (empty_indeces[2] + 3):(marker_indeces[2]-3))] # removing white spaces

  markers_df <- read_csv(I(marks), col_types = cols(.default = col_character())) %>% sm()
  markers_df[,6] <- NULL
  markers_df <- markers_df %>% mutate(
    ID = id, Date.f = hms(Date),
    Marker.Date.Time.f = mdy_hms(paste0(Date.f, " ", Time)))

  objects <- list("properties" = props, "statistics" = stats_df,
                  "markers" = markers_df,  "epochs" = epochs)

  return(objects)
}
