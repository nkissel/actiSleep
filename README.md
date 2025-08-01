# actiSleep

An actigraphy analysis pipeline. This package is intended to process epoch-level actigraphy data by editing rest start and end time, as well as flagging uncertain or aberrant records. 

## actiSleep overview

The core `actiSleep()` function has the following arguments.

- `epoch_df`: a `data.frame` of epoch level data read from raw actigraphy files
- `diary_df`: a `data.frame` of diary data
- `stats_df`: a `data.frame` of statistics data read from raw actigraphy files
- `marker_df`: a `data.frame` of marker data read from raw actigraphy files
- `time_start`: analysis start time as a `POSIXct` object
- `time_stop`: analysis end time as a `POSIXct` object
- `max_invalid_time`: the maximum number of invalid hours allowed per day

> [!NOTE]
> For all `POSIXct` object, it is recommended that they are all converted to either standard or daylight time so to ensure all times are aligned. Furthermore, they should be forced into Coordinated Universal Time (`UTC`). 

### Epoch level data

This package was originally designed for exported actiware data. Therefore, many of the columns follow actiware's naming convention. 

`epoch_df` should have the following columns.

|    Column Name      |  Description                                  |  Data type  |
|---------------------|-----------------------------------------------|-------------|
| `ID`                | Participant identifier                        | `numeric`   |
| `Epoch.Date.Time.f` | Timestamp                                     | `POSIXct`   |
| `Off.Wrist.Status`  | Dummy variable for off wrist (1 is off wrist) | `numeric`   |
| `Activity`          | Activity level                                | `numeric`   |
| `White.Light`       | Light measured in Lux                         | `numeric`   |
| `Sleep.Wake`        | Dummy variable for awake/sleep (1 is wake)    | `numeric`   |
| `Interval.Status`   | Type of epoch (REST/REST-S/ACTIVE/EXCLUDED    | `character` |

### Diary data

The diary contains self-reported rest start and end times. It is recommended that the user adjusts diary data so to stay in either standard or daylight time. Often sleep diaries follow clock time, and are therefore sometimes not aligned with the epoch-level data around daylight savings changes. Further, it is recommended that all times are put into the `UTC` timezone. Below are the necessary `diary_df` columns. 

|    Column Name    |  Description                                  |  Data type  |
|-------------------|-----------------------------------------------|-------------|
| `ID`              | Participant identifier                        | `numeric`   |
| `diary_start`     | Rest start time                               | `POSIXct`   |
| `diary_end`       | Rest end time                                 | `POSIXct`   |

### Marker push

The marker `data.frame` contains the times of all button presses. Data collection devices may have a button that the user presses to designate rest onset and offset times. Below are the necessary `marker_df` columns. 

|    Column Name        |  Description                                  |  Data type  |
|-----------------------|-----------------------------------------------|-------------|
| `ID`                  | Participant identifier                        | `numeric`   |
| `Marker.Date.Time.f ` | Rest start time                               | `POSIXct`   |
