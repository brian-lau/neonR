# neonR

## Installation
Install using devtools
```
devtools::install_github("brian-lau/neonR")
```

## Usage
To read a data directory containing exported raw data from Pupil Cloud,
```
library(neonR)

obj <- read_exported_dir("~/Downloads/raw-data-export/2023-11-27_16-26-05-e99365cf")
```

This will produce a named list, one element that is a tibble containing data from the various .csv files that get exported (e.g., gaze.csv, imu.csv, etc). You can access this by name
```
obj$data

# or

obj[["data"]]
```

Each separate file lives inside a column, which is a another nested tibble. This is a list, so you can access gaze data, for example, as follows
```
obj$data$gaze[[1]]
```

For trial-based experiments where events have been recorded indicating the beginning and end of each trial, it is convenient to split trials out into separate rows,
```
obj <- chop_trials(obj)
```
where we can now access data for each trial,
```
# For example, the third trial
obj$data[3,]$gaze[[1]]
```

If you have [`ffmpeg`](https://ffmpeg.org/) here is also a helper that splits the corresponding world (scene) video into trials,
```
chop_world_video_trials(obj, videofile = "6992c8a2_0.0-443.191.mp4")
```

We can quickly visualize data from individual trials,
```
plot_list <- obj$data %>% 
  group_by(trial) %>%
  group_map(
    ~ ggplot(.x$imu[[1]], aes(timestamp, acceleration_z)) + 
    geom_line()
  )
patchwork::wrap_plots(plot_list)

plot_list = obj$data %>% 
  group_by(trial) %>%
  group_map(
    ~ ggplot(.x$gaze[[1]], aes(timestamp, elevation)) + 
    geom_line()
  )
patchwork::wrap_plots(plot_list)

plot_list = df %>% group_by(trial) %>%
  group_map(~ ggplot(.x$eye_states[[1]], aes(timestamp, pupil_diameter)) + geom_line())
patchwork::wrap_plots(plot_list)
```
