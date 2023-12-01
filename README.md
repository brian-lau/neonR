# neonR

## Installation
Install using devtools
```
devtools::install_github("brian-lau/neonR")
```

## Usage
If you have a data directory containing exported raw data from Pupil Cloud,
```
df = read_exported_dir("~/Downloads/raw-data-export/2023-11-27_16-26-05-e99365cf")
```
This will produce a tibble containing data from the various .csv files that get exported (e.g., gaze.csv, imu.csv, etc). Each separate file lives inside a column, which is a another nested tibble. This is a list, so you can access gaze data as follows
```
df$gaze[[1]]
```

For trial-based experiments, it is convenient to split trials out into separate rows,
```
df = chop_trials(df)
```
where we can now access data for each trial,
```
df[1,]$gaze[[1]]
```

There is also a helper that splits the corresponding world (scene) video into trials,
```
chop_world_video_trials(df, videofile = "6992c8a2_0.0-443.191.mp4")
```

We can quickly visualize data from individual trials,
```
plot_list <- df %>% group_by(trial) %>%
  group_map(~ ggplot(.x$imu[[1]], aes(timestamp, acceleration_z)) + geom_line())
patchwork::wrap_plots(plot_list)

plot_list = df %>% group_by(trial) %>%
  group_map(~ ggplot(.x$gaze[[1]], aes(timestamp, elevation)) + geom_line())
patchwork::wrap_plots(plot_list)

plot_list = df %>% group_by(trial) %>%
  group_map(~ ggplot(.x$eye_states[[1]], aes(timestamp, pupil_diameter)) + geom_line())
patchwork::wrap_plots(plot_list)
```
