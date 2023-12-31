#' csv_col_defs: Column definitions for Neon exported csv files
#'
#' @param name One of "gaze", "3d_eye_states", "blinks", "events", "fixations",
#' "imu", "world_timestamps"
#'
#' @return readr-compatible cols() definition
#' @importFrom readr cols
#' @importFrom readr col_character
#' @importFrom readr col_double
#' @export
#'
#' @examples
csv_col_defs <- function(name) {
  switch(name,
         gaze = {
           cols(
             `section id` = col_character(),
             `recording id` = col_character(),
             `timestamp [ns]` = col_double(),
             `gaze x [px]` = col_double(),
             `gaze y [px]` = col_double(),
             worn = col_double(),
             `fixation id` = col_double(),
             `blink id` = col_double(),
             `azimuth [deg]` = col_double(),
             `elevation [deg]` = col_double()
           )
         },
         "3d_eye_states" = {
           cols(
             `section id` = col_character(),
             `recording id` = col_character(),
             `timestamp [ns]` = col_double(),
             `pupil diameter [mm]` = col_double(),
             `eyeball center left x [mm]` = col_double(),
             `eyeball center left y [mm]` = col_double(),
             `eyeball center left z [mm]` = col_double(),
             `eyeball center right x [mm]` = col_double(),
             `eyeball center right y [mm]` = col_double(),
             `eyeball center right z [mm]` = col_double(),
             `optical axis left x` = col_double(),
             `optical axis left y` = col_double(),
             `optical axis left z` = col_double(),
             `optical axis right x` = col_double(),
             `optical axis right y` = col_double(),
             `optical axis right z` = col_double()
           )
         },
         "blinks" = {
           cols(
             `section id` = col_character(),
             `recording id` = col_character(),
             `blink id` = col_double(),
             `start timestamp [ns]` = col_double(),
             `end timestamp [ns]` = col_double(),
             `duration [ms]` = col_double()
           )
         },
         "events" = {
           cols(
             `recording id` = col_character(),
             `timestamp [ns]` = col_double(),
             name = col_character(),
             type = col_character()
           )
         },
         "fixations" = {
           cols(
             `section id` = col_character(),
             `recording id` = col_character(),
             `fixation id` = col_double(),
             `start timestamp [ns]` = col_double(),
             `end timestamp [ns]` = col_double(),
             `duration [ms]` = col_double(),
             `fixation x [px]` = col_double(),
             `fixation y [px]` = col_double(),
             `azimuth [deg]` = col_double(),
             `elevation [deg]` = col_double()
           )
         },
         "imu" = {
           cols(
             `section id` = col_character(),
             `recording id` = col_character(),
             `timestamp [ns]` = col_double(),
             `gyro x [deg/s]` = col_double(),
             `gyro y [deg/s]` = col_double(),
             `gyro z [deg/s]` = col_double(),
             `acceleration x [G]` = col_double(),
             `acceleration y [G]` = col_double(),
             `acceleration z [G]` = col_double(),
             `roll [deg]` = col_double(),
             `pitch [deg]` = col_double(),
             `yaw [deg]` = col_double(),
             `quaternion w` = col_double(),
             `quaternion x` = col_double(),
             `quaternion y` = col_double(),
             `quaternion z` = col_double()
           )
         },
         "world_timestamps" = {
           cols(
             `section id` = col_character(),
             `recording id` = col_character(),
             `timestamp [ns]` = col_double()
           )
         }
  )
}

#' clean_csv_names: Clean up column names
#'
#' @param names Column names to clean
#'
#' @return
#' @export
#'
#' @examples
clean_csv_names <- function(names) {
  names <- gsub("\\.", "_", make.names(names))
  stringr::str_replace_all(
    names,
    c("__ns_" = "", "__ms_" = "", "__G_" = "",
      "__deg_s_" = "", "__px_" = "", "__mm_" = "", "__deg_" = "")
  )
}

#' read_csv_file: Read a single Neon exported csv file
#'
#' @param filename
#' @param convert_time_cols Convert columns with timestamps to POSIXct (default TRUE)
#' @param clean_names Clean column names (default TRUE)
#'
#' @return
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
read_csv_file <- function(filename,
                          convert_time_cols = T,
                          clean_names = T
) {
  name <- gsub(".csv", "", basename(filename))
  df <- readr::read_csv(filename, col_types = csv_col_defs(name))

  varnames <- names(df)

  if (convert_time_cols) {
    df %<>% mutate(across(ends_with("timestamp [ns]"),
                          ~ anytime::utctime(.x / 1e9)))

    # Duration is a rounded (to msec) variable, replace with actual difference?
    if ("duration [ms]" %in% varnames)
      df %<>% mutate(`duration [ms]` = lubridate::dmilliseconds(`duration [ms]`))
  }

  if (clean_names) names(df) <- clean_csv_names(varnames)

  return(df)
}

#' read_exported_dir: Read all Neon exported csv files in directory
#'
#' @param path
#' @param ...
#'
#' @return
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @export
#'
#' @examples
read_exported_dir <- function(path = ".",
                              ...
) {
  path <- normalizePath(path)
  files <- list.files(path = path, pattern = "\\.csv$", full.names = F)

  DAT <- list()
  for (f in files) {
    df <- read_csv_file(file.path(path, f, ...))
    df %<>% tidyr::nest(data = !any_of(c("recording_id")))

    name <- gsub(".csv", "", f)
    DAT[[name]] <- df
  }

  df <- bind_rows(DAT, .id = "var") %>%
    pivot_wider(names_from = var, values_from = data)

  if ("3d_eye_states" %in% names(df))
    df %<>% rename(eye_states = `3d_eye_states`)

  if ("world_timestamps" %in% names(df)) {
    df$world_timestamps[[1]] %<>% mutate(
      time_from_frame1 = difftime(timestamp, timestamp[1], units = "sec")
    )
  }

  # Setup output
  obj <- NeonR()

  obj$data <- df

  f <- list.files(path = path, pattern = "info.json")
  if (f == "info.json") {
    obj$info <- tidyjson::read_json(file.path(path, "info.json")) %>%
      tidyjson::spread_all() %>%
      tibble::as_tibble()
  }

  f <- list.files(path = path, pattern = "scene_camera.json")
  if (f == "scene_camera.json") {
    obj$scene_camera <- tidyjson::read_json(file.path(path, "scene_camera.json")) %>%
      tidyjson::spread_all() %>%
      tibble::as_tibble()
  }

  # TODO check if only video, unclear if always single video exported
  f <- list.files(path = path, pattern = "\\.mp4$", full.names = F)
  obj$world_videofile <- file.path(path, f)
  # "ffprobe -i <file> -show_entries format=duration -v quiet -of csv="p=0""
  # call <- paste0("ffprobe -v error -select_streams v:0 -count_packets ",
  #               "-show_entries stream=nb_read_packets -of csv=p=0 ", videofile)
  # n_total_frames <- system(call, intern = TRUE) %>% as.numeric()

  return(obj)
}
