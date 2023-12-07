#' chop_trials: Chop continuous recording into trials
#'
#' @param obj
#' @param start_postfix String defining trial start event
#' @param end_postfix String defining trial end event
#' @param pre_time Time to pad start of trial
#' @param post_time Time to pad end of trial
#'
#' @return
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @importFrom dplyr relocate select mutate filter across one_of
#' @importFrom dplyr group_by group_modify ungroup
#' @export
#'
#' @examples
chop_trials <- function(obj,
                        start_postfix = ".begin",
                        end_postfix = ".end",
                        pre_time = lubridate::seconds(0),
                        post_time = lubridate::seconds(0)
) {
  # TODO check class, if trials already chopped


  # Trial tags start with number
  trial_events <- obj$data$events[[1]] %>%
    dplyr::select(-type) %>%
    dplyr::filter(grepl("^[[:digit:]]+", name))

  # TODO check postfixes and matching begin/end

  trial_events %<>% tidyr::separate(name, c("trial", "event"))
  trial_events %<>% pivot_wider(names_from = event, values_from = timestamp)
  trial_events %<>% mutate(begin = begin + pre_time,
                           end = end + post_time)

  # TODO verify start and end for each trial
  has_trials <- TRUE

  if (has_trials) {
    # Format data with duration format (start_timestamp/end_timestamp)
    if ("blinks" %in% names(obj$data)) {
      obj$data$blinks[[1]] %<>% pivot_longer(cols = ends_with("timestamp"),
                                             names_to = "event",
                                             values_to = "timestamp")
    }

    if ("fixations" %in% names(obj$data)) {
      obj$data$fixations[[1]] %<>% pivot_longer(cols = ends_with("timestamp"),
                                                names_to = "event",
                                                values_to = "timestamp")
    }

    chopper <- function(x, y) {
      excluded_vars <- c("recording_id")

      # Extract rows of y falling in trial window
      y %<>% mutate(
        across(
          -one_of(excluded_vars),
          ~ list(.x[[1]] %>% filter(timestamp >= x$begin, timestamp <= x$end))
        )
      )

      # Attach trial begin/end times
      y %>% mutate(begin = x$begin,
                   end = x$end,
                   .after = recording_id)
    }

    obj$data <- trial_events %>%
      group_by(trial) %>%
      group_modify(~ chopper(.x, obj$data), .keep = T) %>%
      ungroup() %>%
      mutate(trial = as.integer(trial)) %>%
      relocate(trial, .after = recording_id) %>%
      dplyr::arrange(trial)

    # TODO repivot blinks & fixations

    obj$has_trials <- TRUE
  }

  return(obj)
}

#' chop_world_video_trials: Chop world (scene) video into trials
#'
#' @param df A trial-based neon_data object
#' @param videofile Name of video file
#' @param trial TODO
#' @param overwrite Overwrite existing files (default = TRUE)
#' @param silent Ignore stdout of ffmpeg call (default = TRUE)
#'
#' @return
#' @export
#'
#' @examples
chop_world_video_trials <- function(obj,
                             save_dir = NULL,
                             overwrite = T,
                             silent = T
) {
  stopifnot(obj$has_trials)

  videofile <- obj$world_videofile
  if (is.null(save_dir))
    save_dir <- file.path(fs::path_dir(videofile), "chopped_video")

  if (!dir.exists(save_dir))
    dir.create(save_dir)

  # Use the start of the section hash as basename for segments
  name <- sub("\\_.*", "", basename(videofile))

  for (trial in 1:nrow(obj$data)) {
    t_start <- obj$data[trial,]$world_timestamps[[1]]$time_from_frame1[1] %>%
      as.numeric()
    n_frames <- length(obj$data[trial,]$world_timestamps[[1]]$time_from_frame1)
    t_end <- obj$data[trial,]$world_timestamps[[1]]$time_from_frame1[n_frames] %>%
      as.numeric()

    fname <- paste0(name, "_", round(t_start, 2), "-", round(t_end,2),
                    "_trial_", obj$data[trial,]$trial, ".mp4")

    call <- paste0("ffmpeg -ss ", round(t_start, 2),
                  " -i ", videofile,
                  " -codec copy",
                  " -frames:v ", n_frames, " ",
                  ifelse(overwrite, " -y ", " -n "),
                  file.path(save_dir, fname)
                  )

    system(call, ignore.stderr = silent)
  }
}
