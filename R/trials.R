#' chop_trials: Chop continuous recording into trials
#'
#' @param obj
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
                        end_postfix = ".end"
) {
  # Trial tags start with number
  trial_events <- obj[["data"]]$events[[1]] %>%
    select(-type) %>%
    filter(grepl("^[[:digit:]]+", name))

  # TODO check postfixes

  trial_events %<>% tidyr::separate(name, c("trial", "event"))
  trial_events %<>% pivot_wider(names_from = event, values_from = timestamp)

  # TODO verify start and end for each trial
  has_trials <- TRUE

  if (has_trials) {
    if ("blinks" %in% names(obj[["data"]])) {
      obj[["data"]]$blinks[[1]] %<>% pivot_longer(cols = ends_with("timestamp"),
                                                  names_to = "event",
                                                  values_to = "timestamp")
    }

    if ("fixations" %in% names(obj[["data"]])) {
      obj[["data"]]$fixations[[1]] %<>% pivot_longer(cols = ends_with("timestamp"),
                                                     names_to = "event",
                                                     values_to = "timestamp")
    }

    chopper <- function(x, y) {
      excluded_vars <- c("recording_id")
      y %<>% mutate(
        across(
          -one_of(excluded_vars),
          ~ list(.x[[1]] %>% filter(timestamp >= x$begin, timestamp <= x$end))
        )
      )
      y %>% mutate(begin = x$begin,
                   end = x$end,
                   .after = recording_id)
    }

    obj[["data"]] <- trial_events %>%
      group_by(trial) %>%
      group_modify(~ chopper(.x, obj[["data"]]), .keep = T) %>%
      ungroup() %>%
      relocate(trial, .after = recording_id)

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
chop_world_video_trials <- function(df,
                             videofile,
                             trial = NULL,
                             overwrite = T,
                             silent = T
) {
  # call <- paste0("ffprobe -v error -select_streams v:0 -count_packets ",
  #               "-show_entries stream=nb_read_packets -of csv=p=0 ", videofile)
  # n_total_frames <- system(call, intern = TRUE) %>% as.numeric()

  name <- gsub(".mp4", "", videofile)

  for (trial in 1:nrow(df)) {
    t_start <- df[trial,]$world_timestamps[[1]]$time_from_frame1[1] %>% as.numeric()
    n_frames <- length(df[trial,]$world_timestamps[[1]]$time_from_frame1)
    call = paste0("ffmpeg -ss ", round(t_start, 2),
                  " -i ", videofile,
                  " -codec copy",
                  " -frames:v ", n_frames, " ",
                  ifelse(overwrite, " -y ", " -n "),
                  paste0(name, "_trial_", df[trial,]$trial, ".mp4")
                  )
    #print(call)
    system(call, ignore.stderr = silent)
  }
}
