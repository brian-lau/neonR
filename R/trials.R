#' chop_trials: Chop continuous recording into trials
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
chop_trials <- function(df,
                        start_postfix = ".begin",
                        end_postfix = ".end"
) {
  trial_events <- df$events[[1]] %>%
    select(-type) %>%
    filter(grepl("^[[:digit:]]+", name))

  trial_events %<>% separate(name, c("trial", "event"))
  trial_events %<>% pivot_wider(names_from = event, values_from = timestamp)

  if ("blinks" %in% names(df)) {
    df$blinks[[1]] %<>% pivot_longer(cols = ends_with("timestamp"),
                                    names_to = "event",
                                    values_to = "timestamp")
  }

  if ("fixations" %in% names(df)) {
    df$fixations[[1]] %<>% pivot_longer(cols = ends_with("timestamp"),
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

  trial_events %>%
    group_by(trial) %>%
    group_modify(~ chopper(.x, df), .keep = T) %>%
    ungroup() %>%
    relocate(trial, .after = recording_id)

  # TODO repivot blinks & fixations
}
