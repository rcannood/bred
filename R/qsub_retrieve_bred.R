qsub_retrieve_bred <- function(
  handle,
  force = FALSE
) {
  wait <- if (force) "just_do_it" else FALSE

  requireNamespace("qsub")

  # retrieve data
  grn <- qsub::qsub_retrieve(
    handle,
    wait = wait
  )

  # remove unfinished executions
  grn[map_lgl(grn, ~ length(.) == 1 && is.na(.))] <- NULL

  # return combined results
  importance <-
    map_df(grn, "importance") %>%
    arrange(desc(importance)) %>%
    mutate(
      interaction_id = paste0(regulator, "->", target),
      interaction_id = factor(interaction_id, levels = interaction_id)
    )

  cw_importance <-
    grn %>%
    inner_join(importance %>% select(-importance), by = c("regulator", "target")) %>%
    arrange(desc(importance))

  lst(importance, cw_importance)
}
