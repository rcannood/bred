#' @importFrom qsub qsub_retrieve
qsub_retrieve_bred <- function(
  handle,
  force = FALSE
) {
  wait <- if (force) "just_do_it" else FALSE

  # retrieve data
  grn <- qsub::qsub_retrieve(
    handle,
    wait = wait#,
    # post_fun = function(i, li) {
    #   li$importance <- li$importance %>% filter(importance > .01)
    #   li$importance_sc <- li$importance_sc %>% filter(importance_sc > .01) %>%
    #     inner_join(li$importance %>% select(regulator, target), by = c("regulator", "target"))
    #   li
    # }
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

  importance_sc <-
    grn %>%
    inner_join(importance %>% select(-importance), by = c("regulator", "target")) %>%
    arrange(desc(importance))

  lst(importance, importance_sc)
}
