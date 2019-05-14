#' @importFrom qsub qsub_retrieve
qsub_retrieve_bred <- function(
  handle,
  interaction_importance_filter = .01,
  force = FALSE
) {
  wait <- if (force) "just_do_it" else FALSE

  # retrieve data
  grn <- qsub::qsub_retrieve(
    handle,
    wait = wait,
    post_fun = function(i, out) {
      # shortcut
      if (!any(out$adj_importance > interaction_importance_filter)) return(out)

      # if grn is too big, filter out low value interactions
      out %>%
        group_by(regulator) %>%
        filter(mean(adj_importance) > interaction_importance_filter) %>%
        ungroup()
    }
  )

  # remove unfinished executions
  grn[map_lgl(grn, ~ length(.) == 1 && is.na(.))] <- NULL

  # return combined results
  bind_rows(grn)
}
