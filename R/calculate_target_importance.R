#' Predict case-wise regulatory interactions for one target
#'
#' @param target_ix The index of the target
#' @param expr_targets The expression matrix of the targets
#' @param expr_regulators The expression matrix of the regulators.
#'   In most cases, this will be the same as `expr_targets`.
#' @param samples The sample names for which to compute the importance values
#' @param regulators The regulator names for which to compute the importance values
#' @param targets The target names for which to compute the importance values
#' @param num_trees The number of trees (ranger param)
#' @param min_node_size Minimal node size to split (ranger param)
#' @param min_importance Retain only (regulator, target) interactions with a sum importance score higher than this threshold.
#' @param min_cw_importance Retain only (regulator, target) interactions with a casewise importance score higher than this threshold.
#' @param sigmoid_mean Scale the casewise importances with a sigmoid with this mean
#' @param sigmoid_sd Scale the casewise importances with a sigmoid with this sd
#'
#' @importFrom rangercase ranger
#' @importFrom reshape2 melt
#' @importFrom sigmoid sigmoid
#' @importFrom stats sd pnorm
#' @importFrom Matrix t
#' @importFrom reshape2 melt
#'
#' @export
calculate_target_importance <- function(
  target_ix,
  expr_targets,
  expr_regulators,
  samples,
  regulators,
  targets,
  num_trees = 10000,
  min_node_size = 10,
  min_importance = .01,
  min_cw_importance = .01,
  sigmoid_mean = mean(expr_regulators[expr_regulators != 0]),
  sigmoid_sd = sd(expr_regulators[expr_regulators != 0])
) {

  target <- targets[[target_ix]]
  regs <- setdiff(regulators, target)
  target_expr <- scale(expr_targets[,target])[,1]

  data <- data.frame(
    PREDICT = target_expr,
    as.matrix(expr_regulators[,regs]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  rf <- rangercase::ranger(
    data = data,
    dependent.variable.name = "PREDICT",
    verbose = TRUE,
    num.threads = 1,
    importance = "permutation",
    num.trees = num_trees,
    min.node.size = min_node_size,
    local.importance = TRUE
  )

  cat("Selecting most important regulators\n")

  imp <- tibble(
    feature_id = names(rf$variable.importance),
    importance = rf$variable.importance,
    effect = ifelse(importance == 0, 0, rf$variable.importance.cor),
    ix = seq_along(feature_id)
  ) %>%
    arrange(desc(importance)) %>%
    filter(importance >= min_importance)

  limp <- Matrix::t(rf$variable.importance.casewise[imp$feature_id, , drop = FALSE])

  # downscale importance if regulator is not expressed
  # ... it can't be regulating anything if it is not expressed ...
  expr_reg_sc <- stats::pnorm(
    as.matrix(expr_regulators[rownames(limp),colnames(limp)]),
    mean = sigmoid_mean,
    sd = sigmoid_sd
  )
  limp_sc <- limp * expr_reg_sc

  limp_df <-
    left_join(
      reshape2::melt(limp, varnames = c("cell_id", "regulator"), value.name = "importance"),
      reshape2::melt(limp_sc, varnames = c("cell_id", "regulator"), value.name = "cw_importance"),
      by = c("cell_id", "regulator")
    ) %>%
    as_tibble() %>%
    arrange(desc(importance)) %>%
    filter(importance > min_cw_importance)

  lst(
    importance = imp %>% transmute(
      regulator = factor(feature_id, levels = regulators),
      target = factor(target, levels = targets),
      importance,
      effect
    ),
    cw_importance = limp_df %>% transmute(
      cell_id = factor(as.character(cell_id), levels = samples),
      regulator = factor(as.character(regulator), levels = regulators),
      target = factor(target, levels = targets),
      importance,
      cw_importance
    )
  )
}
