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
  min_sc_importance = .01,
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
    # parameters that could be added back
    # mtry = num_variables_per_split,
    # sample.fraction = .5,
    # max.depth = max_depth,
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
    filter(importance > min_importance)

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
      reshape2::melt(limp_sc, varnames = c("cell_id", "regulator"), value.name = "importance_sc"),
      by = c("cell_id", "regulator")
    ) %>%
    as_tibble() %>%
    arrange(desc(importance)) %>%
    filter(importance > min_sc_importance)

  lst(
    importance = imp %>% transmute(
      regulator = factor(feature_id, levels = regulators),
      target = factor(target, levels = targets),
      importance,
      effect
    ),
    importance_sc = limp_df %>% transmute(
      cell_id = factor(as.character(cell_id), levels = samples),
      regulator = factor(as.character(regulator), levels = regulators),
      target = factor(target, levels = targets),
      importance,
      importance_sc
    )
  )
}
