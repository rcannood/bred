#' @importFrom randomForest randomForest
#' @importFrom reshape2 melt
#' @importFrom sigmoid sigmoid
#'
#' @export
calculate_target_importance <- function(
  target_ix,
  expr_targets,
  expr_regulators = expr_targets,
  samples,
  regulators,
  targets,
  num_trees = 10000,
  num_variables_per_split = 100,
  num_samples_per_tree = 250,
  min_node_size = 10,
  interaction_importance_filter = .01,
  sigmoid_mean = mean(expr_regulators[expr_regulators != 0]),
  sigmoid_sd = sd(expr_regulators[expr_regulators != 0])
) {
  target <- targets[[target_ix]]

  regs <- setdiff(regulators, target)

  rf <- randomForest::randomForest(
    expr_regulators[,regs],
    scale(expr_targets[,target])[,1],
    mtry = num_variables_per_split,
    ntree = num_trees,
    sampsize = num_samples_per_tree,
    nodesize = min_node_size,
    importance = TRUE,
    localImp = TRUE
  )

  reg_tar_map <- match(regulators, targets)

  rf$localImportance %>%
    reshape2::melt(varnames = c("regulator", "sample"), value.name = "importance") %>%
    as_data_frame() %>%
    mutate(
      # turn columns into factors
      sample = factor(as.character(sample), levels = samples),
      regulator = factor(as.character(regulator), levels = regulators),
      target = factor(targets[[target_ix]], levels = targets),
      # fetch regulator expression
      expr_reg = expr_regulators[cbind(sample, reg_tar_map[regulator])],
      expr_reg_sc = sigmoid::sigmoid((expr_reg - sigmoid_mean) * sigmoid_sd),
      # downscale importance if regulator is not expressed
      # ... it can't be regulating anything if it is not expressed ...
      adj_importance = expr_reg_sc * importance
    ) %>%
    group_by(regulator) %>%
    filter(mean(adj_importance) > interaction_importance_filter) %>% # reduce size of grn...
    ungroup() %>%
    select(sample, regulator, target, importance, adj_importance)
}
