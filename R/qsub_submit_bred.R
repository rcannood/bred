#' @importFrom qsub get_default_qsub_config qsub_lapply override_qsub_config
#'
#' @export
qsub_submit_bred <- dynutils::inherit_default_params(
  list(calculate_target_importance),
  function(
    expr,
    samples,
    regulators,
    targets,
    num_trees,
    num_variables_per_split,
    num_samples_per_tree,
    min_node_size,
    interaction_importance_filter,
    sigmoid_mean,
    sigmoid_sd,
    qsub_config = qsub::get_default_qsub_config()
  ) {
    x <- 1 # dummy data to pass

    qsub::qsub_lapply(
      X = seq_along(targets),
      qsub_config = qsub::override_qsub_config(
        qsub_config = qsub_config,
        name = "bred",
        wait = FALSE,
        stop_on_error = FALSE,
        remove_tmp_folder = FALSE,
        batch_tasks = round(length(targets) / 1000) # split work into ~ 1000 tasks
      ),
      qsub_packages = c("randomForest", "dynutils", "dplyr", "magrittr"),
      qsub_environment = c("x"),
      FUN = calculate_target_importance,
      # pass data and other parameters
      expr = expr,
      samples = samples,
      regulators = regulators,
      targets = targets,
      num_trees = num_trees,
      num_variables_per_split = num_variables_per_split,
      num_samples_per_tree = num_samples_per_tree,
      min_node_size = min_node_size,
      interaction_importance_filter = interaction_importance_filter,
      sigmoid_mean = sigmoid_mean,
      sigmoid_sd = sigmoid_sd
    )
  }
)
