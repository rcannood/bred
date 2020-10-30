#' @export
qsub_submit_bred <- dynutils::inherit_default_params(
  list(calculate_target_importance),
  function(
    expr_targets,
    expr_regulators = expr_targets,
    samples,
    regulators,
    targets,
    num_trees,
    min_node_size,
    min_importance,
    min_cw_importance,
    sigmoid_mean,
    sigmoid_sd,
    qsub_config = NULL
  ) {
    requireNamespace("qsub")

    if (is.null(qsub_config)) {
      qsub_config <- qsub::get_default_qsub_config()
    }

    x <- 1 # dummy data to pass

    qsub::qsub_lapply(
      X = seq_along(targets),
      qsub_config = qsub::override_qsub_config(
        qsub_config = qsub_config,
        max_wall_time = "12:00:00",
        memory = "10G",
        name = "bred",
        wait = FALSE,
        stop_on_error = FALSE,
        remove_tmp_folder = FALSE
      ),
      qsub_packages = c("bred", "rangercase", "dplyr", "magrittr", "tidyr"),
      qsub_environment = c("x"),
      FUN = calculate_target_importance,
      # pass data and other parameters
      expr_targets = expr_targets,
      expr_regulators = expr_regulators,
      samples = samples,
      regulators = regulators,
      targets = targets,
      num_trees = num_trees,
      min_node_size = min_node_size,
      min_importance = min_importance,
      min_cw_importance = min_cw_importance,
      sigmoid_mean = sigmoid_mean,
      sigmoid_sd = sigmoid_sd
    )
  }
)
