#' Perform LMDS dimred and Leiden clustering on the bred output
#'
#' @param cw_importance Case-wise importances
#' @param lmds_ndim Number of LMDS dimensions
#' @param knn K-nearest-neighbours
#' @param use_scaled_imp Whether or not to used the raw importance values or the scaled importance values to compute the KNN.
#'
#' @importFrom Matrix sparseMatrix
#' @importFrom lmds lmds
#' @importFrom RANN nn2
#' @importFrom igraph graph_from_data_frame cluster_louvain layout_with_fr
#' @export
dimred_and_cluster <- function(cw_importance, lmds_ndim = 20, knn = 100, use_scaled_imp = TRUE) {
  sample_ids <- levels(cw_importance$sample_id)
  interaction_ids <- levels(cw_importance$interaction_id)

  cw_imp_mat <- Matrix::sparseMatrix(
    i = cw_importance$sample_id %>% as.integer,
    j = cw_importance$interaction_id %>% as.integer,
    x = cw_importance[[if (use_scaled_imp) "importance_sc" else "importance"]],
    dims = c(length(sample_ids), length(interaction_ids)),
    dimnames = list(sample_ids, interaction_ids)
  )

  dimred_lmds <- lmds::lmds(cw_imp_mat, ndim = lmds_ndim, distance_method = "spearman")
  rm(cw_imp_mat)
  gc()

  # compute knn
  knn <- RANN::nn2(dimred_lmds, k = knn + 1)
  knn$nn.dists <- knn$nn.dists[,-1]
  knn$nn.idx <- knn$nn.idx[,-1]

  # perform louvain clustering and FR dimred
  knndf <-
    inner_join(
      reshape2::melt(knn$nn.idx, varnames = c("i", "nn"), value.name = "j"),
      reshape2::melt(knn$nn.dists, varnames = c("i", "nn"), value.name = "dist"),
      by = c("i", "nn")
    ) %>%
    as_tibble() %>%
    mutate(i2 = pmin(i, j), j2 = pmax(i, j)) %>%
    select(i = i2, j = j2, dist) %>%
    unique() %>%
    arrange(dist) %>%
    mutate(weight = 1 / dist)

  gr <- igraph::graph_from_data_frame(
    knndf %>% select(i, j, dist),
    vertices = seq_len(nrow(dimred_lmds)),
    directed = FALSE
  )
  # cl <- leiden::leiden(gr)
  cl <- igraph::cluster_louvain(gr)
  cluster <- cl$membership
  names(cluster) <- rownames(dimred_lmds)

  dimred_fr <- igraph::layout_with_fr(gr)
  rownames(dimred_fr) <- rownames(dimred_lmds)
  colnames(dimred_fr) <- paste0("comp_", seq_len(ncol(dimred_fr)))

  lst(
    dimred_lmds,
    knn,
    dimred_fr,
    cluster
  )
}
