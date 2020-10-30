#' bred: Inferring Case-wise Regulatory Networks
#'
#' Network inference methods are computational tools which use
#' large omics datasets to predict which genes are regulated by which
#' transcription factors. Since regulatory interactions are context-dependent,
#' attempting to model regulatory dynamics in the form of a single regulatory
#' network may have little relevance. This package allows to predict
#' regulatory networks for individual samples, which can then be clustered
#' together.
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import dynutils
#' @import purrr
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#'
#' @docType package
#' @name bred
NULL

