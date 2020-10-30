library(tidyverse)

human_tf_info <- read_csv("http://humantfs.ccbr.utoronto.ca/download/v_1.01/DatabaseExtract_v_1.01.csv")

human_tf <- human_tf_info %>% filter(`Is TF?` == "Yes") %>% pull(`Ensembl ID`)

tfs <- list(
  human = human_tf
)

usethis::use_data(tfs, overwrite = TRUE)
