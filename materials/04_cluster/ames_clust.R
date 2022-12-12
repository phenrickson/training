library(tidyverse)
library(data.table)
library(tidymodels)
tidymodels_prefer()


# read in ames training
ames_train = fread(here::here("data", "ames_train.csv"))

# k means
library(tidyclust)

kmeans_spec <- k_means(num_clusters = 4) %>%
        set_engine("ClusterR")

kmeans_spec

rec_spec <- recipe(~ ., data = ames) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors()) %>%
        step_normalize(all_numeric_predictors()) %>%
        step_pca(all_numeric_predictors(), threshold = 0.8)

kmeans_wf <- workflow(rec_spec, kmeans_spec)
