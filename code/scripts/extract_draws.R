# for this script to work, either read in saved fits or run the inference script
# reading in data file is preferred, so it is set to the default option

# source("code/scripts/inference.R")
source("code/scripts/setup.R")
source("code/scripts/prepare_inference.R")

# EXTRACTING HAI TITRE POSTERIOR DRAWS
stan_fit_hai_flu_A_10 <- readRDS("data/model_data/stan_fits/sdy180/hai/fit_flu_A_10.RData")
stan_fit_hai_flu_A_59 <- readRDS("data/model_data/stan_fits/sdy180/hai/fit_flu_A_59.RData")
stan_fit_hai_flu_B_60 <- readRDS("data/model_data/stan_fits/sdy180/hai/fit_flu_B_60.RData")

# population level draws
dt_pop_post_hai_flu_A_10 <- extract_posterior_draws(stan_fit_hai_flu_A_10,
                                                    structure_arg = "wide",
                                                    ind_flag = FALSE)

dt_pop_post_hai_flu_A_59 <- extract_posterior_draws(stan_fit_hai_flu_A_59,
                                                    structure_arg = "wide",
                                                    ind_flag = FALSE)

dt_pop_post_hai_flu_B_60 <- extract_posterior_draws(stan_fit_hai_flu_B_60,
                                                    structure_arg = "wide",
                                                    ind_flag = FALSE)

# inidividual-level draws
dt_ind_post_hai_flu_A_10 <- extract_posterior_draws(stan_fit_hai_flu_A_10,
                                                    structure_arg = "wide",
                                                    ind_flag = TRUE)

dt_ind_post_hai_flu_A_59 <- extract_posterior_draws(stan_fit_hai_flu_A_59,
                                                    structure_arg = "wide",
                                                    ind_flag = TRUE)

dt_ind_post_hai_flu_B_60 <- extract_posterior_draws(stan_fit_hai_flu_B_60,
                                                    structure_arg = "wide",
                                                    ind_flag = TRUE)

# EXTRACTING NEUT POSTERIOR DRAWS
stan_fit_neut_flu_A_10 <- readRDS("data/model_data/stan_fits/sdy180/neuts/fit_flu_A_10.RData")
stan_fit_neut_flu_A_59 <- readRDS("data/model_data/stan_fits/sdy180/neuts/fit_flu_A_59.RData")
stan_fit_neut_flu_B_60 <- readRDS("data/model_data/stan_fits/sdy180/neuts/fit_flu_B_60.RData")

# population level draws
dt_pop_post_neut_flu_A_10 <- extract_posterior_draws(stan_fit_neut_flu_A_10,
                                                     structure_arg = "wide",
                                                     ind_flag = FALSE)

dt_pop_post_neut_flu_A_59 <- extract_posterior_draws(stan_fit_neut_flu_A_59,
                                                    structure_arg = "wide",
                                                    ind_flag = FALSE)

dt_pop_post_neut_flu_B_60 <- extract_posterior_draws(stan_fit_neut_flu_B_60,
                                                    structure_arg = "wide",
                                                    ind_flag = FALSE)

# inidividual-level draws
dt_ind_post_neut_flu_A_10 <- extract_posterior_draws(stan_fit_neut_flu_A_10,
                                                     structure_arg = "wide",
                                                     ind_flag = TRUE)

dt_ind_post_neut_flu_A_59 <- extract_posterior_draws(stan_fit_neut_flu_A_59,
                                                     structure_arg = "wide",
                                                     ind_flag = TRUE)

dt_ind_post_neut_flu_B_60 <- extract_posterior_draws(stan_fit_neut_flu_B_60,
                                                     structure_arg = "wide",
                                                     ind_flag = TRUE)

# EXTRACTING CYTOKINE CLUSTER POSTERIOR DRAWS
stan_fit_cluster_1 <- readRDS("data/model_data/stan_fits/sdy180/cytokine_clusters/fit_flu_cluster_1.RData")
stan_fit_cluster_2 <- readRDS("data/model_data/stan_fits/sdy180/cytokine_clusters/fit_flu_cluster_2.RData")
stan_fit_cluster_3 <- readRDS("data/model_data/stan_fits/sdy180/cytokine_clusters/fit_flu_cluster_3.RData")
stan_fit_cluster_4 <- readRDS("data/model_data/stan_fits/sdy180/cytokine_clusters/fit_flu_cluster_4.RData")

# population level draws
dt_pop_post_cluster_1 <- extract_posterior_draws(stan_fit_cluster_1,
                                                 structure_arg = "wide",
                                                 ind_flag = FALSE)

dt_pop_post_cluster_2 <- extract_posterior_draws(stan_fit_cluster_2,
                                                 structure_arg = "wide",
                                                 ind_flag = FALSE)

dt_pop_post_cluster_3 <- extract_posterior_draws(stan_fit_cluster_3,
                                                 structure_arg = "wide",
                                                 ind_flag = FALSE)

dt_pop_post_cluster_4 <- extract_posterior_draws(stan_fit_cluster_4,
                                                 structure_arg = "wide",
                                                 ind_flag = FALSE)

# inidividual-level draws
dt_ind_post_cluster_1 <- extract_posterior_draws(stan_fit_cluster_1,
                                                 structure_arg = "wide",
                                                 ind_flag = TRUE)

dt_ind_post_cluster_2 <- extract_posterior_draws(stan_fit_cluster_2,
                                                 structure_arg = "wide",
                                                 ind_flag = TRUE)

dt_ind_post_cluster_3 <- extract_posterior_draws(stan_fit_cluster_3,
                                                 structure_arg = "wide",
                                                 ind_flag = TRUE)

dt_ind_post_cluster_4 <- extract_posterior_draws(stan_fit_cluster_4,
                                                 structure_arg = "wide",
                                                 ind_flag = TRUE)

