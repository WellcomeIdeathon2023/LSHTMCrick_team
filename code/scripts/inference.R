source("code/scripts/setup.R")
source("code/scripts/prepare_inference.R")

# RUNNING MODEL FOR HAI TITRES
stan_fit_hai_flu_A_10 <- run_stan_model(stan_data_hai_flu_A_10)
stan_fit_hai_flu_A_59 <- run_stan_model(stan_data_hai_flu_A_59)
stan_fit_hai_flu_B_60 <- run_stan_model(stan_data_hai_flu_B_60)

stan_fit_hai_flu_A_10$save_object(file = "data/model_data/stan_fits/sdy180/hai/fit_flu_A_10.RData")
stan_fit_hai_flu_A_59$save_object(file = "data/model_data/stan_fits/sdy180/hai/fit_flu_A_59.RData")
stan_fit_hai_flu_B_60$save_object(file = "data/model_data/stan_fits/sdy180/hai/fit_flu_B_60.RData")

# RUNNING MODEL FOR NEUTS
stan_fit_neut_flu_A_10 <- run_stan_model(stan_data_neut_flu_A_10)
stan_fit_neut_flu_A_59 <- run_stan_model(stan_data_neut_flu_A_59)
stan_fit_neut_flu_B_60 <- run_stan_model(stan_data_neut_flu_B_60)

stan_fit_neut_flu_A_10$save_object(file = "data/model_data/stan_fits/sdy180/neuts/fit_flu_A_10.RData")
stan_fit_neut_flu_A_59$save_object(file = "data/model_data/stan_fits/sdy180/neuts/fit_flu_A_59.RData")
stan_fit_neut_flu_B_60$save_object(file = "data/model_data/stan_fits/sdy180/neuts/fit_flu_B_60.RData")

# RUNNING MODEL FOR CYTOKINE CLUSTERS
stan_fit_flu_cluster_1 <- run_stan_model(stan_data_cluster_1)
stan_fit_flu_cluster_2 <- run_stan_model(stan_data_cluster_2)
stan_fit_flu_cluster_3 <- run_stan_model(stan_data_cluster_3)
stan_fit_flu_cluster_4 <- run_stan_model(stan_data_cluster_4)

stan_fit_flu_cluster_1$save_object(file = "data/model_data/stan_fits/sdy180/cytokine_clusters/fit_flu_cluster_1.RData")
stan_fit_flu_cluster_2$save_object(file = "data/model_data/stan_fits/sdy180/cytokine_clusters/fit_flu_cluster_2.RData")
stan_fit_flu_cluster_3$save_object(file = "data/model_data/stan_fits/sdy180/cytokine_clusters/fit_flu_cluster_3.RData")
stan_fit_flu_cluster_4$save_object(file = "data/model_data/stan_fits/sdy180/cytokine_clusters/fit_flu_cluster_4.RData")

