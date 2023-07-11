library(patchwork)

# simulating the model with the posterior draws and plotting population-level
# posteriors
source("code/scripts/calculate_pop_post_preds.R")

# HAI POPULATION PREDS
p_pop_hai_post_preds <- plot_post_preds(dt_pop_post_hai_all,
                                        dt_stan_hai_flu_all,
                                        ind_flag = FALSE,
                                        strain_flag = TRUE,
                                        cluster_flag = FALSE)  +
  labs(title = "HAI titres")

# NEUTRALISING POPULATION PREDS
p_pop_neut_post_preds <- plot_post_preds(dt_pop_post_neut_all,
                                         dt_stan_neut_flu_all,
                                         ind_flag = FALSE,
                                         strain_flag = TRUE,
                                         cluster_flag = FALSE)  +
  labs(title = "Neutralising antibody titres")

p_titres <- (p_pop_hai_post_preds/p_pop_neut_post_preds)

ggsave("results/covariate_level/covariate_titre_fits.pdf",
       p_titres,
       width = 8,
       height = 8)

ggsave("results/covariate_level/covariate_titre_fits.png",
       p_titres,
       width = 8,
       height = 8,
       bg = "white")

# CLUSTER POPULATION PREDS
p_pop_cluster_post_preds <- plot_post_preds(dt_pop_post_cluster_all,
                                            dt_stan_data_cluster_all,
                                            ind_flag = FALSE,
                                            strain_flag = FALSE,
                                            cluster_flag = TRUE)  +
  labs(title = "Cytokine clusters")

p_clusters <- p_pop_cluster_post_preds

ggsave("results/covariate_level/covariate_cluster_fits.pdf",
       p_clusters,
       width = 8,
       height = 8)

ggsave("results/covariate_level/covariate_cluster_fits.png",
       p_clusters,
       width = 8,
       height = 8,
       bg = "white")

