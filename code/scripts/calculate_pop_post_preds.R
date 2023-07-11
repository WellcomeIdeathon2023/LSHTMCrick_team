source("code/scripts/extract_draws.R")

#--- SUMMARISING HAI TITRE FITS
dt_pop_post_sum_hai_flu_A_10 <- calculate_post_preds(dt_pop_post_hai_flu_A_10,
                                                     seq(-7, 28, 0.25),
                                                     ind_flag = FALSE,
                                                     1000)

dt_pop_post_sum_hai_flu_A_59 <- calculate_post_preds(dt_pop_post_hai_flu_A_59,
                                                     seq(-7, 28, 0.25),
                                                     ind_flag = FALSE,
                                                     1000)

dt_pop_post_sum_hai_flu_B_60 <- calculate_post_preds(dt_pop_post_hai_flu_B_60,
                                                     seq(-7, 28, 0.25),
                                                     ind_flag = FALSE,
                                                     1000)

# Adding strains explicitly for plots
dt_pop_post_sum_hai_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_pop_post_sum_hai_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_pop_post_sum_hai_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_pop_post_hai_all <- rbind(dt_pop_post_sum_hai_flu_A_10,
                             dt_pop_post_sum_hai_flu_A_59,
                             dt_pop_post_sum_hai_flu_B_60)

# Adding strains explicitly for plots
dt_stan_hai_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_stan_hai_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_stan_hai_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_stan_hai_flu_all <- rbind(dt_stan_hai_flu_A_10,
                             dt_stan_hai_flu_A_59,
                             dt_stan_hai_flu_B_60)

#--- SUMMARISING NEUT FITS
# summarising neut titre fits
dt_pop_post_sum_neut_flu_A_10 <- calculate_post_preds(dt_pop_post_neut_flu_A_10,
                                                      seq(-7, 28, 0.25),
                                                      ind_flag = FALSE,
                                                      1000)


dt_pop_post_sum_neut_flu_A_59 <- calculate_post_preds(dt_pop_post_neut_flu_A_59,
                                                      seq(-7, 28, 0.25),
                                                      ind_flag = FALSE,
                                                      1000)

dt_pop_post_sum_neut_flu_B_60 <- calculate_post_preds(dt_pop_post_neut_flu_B_60,
                                                      seq(-7, 28, 0.25),
                                                      ind_flag = FALSE,
                                                      1000)

# Adding strains explicitly for plots
dt_pop_post_sum_neut_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_pop_post_sum_neut_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_pop_post_sum_neut_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_pop_post_neut_all <- rbind(dt_pop_post_sum_neut_flu_A_10,
                              dt_pop_post_sum_neut_flu_A_59,
                              dt_pop_post_sum_neut_flu_B_60)

# Adding strains explicitly for plots
dt_stan_neut_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_stan_neut_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_stan_neut_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_stan_neut_flu_all <- rbind(dt_stan_neut_flu_A_10,
                             dt_stan_neut_flu_A_59,
                             dt_stan_neut_flu_B_60)

#--- SUMMARISING CLUSTER TITRE FITS
dt_pop_post_sum_cluster_1 <- calculate_post_preds(dt_pop_post_cluster_1,
                                                  seq(-7, 28, 0.25),
                                                  ind_flag = FALSE,
                                                  1000)


dt_pop_post_sum_cluster_2 <- calculate_post_preds(dt_pop_post_cluster_2,
                                                  seq(-7, 28, 0.25),
                                                  ind_flag = FALSE,
                                                  1000)

dt_pop_post_sum_cluster_3 <- calculate_post_preds(dt_pop_post_cluster_3,
                                                  seq(-7, 28, 0.25),
                                                  ind_flag = FALSE,
                                                  1000)

dt_pop_post_sum_cluster_4 <- calculate_post_preds(dt_pop_post_cluster_4,
                                                  seq(-7, 28, 0.25),
                                                  ind_flag = FALSE,
                                                  1000)

# Adding strains explicitly for plots
dt_pop_post_sum_cluster_1[, Cluster := "Cytokine cluster 1"]
dt_pop_post_sum_cluster_2[, Cluster := "Cytokine cluster 2"]
dt_pop_post_sum_cluster_3[, Cluster := "Cytokine cluster 3"]
dt_pop_post_sum_cluster_4[, Cluster := "Cytokine cluster 4"]

dt_pop_post_cluster_all <- rbind(dt_pop_post_sum_cluster_1,
                                 dt_pop_post_sum_cluster_2,
                                 dt_pop_post_sum_cluster_3,
                                 dt_pop_post_sum_cluster_4)

# Adding strains explicitly for plots
dt_stan_data_cluster_1[, Cluster := "Cytokine cluster 1"]
dt_stan_data_cluster_2[, Cluster := "Cytokine cluster 2"]
dt_stan_data_cluster_3[, Cluster := "Cytokine cluster 3"]
dt_stan_data_cluster_4[, Cluster := "Cytokine cluster 4"]

dt_stan_data_cluster_all <- rbind(dt_stan_data_cluster_1,
                                  dt_stan_data_cluster_2,
                                  dt_stan_data_cluster_3,
                                  dt_stan_data_cluster_4)
