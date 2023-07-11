#--- GTR FOR HAI TITRES
dt_sim_hai_flu_A_10 <- simulate_trajectories(seq(-7, 28, 0.25),
                                             dt_pop_post_hai_flu_A_10,
                                             1000,
                                             ind_flag = FALSE,
                                             by_args = NULL)

dt_sim_hai_flu_A_59 <- simulate_trajectories(seq(-7, 28, 0.25),
                                             dt_pop_post_hai_flu_A_59,
                                             1000,
                                             ind_flag = FALSE,
                                             by_args = NULL)

dt_sim_hai_flu_B_60 <- simulate_trajectories(seq(-7, 28, 0.25),
                                             dt_pop_post_hai_flu_B_60,
                                             1000,
                                             ind_flag = FALSE,
                                             by_args = NULL)

# Adding strains explicitly for plots
dt_sim_hai_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_sim_hai_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_sim_hai_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_sim_hai_flu_all <- rbind(dt_sim_hai_flu_A_10,
                            dt_sim_hai_flu_A_59,
                            dt_sim_hai_flu_B_60)

dt_gtr_hai_t_minus_7 <- dt_sim_hai_flu_all[, .SD[t == -7], by = "Strain"][, .(t, exp_titre = 5*2^exp_titre), by = "Strain"]
dt_gtr_hai_t_28 <- dt_sim_hai_flu_all[, .SD[t == 28], by = "Strain"][, .(t, exp_titre = 5*2^exp_titre), by = "Strain"]

setnames(dt_gtr_hai_t_minus_7,
         c("Strain", "t", "exp_titre"),
         c("Strain_t_minus_7", "t_minus_7", "y_t_minus_7"))

setnames(dt_gtr_hai_t_28,
         c("Strain", "t", "exp_titre"),
         c("Strain_t_28", "t_28", "y_t_28"))

dt_gtr_hai_proc <- cbind(dt_gtr_hai_t_minus_7,
                         dt_gtr_hai_t_28)

dt_gtr_hai <- dt_gtr_hai_proc[, gtr := y_t_28/y_t_minus_7, by = "Strain_t_28"]

dt_gtr_hai_sum <- dt_gtr_hai[, .(gtr_me = quantile(gtr, 0.5),
                                 gtr_lo = quantile(gtr, 0.025),
                                 gtr_hi = quantile(gtr, 0.975)),
                             by = "Strain_t_28"][, Type := "Modelled"]

#--- GTR FOR NEUTRALISING ANTIBODIES
dt_sim_neut_flu_A_10 <- simulate_trajectories(seq(-7, 28, 0.25),
                                             dt_pop_post_neut_flu_A_10,
                                             1000,
                                             ind_flag = FALSE,
                                             by_args = NULL)

dt_sim_neut_flu_A_59 <- simulate_trajectories(seq(-7, 28, 0.25),
                                             dt_pop_post_neut_flu_A_59,
                                             1000,
                                             ind_flag = FALSE,
                                             by_args = NULL)

dt_sim_neut_flu_B_60 <- simulate_trajectories(seq(-7, 28, 0.25),
                                             dt_pop_post_neut_flu_B_60,
                                             1000,
                                             ind_flag = FALSE,
                                             by_args = NULL)

# Adding strains explicitly for plots
dt_sim_neut_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_sim_neut_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_sim_neut_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_sim_neut_flu_all <- rbind(dt_sim_neut_flu_A_10,
                             dt_sim_neut_flu_A_59,
                             dt_sim_neut_flu_B_60)

dt_gtr_neut_t_minus_7 <- dt_sim_neut_flu_all[, .SD[t == -7], by = "Strain"][, .(t, exp_titre = 5*2^exp_titre), by = "Strain"]
dt_gtr_neut_t_28 <- dt_sim_neut_flu_all[, .SD[t == 28], by = "Strain"][, .(t, exp_titre = 5*2^exp_titre), by = "Strain"]

setnames(dt_gtr_neut_t_minus_7,
         c("Strain", "t", "exp_titre"),
         c("Strain_t_minus_7", "t_minus_7", "y_t_minus_7"))

setnames(dt_gtr_neut_t_28,
         c("Strain", "t", "exp_titre"),
         c("Strain_t_28", "t_28", "y_t_28"))

dt_gtr_neut_proc <- cbind(dt_gtr_neut_t_minus_7,
                          dt_gtr_neut_t_28)

dt_gtr_neut <- dt_gtr_neut_proc[, gtr := y_t_28/y_t_minus_7, by = "Strain_t_28"]

dt_gtr_neut_sum <- dt_gtr_neut[, .(gtr_me = quantile(gtr, 0.5),
                                   gtr_lo = quantile(gtr, 0.025),
                                   gtr_hi = quantile(gtr, 0.975)),
                               by = "Strain_t_28"][, Type := "Modelled"]


#--- GTR FOR CYTOKINE CLUSTERS ANTIBODIES
dt_sim_cluster_1 <- simulate_trajectories(seq(-7, 28, 0.25),
                                              dt_pop_post_cluster_1,
                                              1000,
                                              ind_flag = FALSE,
                                              by_args = NULL)

dt_sim_cluster_2 <- simulate_trajectories(seq(-7, 28, 0.25),
                                         dt_pop_post_cluster_2,
                                         1000,
                                         ind_flag = FALSE,
                                         by_args = NULL)

dt_sim_cluster_3 <- simulate_trajectories(seq(-7, 28, 0.25),
                                         dt_pop_post_cluster_3,
                                         1000,
                                         ind_flag = FALSE,
                                         by_args = NULL)

dt_sim_cluster_4 <- simulate_trajectories(seq(-7, 28, 0.25),
                                               dt_pop_post_cluster_4,
                                               1000,
                                               ind_flag = FALSE,
                                               by_args = NULL)

# Adding strains explicitly for plots
dt_sim_cluster_1[, Cluster := "Cytokine cluster 1"]
dt_sim_cluster_2[, Cluster := "Cytokine cluster 2"]
dt_sim_cluster_3[, Cluster := "Cytokine cluster 3"]
dt_sim_cluster_4[, Cluster := "Cytokine cluster 4"]

dt_sim_cluster_flu_all <- rbind(dt_sim_cluster_1,
                                dt_sim_cluster_2,
                                dt_sim_cluster_3,
                                dt_sim_cluster_4)

dt_gtr_cluster_t_minus_7 <- dt_sim_cluster_flu_all[, .SD[t == -7], by = "Cluster"][, .(t, exp_titre = 5*2^exp_titre), by = "Cluster"]
dt_gtr_cluster_t_28 <- dt_sim_cluster_flu_all[, .SD[t == 28], by = "Cluster"][, .(t, exp_titre = 5*2^exp_titre), by = "Cluster"]

setnames(dt_gtr_cluster_t_minus_7,
         c("Cluster", "t", "exp_titre"),
         c("Cluster_t_minus_7", "t_minus_7", "y_t_minus_7"))

setnames(dt_gtr_cluster_t_28,
         c("Cluster", "t", "exp_titre"),
         c("Cluster_t_28", "t_28", "y_t_28"))

dt_gtr_cluster_proc <- cbind(dt_gtr_cluster_t_minus_7,
                             dt_gtr_cluster_t_28)

dt_gtr_cluster <- dt_gtr_cluster_proc[, gtr := y_t_28/y_t_minus_7, by = "Cluster_t_28"]

dt_gtr_cluster_sum <- dt_gtr_cluster[, .(gtr_me = quantile(gtr, 0.5),
                                         gtr_lo = quantile(gtr, 0.025),
                                         gtr_hi = quantile(gtr, 0.975)),
                                     by = "Cluster_t_28"][, Type := "Modelled"]


#--- NOW ADDING GTR FROM RAW DATA
#--- HAI TITRE DATA
dt_hai_data_t_min <- dt_stan_hai_flu_all[, .SD[time == min(time)], by = "id"][, .(t_min_titre = mean(5*2^titre)), by = "Strain"]
dt_hai_data_t_max <- dt_stan_hai_flu_all[, .SD[time == max(time)], by = "id"][, .(t_max_titre = mean(5*2^titre)), by = "Strain"]

dt_hai_data_all <- merge(dt_hai_data_t_min, dt_hai_data_t_max)

dt_hai_data_all[, gtr_data := t_max_titre/t_min_titre, by = "Strain"][, Type := "Data"]

#--- NEUTRALISING ANTIBODY DATA
dt_neut_data_t_min <- dt_stan_neut_flu_all[, .SD[time == min(time)], by = "id"][, .(t_min_titre = mean(5*2^titre)), by = "Strain"]
dt_neut_data_t_max <- dt_stan_neut_flu_all[, .SD[time == max(time)], by = "id"][, .(t_max_titre = mean(5*2^titre)), by = "Strain"]

dt_neut_data_all <- merge(dt_neut_data_t_min, dt_neut_data_t_max)

dt_neut_data_all[, gtr_data := t_max_titre/t_min_titre, by = "Strain"][, Type := "Data"]

#--- CYTOKINE CLUSTERS
dt_cluster_data_t_min <- dt_stan_data_cluster_all[, .SD[time == min(time)], by = "id"][, .(t_min_titre = mean(5*2^titre)), by = "Cluster"]
dt_cluster_data_t_max <- dt_stan_data_cluster_all[, .SD[time == max(time)], by = "id"][, .(t_max_titre = mean(5*2^titre)), by = "Cluster"]

dt_cluster_data_all <- merge(dt_cluster_data_t_min, dt_cluster_data_t_max)

dt_cluster_data_all[, gtr_data := t_max_titre/t_min_titre, by = "Cluster"][, Type := "Data"]


# PLOTTING GTR ESTIMATES
p_gtr_hai <- ggplot() +
  geom_pointrange(data = dt_gtr_hai_sum,
                  aes(x = Strain_t_28,
                      y = gtr_me,
                      ymax = gtr_lo,
                      ymin = gtr_hi,
                      colour = Strain_t_28,
                      shape = Type)) +
  geom_point(data = dt_hai_data_all,
             aes(x = Strain,
                 y = gtr_data,
                 colour = Strain,
                 shape = Type)) +
  labs(title = "Strain",
       x = "Strain",
       y = "Geometric Titre Ratio") +
  guides(colour = "none") +
  theme_minimal()

p_gtr_neut <- ggplot() +
  geom_pointrange(data = dt_gtr_neut_sum,
                  aes(x = Strain_t_28,
                      y = gtr_me,
                      ymax = gtr_lo,
                      ymin = gtr_hi,
                      colour = Strain_t_28,
                      shape = Type)) +
  geom_point(data = dt_neut_data_all,
             aes(x = Strain,
                 y = gtr_data,
                 colour = Strain,
                 shape = Type)) +
  guides(colour = "none") +
  labs(title = "HAI titres",
       x = "Strain",
       y = "Geometric Titre Ratio") +
  theme_minimal()

p_boost_titres <- p_gtr_hai/p_gtr_neut + plot_layout(guides = "collect")

ggsave("results/covariate_level/boost_titre_fits.pdf",
       p_boost_titres,
       width = 8,
       height = 8)

ggsave("results/covariate_level/boost_titre_fits.png",
       p_boost_titres,
       width = 8,
       height = 8,
       bg = "white")

p_gtr_cluster <- ggplot() +
  geom_pointrange(data = dt_gtr_cluster_sum,
                  aes(x = Cluster_t_28,
                      y = gtr_me,
                      ymax = gtr_lo,
                      ymin = gtr_hi,
                      colour = Cluster_t_28,
                      shape = Type)) +
  geom_point(data = dt_cluster_data_all,
             aes(x = Cluster,
                 y = gtr_data,
                 colour = Cluster,
                 shape = Type)) +
  labs(title = "Cytokine clusters",
       x = "Cytokine cluster",
       y = "Geometric Titre Ratio") +
  guides(colour = "none") +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal()

ggsave("results/covariate_level/boost_cluster_fits.pdf",
       p_clusters,
       width = 8,
       height = 8)

ggsave("results/covariate_level/boost_cluster_fits.png",
       p_clusters,
       width = 8,
       height = 8,
       bg = "white")
