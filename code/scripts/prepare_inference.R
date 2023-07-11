# CONCENTRATING ON THE SDY180 STUDY FOR NOW
dt_sdy180_sero <- fread("data/model_data/SDY180/sero_edit.csv")

#--- PREPARING DATA FOR GAVINS CYTOKINE CLUSTERS
cytokine_clusters <- fread("data/model_data/SDY180/cytokine_clusters.csv")
setnames(cytokine_clusters, "SUBJECT_ACCESSION", "subject_accession")

dt_sdy180_sero_cyto <- merge(dt_sdy180_sero, cytokine_clusters, by = "subject_accession")


#--- PREPARING INFRERENCE FOR HAI TITRES
dt_stan_hai_flu_A_10 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                          assay_type = "HAI",
                                          virus_strain = "A/Brisbane/10/2007",
                                          cluster = c(1, 2, 3, 4))

dt_stan_hai_flu_A_59 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                          assay_type = "HAI",
                                          virus_strain = "A/Brisbane/59/2007",
                                          cluster = c(1, 2, 3, 4))

dt_stan_hai_flu_B_60 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                          assay_type = "HAI",
                                          virus_strain = "B/Brisbane/60/2008",
                                          cluster = c(1, 2, 3, 4))

dt_stan_hai_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_stan_hai_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_stan_hai_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_stan_hai_flu_all <- rbind(dt_stan_hai_flu_A_10,
                             dt_stan_hai_flu_A_59,
                             dt_stan_hai_flu_B_60)

stan_data_hai_flu_A_10 <- get_stan_data(dt_stan_hai_flu_A_10)
stan_data_hai_flu_A_59 <- get_stan_data(dt_stan_hai_flu_A_59)
stan_data_hai_flu_B_60 <- get_stan_data(dt_stan_hai_flu_B_60)

#--- PREPARING INFRERENCE FOR NEUTRALISING ANTIBODY TITRES
dt_stan_neut_flu_A_10 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                           assay_type = "Neut",
                                           virus_strain = "A/Brisbane/10/2007",
                                           cluster = c(1, 2, 3, 4))

dt_stan_neut_flu_A_59 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                           assay_type = "Neut",
                                           virus_strain = "A/Brisbane/59/2007",
                                           cluster = c(1, 2, 3, 4))

dt_stan_neut_flu_B_60 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                           assay_type = "Neut",
                                           virus_strain = "B/Brisbane/60/2008",
                                           cluster = c(1, 2, 3, 4))

dt_stan_neut_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_stan_neut_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_stan_neut_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_stan_neut_flu_all <- rbind(dt_stan_neut_flu_A_10,
                              dt_stan_neut_flu_A_59,
                              dt_stan_neut_flu_B_60)

stan_data_neut_flu_A_10 <- get_stan_data(dt_stan_neut_flu_A_10)
stan_data_neut_flu_A_59 <- get_stan_data(dt_stan_neut_flu_A_59)
stan_data_neut_flu_B_60 <- get_stan_data(dt_stan_neut_flu_B_60)

#--- PREPARING INFERENCE FOR CYTOKINE CLUSTERS
dt_stan_data_cluster_1 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                         assay_type = c("HAI", "Neut"),
                                         virus_strain = c("A/Brisbane/10/2007",
                                                          "A/Brisbane/59/2007",
                                                          "B/Brisbane/60/2008"),
                                         cluster = 1)[, virus_strain_preferred := NULL] |>
  unique()

dt_stan_data_cluster_2 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                         assay_type = c("HAI", "Neut"),
                                         virus_strain = c("A/Brisbane/10/2007",
                                                          "A/Brisbane/59/2007",
                                                          "B/Brisbane/60/2008"),
                                         cluster = 2)[, virus_strain_preferred := NULL] |>
  unique()

dt_stan_data_cluster_3 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                         assay_type = c("HAI", "Neut"),
                                         virus_strain = c("A/Brisbane/10/2007",
                                                          "A/Brisbane/59/2007",
                                                          "B/Brisbane/60/2008"),
                                         cluster = 3)[, virus_strain_preferred := NULL] |>
  unique()

dt_stan_data_cluster_4 <- prepare_stan_data(dt_sdy180_sero_cyto,
                                         assay_type = c("HAI", "Neut"),
                                         virus_strain = c("A/Brisbane/10/2007",
                                                          "A/Brisbane/59/2007",
                                                          "B/Brisbane/60/2008"),
                                         cluster = 4)[, virus_strain_preferred := NULL] |>
  unique()

stan_data_cluster_1 <- get_stan_data(dt_stan_data_cluster_1)
stan_data_cluster_2 <- get_stan_data(dt_stan_data_cluster_2)
stan_data_cluster_3 <- get_stan_data(dt_stan_data_cluster_3)
stan_data_cluster_4 <- get_stan_data(dt_stan_data_cluster_4)


