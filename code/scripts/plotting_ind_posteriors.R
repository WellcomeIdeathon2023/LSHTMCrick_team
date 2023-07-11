# for this script to work, either read in saved fits or run the inference script
# reading in data file is preferred, so it is set to the default option

# source("code/scripts/inference.R")
source("code/scripts/calculate_pop_post_preds.R")
source("code/scripts/calculate_ind_post_preds.R")

# PLOTTING HAI TITRE FITS
dt_ind_post_sum_long_flu_A_10[, Strain := "A/Brisbane/10/2007"]
dt_ind_post_sum_long_flu_A_59[, Strain := "A/Brisbane/59/2007"]
dt_ind_post_sum_long_flu_B_60[, Strain := "B/Brisbane/60/2008"]

dt_ind_post_hai_all <- rbind(dt_ind_post_sum_long_flu_A_10,
                             dt_ind_post_sum_long_flu_A_59,
                             dt_ind_post_sum_long_flu_B_60)

#--- SORTING OUT IDS
dt_id_lookup <- dt_stan_hai_flu_all[, .(subject_accession, id, Strain)] |>
  unique()

dt_ind_post_hai_all_new_id <- merge(dt_id_lookup,
                                    dt_ind_post_hai_all,
                                    by = c("id", "Strain"))

dt_ind_post_hai_all_new_id[order(subject_accession, Strain),
                           id := .GRP, by = id]

dt_stan_hai_flu_all_new_id <- dt_stan_hai_flu_all[
  order(subject_accession, Strain), id := .GRP, by = id]


dt_ind_post_hai_all <- rbind(dt_ind_post_sum_long_flu_A_10,
                             dt_ind_post_sum_long_flu_A_59,
                             dt_ind_post_sum_long_flu_B_60)

#--- SORTING OUT IDS
dt_id_lookup <- dt_stan_hai_flu_all[, .(subject_accession, id, Strain)] |>
  unique()

dt_ind_post_hai_all_new_id <- merge(dt_id_lookup,
                                    dt_ind_post_hai_all,
                                    by = c("id", "Strain"))

dt_ind_post_hai_all_new_id[order(subject_accession, Strain),
                           id := .GRP, by = id]

dt_stan_hai_flu_all_new_id <- dt_stan_hai_flu_all[
  order(subject_accession, Strain), id := .GRP, by = id]

# PLOTTING INDIVIDUAL-LEVEL FITS
p_ind_post_preds <- plot_post_preds(dt_ind_post_hai_all_new_id,
                                    dt_stan_hai_flu_all_new_id,
                                    ind_flag = TRUE,
                                    strain_flag = TRUE,
                                    cluster_flag = FALSE)

ggsave("results/individual_level/boost_titre_fits.pdf",
       p_ind_post_preds,
       width = 8,
       height = 12)

ggsave("results/individual_level/boost_titre_fits.png",
       p_ind_post_preds,
       width = 8,
       height = 12,
       bg = "white")
