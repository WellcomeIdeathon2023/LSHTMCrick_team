prepare_stan_data <- function(dt_in,
                              assay_type,
                              virus_strain,
                              cluster) {

  dt_stan_trim <- dt_in[assay %in% assay_type &
                        virus_strain_preferred %in% virus_strain &
                        cytokine_cluster %in% cluster]

  if(dt_stan_trim[, uniqueN(experiment_accession)] > 1) {

    dt_stan_proc <- dt_stan_trim[, .(virus_strain_preferred = virus_strain_preferred,
                                     value_preferred = mean(value_preferred),
                                     cytokine_cluster = cytokine_cluster),
                                  by = c("study_time_collected", "subject_accession")]

    dt_stan_proc <- unique(dt_stan_proc)

  } else {

    dt_stan_proc <- dt_stan_trim[, .(virus_strain_preferred = virus_strain_preferred,
                                     study_time_collected = study_time_collected,
                                     cytokine_cluster = cytokine_cluster,
                                     value_preferred = value_preferred),
                                 by = "subject_accession"]

    dt_stan_proc <- unique(dt_stan_proc)
  }
#
  # dt_stan_trim[subject_accession == "SUB119292", mean(value_preferred),
  #              by = c("study_time_collected")]

  dt_stan <- dt_stan_proc[
    !(virus_strain_preferred %like% "pneumoniae")
  ][, titre := log2(value_preferred/5)][
    , id := .GRP, by = subject_accession
  ][, time := study_time_collected][order(id, time)]

  return(dt_stan)
}

get_stan_data <- function(dt_stan) {

  stan_data <- list(
    N = dt_stan[, .N],
    N_ind = dt_stan[, uniqueN(id)],
    ids = dt_stan[, id],
    t = dt_stan[, time],
    y = dt_stan[, titre])

  return(stan_data)

}


run_stan_model <- function(stan_data) {

  mod <- cmdstanr::cmdstan_model("code/stan/boost_model.stan")

  res <- mod$sample(data = stan_data,
                    chains = 4,
                    iter_warmup = 1000,
                    iter_sampling = 2000,
                    parallel_chains = 4,
                    adapt_delta = 0.99)

  return(res)

}
