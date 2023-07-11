calculate_post_preds <- function(dt_in,
                                 time_range,
                                 ind_flag,
                                 n_samples) {

  if(ind_flag == TRUE) {

    dt_sim <- simulate_trajectories(time_range,
                                    dt_in,
                                    n_samples,
                                    ind_flag = TRUE,
                                    by_args = NULL)

    dt_post_pred <- dt_sim[, .(y_me = quantile(exp_titre, 0.5),
                                   y_lo = quantile(exp_titre, 0.025),
                                   y_hi = quantile(exp_titre, 0.975)),
                               by = c("t", "id")]

  } else if(ind_flag == FALSE) {

    dt_sim <- simulate_trajectories(time_range,
                                    dt_in,
                                    n_samples,
                                    ind_flag = FALSE,
                                    by_args = NULL)

    dt_post_pred <- dt_sim[, .(y_me = quantile(exp_titre, 0.5),
                                   y_lo = quantile(exp_titre, 0.025),
                                   y_hi = quantile(exp_titre, 0.975)),
                               by = "t"]
  }

  return(dt_post_pred)

}
