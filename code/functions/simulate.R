# plotting the fitted curves
boost_fun <- function(t, t_e, grad_boost) {

  mu = t_e + grad_boost*t;
  return(mu)
}

# function that simulates the boost and wane trajectories over time, give
# a data.table of posterior samples
simulate_trajectories <- function(time_range,
                                  dt_posterior,
                                  n_samples,
                                  ind_flag,
                                  by_args) {

  times <- data.table::data.table(
    t = time_range
  )

  dt_samples <- dt_posterior[, .SD[.draw %in% 1:n_samples], by = by_args]

  by_args_complete = c(by_args, "t", ".draw")

  if(ind_flag == TRUE) {
    by_args_complete <- c(by_args_complete, "id")
  }

  if(ind_flag == TRUE) {
    trajectories <- merge(
      dt_samples[, tid := 1], times[, tid := 1], by = "tid",
      allow.cartesian = TRUE
    )[,
      tid := NULL][,
                   exp_titre := boost_fun(t, t_e_ind, grad_boost_ind),
                   by = by_args_complete
      ]
  } else if(ind_flag == FALSE) {
    trajectories <- merge(
      dt_samples[, tid := 1], times[, tid := 1], by = "tid",
      allow.cartesian = TRUE
    )[,
      tid := NULL][,
                   exp_titre := boost_fun(t, t_e, grad_boost),
                   by = by_args_complete
      ]
  }

  return(trajectories[])
}
