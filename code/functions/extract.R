extract_posterior_draws <- function(fit,
                                    structure_arg,
                                    ind_flag = FALSE) {

  if(ind_flag == FALSE) {
    if(structure_arg == "wide") {
      # gathering posterior draws into a data.table using tidybayes package
      dt_posterior <- data.table(
        spread_draws(fit,
                     t_e,
                     grad_boost,
                     sigma_t_e,
                     sigma_grad_boost,
                     sigma))[,
  type := "posterior"]
    }

    if(structure_arg == "long") {
      dt_posterior <- data.table(
        gather_draws(fit,
                     t_e,
                     grad_boost,
                     sigma_t_e,
                     sigma_grad_boost,
                     sigma))[,
  type := "posterior"]

      # changing the names of the columns in the data.table
      setnames(dt_posterior,
               c(".variable", ".value"),
               c("parameter", "value"))

    }
  } else if(ind_flag == TRUE) {
    if(structure_arg == "wide") {
      # gathering posterior draws into a data.table using tidybayes package
      # gathering posterior draws into a data.table using tidybayes package
      dt_posterior <- data.table(
        spread_draws(fit,
                     t_e_ind[i],
                     grad_boost_ind[i]))[,
                             type := "posterior"]
      setnames(dt_posterior,
               c("i"),
               c("id"))
    }

    if(structure_arg == "long") {
      dt_posterior <- data.table(
        gather_draws(fit,
                     t_e_ind[i],
                     grad_boost_ind[i]))[,
                                     type := "posterior"]

      # changing the names of the columns in the data.table
      setnames(dt_posterior,
               c(".variable", ".value", "i"),
               c("parameter", "value", "id"))

    }
  }
  return(dt_posterior)
}
