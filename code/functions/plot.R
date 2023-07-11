plot_post_preds <- function(dt_in_pop_params,
                            dt_in_data,
                            ind_flag,
                            strain_flag,
                            cluster_flag) {

  if(strain_flag == TRUE) {
    p_out <- ggplot() +
      geom_point(data = dt_in_data,
                 aes(x = time, y = titre, colour = Strain), alpha = 0.4) +
      geom_line(data = dt_in_data,
                aes(x = time, y = titre, colour = Strain, group = id), alpha = 0.1) +
      geom_ribbon(data = dt_in_pop_params, inherit.aes = FALSE,
                  aes(x = t, ymin = y_lo, ymax = y_hi, fill = Strain), alpha = 0.5) +
      geom_line(data = dt_in_pop_params, inherit.aes = FALSE,
                aes(x = t, y = y_me, colour = Strain)) +
      coord_cartesian(clip = 'off', xlim = c(-7, 28), ylim = c(0, 10)) +
      labs(x = "Time (days since vaccination)", y = "Titre value") +
      theme_minimal() +
      theme(legend.position = "none") +
      facet_wrap(~Strain)
  }

  if(cluster_flag == TRUE) {
    p_out <- ggplot() +
      geom_point(data = dt_in_data,
                 aes(x = time, y = titre, colour = Cluster), alpha = 0.2) +
      geom_line(data = dt_in_data,
                aes(x = time, y = titre, colour = Cluster, group = id), alpha = 0.1) +
      geom_ribbon(data = dt_in_pop_params, inherit.aes = FALSE,
                  aes(x = t, ymin = y_lo, ymax = y_hi, fill = Cluster), alpha = 0.5) +
      geom_line(data = dt_in_pop_params, inherit.aes = FALSE,
                aes(x = t, y = y_me, colour = Cluster)) +
      coord_cartesian(clip = 'off', xlim = c(-7, 28), ylim = c(0, 10)) +
      labs(x = "Time (days since vaccination)", y = "Titre value") +
      theme_minimal() +
      theme(legend.position = "none") +
      facet_wrap(~Cluster)
  }

  if(ind_flag == TRUE & strain_flag == TRUE) {

    p_out <- p_out +
      facet_grid(id~Strain) +
      theme(legend.position = "none")

  }

  # if(ind_flag == FALSE & cluster_flag == TRUE) {
  #
  #   p_out <- p_out +
  #     facet_wrap(~Cluster)
  #
  # }

  # if(ind_flag == TRUE & cluster_flag == TRUE) {
  #
  #   p_out <- p_out +
  #     facet_grid(id~Cluster) +
  #     theme(legend.position = "none")
  #
  # }
  return(p_out)

}
