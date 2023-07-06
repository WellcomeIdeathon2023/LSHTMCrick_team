
create_plot_flu <- function(HAI, input, covar, sero_SDY180, sero_SDY296, sero_SDY301 ) {
  rawTitres <- reactive({
      if (input$study == "SDY296") {
        sero <- sero_SDY296
      } else if (input$study == "SDY301") {
        sero <- sero_SDY301
      } else if (input$study == "SDY180")
        sero <- sero_SDY180
    })

  boostTires <- reactive({
      if (input$study == "SDY296") {
        sero <- sero_SDY296
        filter(sero, assay == !!HAI) %>%
         pivot_wider(!c(value_preferred, result_id, planned_visit_accession, biosample_accession, expsample_accession, X), names_from = "study_time_collected", values_from = "value_log2") %>% as.data.frame %>% 
         mutate(boost = `28` - `0`)
      } else if (input$study == "SDY301") {
        sero <- sero_SDY301
        filter(sero, assay == !!HAI) %>%
         pivot_wider(!c(planned_visit_accession, biosample_accession), names_from = "study_time_collected", values_from = "value_log2") %>% as.data.frame %>% 
         mutate(boost = `28` - `0`)
      }
      else if (input$study == "SDY180") {
        sero <- sero_SDY180
        sero %>%
          pivot_wider(!c(biosample_accession,description, planned_visit_accession), names_from = "study_time_collected", values_from = "value_log2") %>% as.data.frame %>% 
        mutate(boost = `28` - `0`)
      }

    })


  plot_data <- reactive({
      if (input$covar == "none") {
      p1 <- ggplot(rawTitres()) + 
          geom_count(aes(x = study_time_collected, y = 5 * 2^value_log2), fill = "gray", shape = 21) + 
          geom_line(aes(x = study_time_collected, y = 5 * 2^value_log2, group = subject_accession), alpha = 0.2) + 
          stat_summary(aes(x = study_time_collected, y = 5 * 2^value_log2), 
              fun = "mean", geom = "line", color = "red", size = 2, alpha = 0.8) +
          stat_summary(aes(x = study_time_collected, y = 5 * 2^value_log2), 
              fun = "mean", geom = "point", fill = "red", size = 4, shape = 24, alpha = 0.8) +
          facet_wrap(vars(virus_strain_preferred)) + 
          theme_bw() + 
          guides(size = "none") +
          labs(x = "Time post vaccination",  y = paste0("HAI", " assay titre" ) ) +
          scale_y_continuous(trans = "log2") +
          theme(text = element_text(size = 15))

        p2 <- ggplot(boostTires()) + 
          geom_count(aes(x = 1, y = 2^boost), fill = "gray", shape = 21) + 
          stat_summary(aes(x = 1, y = 2^boost),  fun = "mean", geom = "point", fill = "red", size = 4, shape = 24, alpha = 0.8) + 
          facet_wrap(vars(virus_strain_preferred)) + 
          theme_bw() + 
          guides(size = "none") +
          labs(x = "Time post vaccination",  y = paste0("HAI", " boosting" ) ) +
          scale_y_continuous(trans = "log2") +
          theme(text = element_text(size = 15))
      } else {
          p1 <- ggplot(rawTitres()) + 
            geom_count(aes(x =study_time_collected, y = 5 * 2^value_log2, fill = .data[[input$covar]]), shape = 21, alpha = 0.3, position = position_dodge(0.5)) + 
            geom_line(aes(x = study_time_collected, y = 5 * 2^value_log2, color = .data[[input$covar]], group = subject_accession), alpha = 0.2, position = position_dodge(0.5)) + 
            stat_summary(aes(x = study_time_collected, y = 5 * 2^value_log2, color = .data[[input$covar]]), 
                fun = "mean", geom = "line",  size = 2, alpha = 0.8, position = position_dodge(0.5)) +
            stat_summary(aes(x = study_time_collected, y = 5 * 2^value_log2, fill = .data[[input$covar]] ), 
                fun = "mean", geom = "point", size = 4, shape = 24, alpha = 0.8, position = position_dodge(0.5)) +
            facet_wrap(vars(virus_strain_preferred)) + 
            theme_bw() + 
            guides(size = "none") +
            labs(x = "Time post vaccination",  y = paste0("HAI", " assay titre" ) ) +
            scale_y_continuous(trans = "log2") +
            theme(text = element_text(size = 15))

        p2 <- ggplot(boostTires()) + 
            geom_count(aes(x = .data[[input$covar]], y = 2^boost, fill = .data[[input$covar]]), alpha = 0.2, color = "black", shape = 21, position = position_dodge(0.5)) + 
            stat_summary(aes(x = .data[[input$covar]], y = 2^boost, fill = .data[[input$covar]]),  color = "black", fun = "mean", geom = "point", size = 4, shape = 24, alpha = 0.8, position = position_dodge(0.5)) + 
            facet_wrap(vars(virus_strain_preferred)) + 
            theme_bw() + 
            guides(size = "none") +
            labs(x = "Time post vaccination",  y = paste0("HAI", " boosting" ) ) +
            scale_y_continuous(trans = "log2") +
            theme(text = element_text(size = 15))
      }
      p1 / p2
  })

  plot1 <- renderPlot({
    par(mar = c(5.1, 1.1, 0, 1))
    plot_data()
  }, height = 1000, width = 600 )
}

vars <- c("none", "ethnicity", "gender", "race")
study <- c("SDY180", "SDY296", "SDY301")



create_plot_pneumo <- function(input, covar, sero_SDY180_p ) {
  rawTitres <- reactive({
      sero <- sero_SDY180_p
      
    })

  boostTitres <- reactive({
        sero <- sero_SDY180_p
        sero %>%
         pivot_wider(!c(X, result_id, planned_visit_accession, biosample_accession, expsample_accession, value_reported), names_from = "study_time_collected", values_from = "value_preferred") %>% as.data.frame %>% 
         mutate(boost = `28` - `0`)
    }
  )


  plot_data <- reactive({
      if (input$covar2 == "none") {
      p1 <- ggplot(rawTitres()) + 
          geom_count(aes(x = study_time_collected, y = value_preferred), fill = "gray", shape = 21) + 
          geom_line(aes(x = study_time_collected, y = value_preferred, group = subject_accession), alpha = 0.2) + 
          stat_summary(aes(x = study_time_collected, y = value_preferred), 
              fun = "mean", geom = "line", color = "red", size = 2, alpha = 0.8) +
          stat_summary(aes(x = study_time_collected, y = value_preferred), 
              fun = "mean", geom = "point", fill = "red", size = 4, shape = 24, alpha = 0.8) +
          facet_wrap(vars(virus_strain_reported)) + 
          theme_bw() + 
          guides(size = "none") +
          labs(x = "Time post vaccination",  y = paste0("Pneumo", " assay titre" ) ) +
          scale_y_continuous(trans = "log2") +
          theme(text = element_text(size = 15))

        p2 <- ggplot(boostTitres()) + 
          geom_count(aes(x = 1, y = boost), fill = "gray", shape = 21) + 
          stat_summary(aes(x = 1, y = boost),  fun = "mean", geom = "point", fill = "red", size = 4, shape = 24, alpha = 0.8) + 
          facet_wrap(vars(virus_strain_reported)) + 
          theme_bw() + 
          guides(size = "none") +
          labs(x = "Time post vaccination",  y = paste0("Pneumo", " boosting" ) ) +
          scale_y_continuous(trans = "log2") +
          theme(text = element_text(size = 15))
      } else {
          p1 <- ggplot(rawTitres()) + 
            geom_count(aes(x = study_time_collected, y = value_preferred, fill = .data[[input$covar2]]), shape = 21, alpha = 0.3, position = position_dodge(0.5)) + 
            geom_line(aes(x = study_time_collected, y = value_preferred, color = .data[[input$covar2]], group = subject_accession), alpha = 0.2, position = position_dodge(0.5)) + 
            stat_summary(aes(x = study_time_collected, y = value_preferred, color = .data[[input$covar2]]), 
                fun = "mean", geom = "line",  size = 2, alpha = 0.8, position = position_dodge(0.5)) +
            stat_summary(aes(x = study_time_collected, y = value_preferred, fill = .data[[input$covar2]] ), 
                fun = "mean", geom = "point", size = 4, shape = 24, alpha = 0.8, position = position_dodge(0.5)) +
            facet_wrap(vars(virus_strain_reported)) + 
            theme_bw() + 
            guides(size = "none") +
            labs(x = "Time post vaccination",  y = paste0("Pneumo", " assay titre" ) ) +
            scale_y_continuous(trans = "log2") +
            theme(text = element_text(size = 15))

        p2 <- ggplot(boostTitres()) + 
            geom_count(aes(x = .data[[input$covar2]], y = boost, fill = .data[[input$covar2]]), alpha = 0.2, color = "black", shape = 21, position = position_dodge(0.5)) + 
            stat_summary(aes(x = .data[[input$covar2]], y = boost, fill = .data[[input$covar2]]),  color = "black", fun = "mean", geom = "point", size = 4, shape = 24, alpha = 0.8, position = position_dodge(0.5)) + 
            facet_wrap(vars(virus_strain_reported)) + 
            theme_bw() + 
            guides(size = "none") +
            labs(x = "Time post vaccination",  y = paste0("Pneumo", " boosting" ) ) +
            scale_y_continuous(trans = "log2") +
            theme(text = element_text(size = 15))
      }
      p1 / p2
  })

  plot1 <- renderPlot({
    par(mar = c(5.1, 10.1, 10, 1))
    plot_data()
  }, height = 1000, width = 600 )
}