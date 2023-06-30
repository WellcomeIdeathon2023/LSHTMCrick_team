library(dm)
library(here)
library(janitor)
library(DiagrammeR)
library(tidyverse)
library(readxl)


study_name <- "SDY180"

# load data
clean_SDY180 <-function() {
    study_name <- "SDY180"

    df_hai <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "hai_result.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
    df_neut <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "neut_ab_titer_result.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
    df_meta <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "subject.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names

    # combine the two hai and the neuts
    df_neut_2 <- df_neut %>% mutate(value_preferred =
        case_when(
            value_preferred == "<0.3" ~ "0",
            value_preferred != "<0.3 "~ as.character(value_preferred)) ) %>%
        mutate(value_preferred = as.numeric(value_preferred))
    df_neut_3 <- df_neut_2 %>% mutate(value_reported =
        case_when(
            value_reported == "<0.3" ~ "0",
            value_reported != "<0.3 "~ as.character(value_reported)) ) %>%
        mutate(value_reported = as.numeric(value_reported))

    df_sero <- bind_rows(df_hai, df_neut_3)

    df_sero_clean <- df_sero %>% mutate(virus_strain_plot = case_when(virus_strain_preferred == ""~virus_strain_reported, TRUE~virus_strain_preferred)) %>% 
        select(result_id, subject_accession, time = study_time_collected, strain = virus_strain_plot, value = value_preferred)

    df_meta_clean <- df_meta %>% select(!strain)
    list(df_sero_clean, df_meta_clean)
}

# load data
clean_SDY296_301 <-function(study_name) {

    study_name <- "SDY296"
    df_hai <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "hai_result.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
    df_neut <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "neut_ab_titer_result.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
    df_meta <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "subject.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names

    # combine the two hai and the neuts
    df_sero <- bind_rows(df_hai, df_neut)

    df_sero_clean <- df_sero %>% mutate(virus_strain_plot = case_when(virus_strain_preferred == ""~virus_strain_reported, TRUE~virus_strain_preferred)) %>% 
        select(result_id, subject_accession, time = study_time_collected, strain = virus_strain_plot, value = value_preferred)

    df_meta_clean <- df_meta %>% select(!strain)
    list(df_sero_clean, df_meta_clean)
}

link_sero_meta <- function(df_sero, df_meta) {
    df_sero %>% left_join(df_meta)
}


cleaned_sero_SDY180 <- clean_SDY180()

df_sero_clean_SDY180 <- cleaned_sero_SDY180[[1]]
df_meta_clean_SDY180 <- cleaned_sero_SDY180[[2]]

df_linked_sero_SDY180 <- link_sero_meta(df_sero_clean_SDY180, df_meta_clean_SDY180)
write.csv(df_linked_sero_SDY180, here::here("data", "model_data", "SDY180", "sero_edit.csv"))


cleaned_sero_SDY296 <- clean_SDY296_301("SDY296")
cleaned_sero_SDY301 <- clean_SDY296_301("SDY301")

df_sero_clean_SDY296 <- cleaned_sero_SDY296[[1]]
df_meta_clean_SDY296 <- cleaned_sero_SDY296[[2]]

df_linked_sero_SDY296 <- link_sero_meta(df_sero_clean_SDY296, df_meta_clean_SDY296)
write.csv(df_linked_sero_SDY296, here::here("data", "model_data", "SDY296", "sero_edit.csv"))


df_sero_clean_SDY301 <- cleaned_sero_SDY301[[1]]
df_meta_clean_SDY301 <- cleaned_sero_SDY301[[2]]

df_linked_sero_SDY301 <- link_sero_meta(df_sero_clean_SDY301, df_meta_clean_SDY301)
write.csv(df_linked_sero_SDY301, here::here("data", "model_data", "SDY301", "sero_edit.csv"))