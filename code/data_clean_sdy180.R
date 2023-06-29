# Load some libraries

library(dm)
library(here)
library(janitor)
library(DiagrammeR)
library(tidyverse)
library(readxl)


study_name <- "SDY180"


# load data
df_hai <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "hai_result.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
df_neut <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "neut_ab_titer_result.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
df_gene <- read.table(here::here("data", "datasets", study_name, "ResultFiles", paste0(study_name, "-DR47_Subject_2_Gene_expression_result.txt")), sep = "\t", header = TRUE) %>% clean_names
df_bead <- read.table(here::here("data", "datasets", study_name, "ResultFiles", paste0(study_name, "-DR47_Subject_2_Illumina_BeadArray.txt")), sep = "\t", header = TRUE) %>% clean_names
df_sub <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "biosample.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
df_meta <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "subject.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names

# Add gene details and rsn_sequecing details

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

# Create a data model
dm_full <- dm(df_sero, df_gene, df_bead, df_sub, df_meta)

pkey_sero <- dm_enum_pk_candidates(dm = dm_full, table = df_sero) %>% as.data.frame %>% .[, 1] %>% .[[1]]
pkey_sub <- dm_enum_pk_candidates(dm = dm_full, table = df_sub) %>% as.data.frame %>% .[, 1] %>% .[[1]]
pkey_gene <- dm_enum_pk_candidates(dm = dm_full, table = df_gene) %>% as.data.frame %>% .[, 1] %>% .[[1]]
pkey_bead <- dm_enum_pk_candidates(dm = dm_full, table = df_bead) %>% as.data.frame %>% .[, 1] %>% .[[1]]
pkey_meta <- dm_enum_pk_candidates(dm = dm_full, table = df_meta) %>% as.data.frame %>% .[, 1] %>% .[[1]]


# Add primary keys to each table
dm_full_only_pks <-
    dm_full %>%
    dm_add_pk(df_sero, !!pkey_sero) %>%
    dm_add_pk(df_sub, !!pkey_sub) %>%
    dm_add_pk(df_gene, !!pkey_gene) %>%
    dm_add_pk(df_bead, !!pkey_bead) %>% 
    dm_add_pk(df_meta, !!pkey_meta)

dm_full_only_pks %>% dm_draw

### CHECK FOR FK CANDIDATES
#dm_enum_pk_candidates(
  #  dm = dm_full,
  #  table = df_meta
#) 


#dm_enum_fk_candidates(
#  dm = dm_full_only_pks,
#  table = df_sero,
#  ref_table = df_meta
#)

dm_enum_fk_candidates(dm = dm_full_only_pks, table = df_sero, ref_table = df_sub)
dm_enum_fk_candidates(dm = dm_full_only_pks, table = df_sero, ref_table = df_meta)
dm_enum_fk_candidates(dm = dm_full_only_pks, table = df_sero, ref_table = df_meta)

# Add foreign keys to each table
dm_full_ona_all_pks <-
    dm_full_only_pks %>%
    # link sero 
    dm_add_fk(table = df_sero, columns = biosample_accession, ref_table = df_sub) %>%
    dm_add_fk(table = df_gene, columns = biosample_accession, ref_table = df_sub) %>%
    dm_add_fk(table = df_bead, columns = biosample_accession, ref_table = df_meta) %>%
    dm_add_fk(table = df_sero, columns = subject_accession, ref_table = df_meta) %>%
    dm_add_fk(table = df_gene, columns = subject_accession, ref_table = df_meta) %>%
    dm_add_fk(table = df_bead, columns = subject_accession, ref_table = df_meta)

# Code to clean the flattened tables
generate_long_table <- function(sero_edit) {    
    sero_edit <- dm_full_ona_all_pks %>%
    dm_flatten_to_tbl(df_sero)
    duplicated_columns <- duplicated(as.list(sero_edit))

    #colnames(sero_edit[duplicated_columns])
    sero_edit_A <- sero_edit[!duplicated_columns]
    sero_edit_A <- sero_edit_A %>% select(-study_time_collected.df_sub)
    sero_edit_A %>% rename_with(~str_remove(., '\\..*$'))
}

# Generate long tables of all samples
sero_edit <- dm_full_ona_all_pks %>%
    dm_flatten_to_tbl(df_sero) %>% 
    generate_long_table

gene_edit <- dm_full_ona_all_pks %>%
    dm_flatten_to_tbl(df_gene) %>% 
    generate_long_table

# save
write.csv(sero_edit, here::here("data", "model_data", study_name, "sero_edit.csv"))
write.csv(gene_edit, here::here("data", "model_data", study_name, "gene_edit.csv"))


#sero_edit %>% mutate(value_preferred_trans = log2(value_preferred/ 5) ) %>%
 #   ggplot() + 
  #      geom_count(aes(x = study_time_collected, y = value_preferred_trans, color = virus_strain_preferred), 
   #         position = position_dodge(0.5))