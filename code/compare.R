
# load data
study_name <- "SDY180"
df_hai_1 <- read_excel(here::here("data", "datasets", study_name, "ResultFiles", "HAI_result", "Serology_by_Cohort.423738.xlsx")) %>% clean_names
df_sub_1 <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "biosample.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names

study_name <- "SDY296"
df_hai_2 <- read.csv(here::here("data", "datasets", study_name, "ResultFiles", "hai_result.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
df_neut_2 <- read.csv(here::here("data", "datasets", study_name, "ResultFiles", "neut_ab_titer_result.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
df_sub_2 <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "biosample.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names

# Add gene details and rsn_sequecing details
#df_gene_deet <- read.table(here::here("data", "datasets", study_name, "ResultFiles", "Gene_expression_result", "Nanostring_norm_data_DS10_ESIDs_SDY296.587721.txt"), sep = "\t", header = TRUE) %>% clean_names

# combine the two hai and the neuts
df_sero_1 <- df_hai_1
dm_full_1 <- dm(df_sero_1, df_sub_1)

df_sero_2 <- bind_rows(df_hai_2, df_neut_2)
dm_full_2 <- dm(df_sero_2, df_sub_2)



pkey_sero_1 <- dm_enum_pk_candidates(dm = dm_full_1, table = df_sero_1) %>% as.data.frame %>% .[, 1] %>% .[[1]]
pkey_sub_1 <- dm_enum_pk_candidates(dm = dm_full_1, table = df_sub_1) %>% as.data.frame %>% .[, 1] %>% .[[1]]

pkey_sero_2 <- dm_enum_pk_candidates(dm = dm_full_2, table = df_sero_2) %>% as.data.frame %>% .[, 1] %>% .[[1]]
pkey_sub_2 <- dm_enum_pk_candidates(dm = dm_full_2, table = df_sub_2) %>% as.data.frame %>% .[, 1] %>% .[[1]]


# Add primary keys to each table
dm_full_only_pks_1 <-
    dm_full_1 %>%
    dm_add_pk(df_sero_1, !!pkey_sero_1) %>%
    dm_add_pk(df_sub_1, !!pkey_sub_1)

# Add primary keys to each table
dm_full_only_pks_2 <-
    dm_full_2 %>%
    dm_add_pk(df_sero_2, !!pkey_sero_2) %>%
    dm_add_pk(df_sub_2, !!pkey_sub_2)



dm_enum_fk_candidates(dm = dm_full_only_pks_1, table = df_sero_1, ref_table = df_sub_1)
dm_enum_fk_candidates(dm = dm_full_only_pks_2, table = df_sero_2, ref_table = df_sub_2)
