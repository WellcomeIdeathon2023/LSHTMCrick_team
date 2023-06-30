
get_gene_info <- function(study_name, file_number) {

    df_bio <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "biosample.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names
    df_gene <- read.table(here::here("data", "datasets", study_name, "ResultFiles", "Gene_expression_result", paste0("Nanostring_norm_data_DS10_ESIDs_", study_name, ".", file_number, ".txt")), sep = "\t", header = TRUE) %>% clean_names %>% 
        rename(expsample_accession = exp_sample_acc_num)
    df_bio_link <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "expsample_2_biosample.csv"), colClasses = c( "NULL", rep(NA, 2))) %>% clean_names 
    df_bio <- read.csv(here::here("data", "datasets", study_name, paste0(study_name, "-DR47_Tab"), "biosample.csv"), colClasses = c("NULL", rep(NA, 10))) %>% clean_names

    df_complete_gene <- df_gene %>% left_join(df_bio_link) %>% left_join(df_bio)
    write.csv(df_complete_gene, here::here("data", "model_data", study_name, "gene_edit.csv"))

}

get_gene_info("SDY180", "587719")
get_gene_info("SDY296", "587721")
get_gene_info("SDY301", "587720")
