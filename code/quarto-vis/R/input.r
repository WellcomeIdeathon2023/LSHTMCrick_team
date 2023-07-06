read_my_files <- function(params, transform) {
  msg=""
  feat_meta <- read_csv(file.path(params$`data-source`, params$input_feature_meta), show_col_types = FALSE)
  ID <- feat_meta$Feature[feat_meta$Type=="ID"]
  
  fnames <- strsplit(params$input_matrix," ")[[1]]
  mats <- lapply(fnames, function(fname) {
    mat <- data.frame(read_csv(file.path(params$`data-source`,fname), show_col_types = FALSE), check.names=FALSE)
    potential_group_vars <- subset(feat_meta, Type=="metadata", select=Feature)[[1]]
    bad_ID <- group_by(mat, across(all_of(ID))) %>%
      summarise(across(all_of(potential_group_vars), function(x) length(unique(x)))) %>%
      filter(if_any(all_of(potential_group_vars), function(x) x!=1)) %>%
      pull(all_of(ID))
    mat <- mat %>%
      filter(if_any(all_of(ID), function(x) !x %in%bad_ID))
    assays <- setdiff(feat_meta$Type, c("metadata","ID"))
    measure_vars <- subset(feat_meta, Type %in% assays, select=Feature)[[1]]
    mat <- group_by(mat, across(all_of(ID))) %>%
        summarise(across(where(is.numeric), mean),
                  across(where(is.character), dplyr::first)
                  ) %>%
      as.data.frame()
    row.names(mat) <- mat[[ID]]
    mat[[ID]] <- NULL
    mat
  })
  mat <- do.call(cbind, mats)
  
  ord <- match(colnames(mat), feat_meta$Feature)
  if (any(is.na(ord))) {
    msg <- paste0("The following features are missing annotation and have been discarded from analysis: ", paste(colnames(mat)[is.na(ord)], collapse=", "), ". ")
    mat <- mat[,!is.na(ord)]
    ord <- ord[!is.na(ord)]
  }
  feat_meta <- feat_meta[ord,]
  
  ind_samp <- eval(parse(text=params$sample_subset), mat)
  mat <- mat[ind_samp,]
  
  ind_meta <- eval(parse(text=params$metadata_subset), feat_meta)
  samp_meta <- mat[,ind_meta,drop=FALSE]
  
  ind_feat <- eval(parse(text=params$feature_subset), feat_meta)
  if (!missing(transform)) {
    tmp  <- try(transform(mat), silent=TRUE)
    if (!"try-error" %in% class(tmp)) mat <- tmp
  }
  mat <- as.matrix(mat[, ind_feat,drop=FALSE])
  feat_meta <- feat_meta[ind_feat,,drop=FALSE]
  feat_meta$Feature <- NULL
  dat <- SummarizedExperiment(
    assays=list(raw=t(mat)),
    colData=samp_meta,
    rowData=feat_meta
  )
  metadata(dat)$assay <- list(raw="Raw")
  metadata(dat)$warnings <- msg
  dat
}


read_long_data <- function(params, input_file) {
  long_dat <- vroom::vroom(input_file) %>%
    dplyr::filter(!if_any(all_of(params$subject_vars), is.na))
  wide_dat <- long_dat %>%
    tidyr::pivot_wider(
      values_from=all_of(params$value_vars),
      id_cols=all_of(params$subject_vars),
      names_from=all_of(params$names_from)
    )
  mat <- dplyr::select(wide_dat,
                       all_of(unique(long_dat[[params$names_from]]))) %>%
    as.matrix()
  row.names(mat) <-  dplyr::select(wide_dat, all_of(params$subject_vars)) %>% tidyr::unite("colname", everything()) %>% dplyr::pull(colname)
  dat <- SummarizedExperiment(
    assays=list(raw=t(mat)),
    colData=dplyr::select(wide_dat, all_of(params$subject_vars)),
    rowData=data.frame(type=rep(params$names_from, ncol(mat)))
  )
  metadata(dat)$assay <- list(raw="Raw")
  metadata(dat)$warnings <- ""
  dat

  }

read_extra <- function(dat, params, here) {
  df <- as.data.frame(colData(dat))
  ind <- row.names(df)
  for (extra in params$files) {
    add_df <- vroom::vroom(file.path(here, extra$file)) %>%
        select(all_of(c(extra$key, extra$retain)))
    df <- left_join(add_df, df, by=extra$key)
  }
  colData(dat) <- df[ind,]
  dat
}
    
    
