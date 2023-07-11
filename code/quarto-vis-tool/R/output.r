
write_assay <- function(se, assay=1, path="results", prefix="", with_pca=FALSE) {
  out <- list()
  x <- assay(se, assay)
  if (with_pca) {
    mcol_ind <- TRUE
    coldat_ind <- TRUE
  } else {
    mcol_ind <- !mcols(mcols(se))$PCA
    coldat_ind <- !mcols(colData(se))$PCA
  }
  content_frame <- cbind(mcols(se)[,mcol_ind, drop=FALSE],x)
  head_frame <- as.data.frame(colData(se)[,coldat_ind, drop=FALSE])
  head_frame[] <- sapply(head_frame, as.character)
  spacer_frame <- as.data.frame(mcols(se)[,mcol_ind, drop=FALSE])[rep(1, ncol(head_frame)),,drop=FALSE]
  spacer_frame[] <- ""
  row.names(spacer_frame) <- colnames(head_frame)
  fname <- file.path(path, paste0(prefix, "_", assay, ".csv"))
  write.csv(cbind(spacer_frame, t(head_frame)), file=fname, quote=TRUE)
  write.table(content_frame, file=fname,
              sep=",", col.names=FALSE,
              quote=TRUE, append=TRUE)
  fname
}

