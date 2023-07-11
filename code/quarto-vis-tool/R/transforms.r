##' Calculate dimension reduction 
##'
##' Add a projection of the samples onto PCA space
##' @title Store dimension-reduction results in SummarizedExperiment object
##' @param es The SummarizedExperiment data object
##' @param n 
##' @return 
##' @author Gavin Kelly
##' @export
add_dim_reduct  <-  function(se, assay=assayNames(se)[1], scale=FALSE, center=TRUE) {
  var_stab <- assay(se, assay)
  pc <- prcomp(t(var_stab), scale=scale, center=center)
  percentVar <- round(100 * pc$sdev^2 / sum( pc$sdev^2 ))
  pca_name <- paste0(assay, ".PCA")
  colData(se)[[pca_name]] <- DataFrame(pc$x)
  mcols(colData(se))[pca_name, "PCA"] <- TRUE
  metadata(colData(se)[[pca_name]])$percentVar <- setNames(percentVar, colnames(pc$x))
  mcols(se)[[pca_name]] <-DataFrame(pc$rotation)
  mcols(mcols(se))[pca_name, "PCA"] <- TRUE
  se
}


impute_se <- function(se, method="to-zero", assay=1) {
  if ("SummarizedExperiment" %in% class(se)) {
    mat <- assay(se, assay)
  } else {
    mat <- se
  }
  if (method=="to-zero") {
    mat <- assay(se, assay)
    mat[is.na(mat)] <- 0
  }
  if (method=="lod") {
    mat[is.na(mat)] <- min(mat, na.rm=TRUE)
  }
  if (method=="lod-feature") {
    imp <- apply(mat, 1, min, na.rm=TRUE)
    mat[is.na(mat)] <- imp[row(mat)[is.na(mat)]]
  }
  if (method=="third-of-min") {
    mat[is.na(mat)] <- min(mat, na.rm=TRUE)/3
  }
  if (method=="third-of-min-feature") {
    imp <- apply(mat, 1, min, na.rm=TRUE)/3
    mat[is.na(mat)] <- imp[row(mat)[is.na(mat)]]
  }
  if (method=="mean") {
    imp <- apply(mat, 1, mean, na.rm=TRUE)
    mat[is.na(mat)] <- imp[row(mat)[is.na(mat)]]
  }
  if (method=="binary") {
    mat <- is.na(mat)
    mat[] <- as.numeric(mat)
  }
  mat
}

cts_as_binary <- function(threshold) {
  function(mat) {
    dist(mat>threshold, method="binary")
  }
}

dist_fns <- list(
  mahalanobis2 = function(m) {
    dec <- chol( cov(m) )
    tmp <- forwardsolve(t(dec), t(m) )
    dist(t(tmp))
  },
  mahalanobis = function(m) {
    dist(svd(scale(m, center = TRUE, scale = FALSE), nv = 0)$u)
  }
)

normalise_assay <- function(se, assay_to_norm, model) {
  df <- as.data.frame(colData(se))
  df$y <- I(t(assay(se, assay_to_norm)))
  fit <- lm(update(model, y~.), data=df)
  baseline <- t(predict(fit,
                       newdata=df
                       )
               )
  assay(se, assay_to_norm) - baseline
}


log0_third <- function(x) {
  xmin <- min(x[x!=0], na.rm=TRUE)
  x[x==0] <- xmin/3
  log(x)
}
