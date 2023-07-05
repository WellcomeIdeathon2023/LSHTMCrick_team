sym_colour <- function(dat, lo="blue",zero="white", hi="red", clip=0.01) {
  signs <- sign(range(dat))
  zero_crossing <- prod(signs)
  mx <- quantile(dat, c(clip, 1-clip))
  if (zero_crossing==0) { # zero is a boundary
    circlize::colorRamp2(signs * mx, colors=c(lo, zero, hi)[1:2 + (signs[1]==0)])
  } else {
    if (zero_crossing == -1) { # zero included
      circlize::colorRamp2(c(-max(abs(mx)), 0, max(abs(mx))), colors=c(lo, zero, hi))
    }  else { # zero excluded
      circlize::colorRamp2(c(mx[1], mx[2]), colors=c(lo, hi))
    }
  }
}


##' Generate visualisations of raw data
##'
##' Plot heatmaps of the raw data, along with plots of the samples
##' projected onto various slices of the PCA space.
##' @title Data Visualisation
##' @param se The SummarizedExperiment object
##' @param pc_x The default choice for which principal component to
##'   plot on the x axis
##' @param pc_y The default choice for which principal component to
##'   plot on the y axis
##' @param title Data Visualisation
##' @param param A parameter-set
##' @param caption The function to use to create captions
##' @return A list (invisible) of ggplot2 objects
##' @author Gavin Kelly
#' @export
#'

se_heatmap <- function(se, assay=assayNames(se)[1], 
               column_split=NULL,
               model=~1,
               scale_to=TRUE,
               only_scale_colour=FALSE,
               row_title=assay,
               column_title="Samples",
               name=assay,
               show_row_names=FALSE,
               show_column_names=TRUE,
               max_legend_levels=Inf,
               colour_clip=0.01,
               clustering_distance_rows="euclidean",
               ...
               ) {
  colDat <- as.data.frame(colData(se))
  names(colDat) <- names(colData(se))
  e <- substitute(scale_to)
  r <- eval(e, colDat, parent.frame())
  plotDat <- assay(se, assay)
  if (any(r)) {
        plotDat <- plotDat-apply(plotDat[,r, drop=FALSE],1, mean, na.rm=TRUE)
  }
  vars <- get_terms(model)
  vars <- lapply(vars, function(x) intersect(x, names(colData(se))))
  colnames(plotDat) <- rownames(colDat)
  if (is_formula(column_split)) {
    column_split <- colDat[all.vars(column_split)]
  }
  if (ncol(plotDat)==1 || (any(apply(plotDat, 1, sd)==0) && clustering_distance_rows %in%c("pearson", "spearman", "kendall"))) {
    clustering_distance_rows <- "euclidean"
  }
  pl <- ComplexHeatmap::Heatmap(
    matrix=plotDat,
    col=sym_colour(plotDat, clip=colour_clip),
    name=name,
    row_title=row_title,
    column_title=column_title,
    column_split=column_split,
    heatmap_legend_param = list(direction = "horizontal" ),
    top_annotation=ComplexHeatmap::HeatmapAnnotation(
      df=colDat[vars$fixed],
      col = metadata(colData(se))$palette$Heatmap[vars$fixed],
      show_legend=FALSE#sapply(colDat[vars$fixed], function(f) length(unique(f))<=max_legend_levels)
     ),
    left_annotation=ComplexHeatmap::HeatmapAnnotation(
      df=as.data.frame(rowData(se)[!mcols(mcols(se))$PCA]),
      col = metadata(rowData(se))$palette$Heatmap,
      which="row",
      show_legend=FALSE#sapply(as.data.frame(rowData(se)), function(f) length(unique(f))<=max_legend_levels)
    ),
    show_row_names=show_row_names,
    show_column_names=show_column_names,
    clustering_distance_rows=clustering_distance_rows,
    ...)
  ComplexHeatmap::draw(pl, heatmap_legend_side="top")
  invisible(pl)
}



se_pca_pred <- function(se, assay=assayNames(se)[1], model) {
  pc <- as.matrix(colData(se)[[paste0(assay, ".PCA")]])
  colDat <- data.frame(colData(se)[,!mcols(colData(se))$PCA], check.names=FALSE)
  pc_frame <- expand.grid(sample=rownames(colDat), PC=1:ncol(pc))
  pc_frame$coord <- as.vector(pc)
  pc_frame <- cbind(pc_frame, colDat)
  fitFrame <- colDat
  yvar <- make.unique(c(colnames(fitFrame), "y", sep=""))[ncol(fitFrame)+1]
  fml <- update(model, paste(yvar, "~ ."))
  plotFrame <- expand.grid(Covariate=attr(terms(fml), "term.labels"),
                          PC=1:ncol(pc))
  plotFrame$Assoc <- NA
  npc <- ncol(pc)
  fit_selected <- list()
  for (ipc in 1:npc) {
    fitFrame[[yvar]] <- pc[,ipc]
    fit1 <-  lm(fml, data=fitFrame)
    fit_selected[[ipc]] <- MASS::stepAIC(fit1, trace=0)
    ind <- attr(terms(fit_selected[[ipc]]),"term.labels")
    if (length(ind)) {
      fit0 <- as.data.frame(anova(fit_selected[[ipc]]))
      rss <- fit0[ind,"Sum Sq"]/sum(fit0[,"Sum Sq"])
      ind_pc <- plotFrame$PC==ipc
      ind_fit <- match(ind, plotFrame$Covariate[ind_pc])
      plotFrame$Assoc[ind_pc][ind_fit] <- rss
    }
  }
  plotFrame$wrap <- (plotFrame$PC-1) %/% 20
  plotFrame$wrap <- paste0("PCs ", plotFrame$wrap*20+1, "-", min((plotFrame$wrap+1) * 20, npc))
  plotFrame$PC <- sprintf("%02d", plotFrame$PC)
  pl <- ggplot(plotFrame, aes(x=PC, y=Covariate, fill=Assoc)) +
    geom_raster() +
    facet_wrap(~wrap, scales="free_x", ncol=1) + 
    scale_fill_gradient2(low="#4575b4", mid="grey90", high="#d73027") +
    theme_bw() + theme(aspect.ratio = length(unique(plotFrame$Covariate)) / min(20, npc))
  print(pl)
  }




se_pca <- function(se, pc_x=1, pc_y=2, title="QC Visualisation",  caption=print,
           assay=assayNames(se)[1], model=~1, focus=list()
               ) {
  vars <- get_terms(model)
  plotDat <- assay(se, assay)
  colDat <- as.data.frame(colData(se))
  ### PCA
  pc_name=paste0(assay, ".PCA")
  pc <- as.matrix(colData(se)[[pc_name]])
  percentVar <- metadata(colData(se)[[pc_name]])$percentVar
  is_vary <- sapply(colDat[vars$fixed], function(v) length(unique(v))!=1)

  equiv_models <- unique(sapply(focus, update, NULL ~ .))
  focus2model <- match(sapply(focus, update, NULL ~ .), equiv_models)
  resids <- lapply(equiv_models, function(mdl) { # for each unique model
    sample_gene_factor <- residual_heatmap_transform( # get terms
      plotDat,
      colDat,
      mdl)
  })
  equiv_dropgroups <- tapply(
    lapply(focus, function(x) parse_aes_fml(x)$drop_terms),
    focus2model,
    unique
  )
  focus2dropgroup_in_model <- sapply(
    seq(along=focus),
    function(f_ind) {
      match(paste(parse_aes_fml(focus[[f_ind]])$drop_terms, collapse="-"),
            sapply(equiv_dropgroups[[focus2model[f_ind]]], paste, collapse="-")
            )
      })
        
  
  pc_resids <- lapply(seq(along=resids),
                     function(i_model) {
                       my_resid <- resids[[i_model]]
                       lapply(equiv_dropgroups[[i_model]],
                              function(my_drop) {
                                my_include <- !(dimnames(my_resid$terms)[[3]] %in% my_drop)
                                part_resid <- apply(my_resid$terms[,,my_include, drop=FALSE], c(1,2), sum) +
                                  my_resid$resid
                                pc <- prcomp(part_resid, scale=FALSE)
                                list(pc=pc$x, percent=round(100 * pc$sdev^2 / sum( pc$sdev^2 )))
                              })
                       })
                              
  
  
#  cat("<h3> Visualisation of PCs ", pc_x, " and ", pc_y, " coloured by covariate", "</h3>\n", sep="")
  do_labels <- nrow(colDat)<10
  pc.df <- colDat
  for (j in seq(along=focus)) {
    i_equiv_model <- focus2model[j]
    i_dropgroup <- focus2dropgroup_in_model[j]
    
    pc_resid <- pc_resids[[i_equiv_model]][[i_dropgroup]]
    pc.df$PC1 <- pc_resid$pc[,pc_x]
    pc.df$PC2 <- pc_resid$pc[,pc_y]
    my_aes <- parse_aes_fml(focus[[j]])$aes
    
    pl <- ggplot(pc.df, aes(x=PC1, y=PC2))  +
      annotate("text", x = Inf, y = -Inf, label = "PROOF ONLY",
               hjust=1.1, vjust=-1.1, col="white", cex=6,
               fontface = "bold", alpha = 0.8) + 
      geom_point(my_aes, size=3) +
      xlab(paste0("PC ", pc_x, ": ", pc_resid$percent[pc_x], "% variance")) +
      ylab(paste0("PC ", pc_y, ": ", pc_resid$percent[pc_y], "% variance")) +
      labs(colour=j)+ theme_bw()
    colr <- intersect(c("color", "colour"), names(my_aes))
    if (length(colr)>0) {
      colvars <- as.character(rlang::quo_get_expr(my_aes[[colr]]))
      if (length(colvars)>1) {
        colvars <- knitr::combine_words(colvars[-1])
      } else {
        if (is.numeric(colDat[[colvars]])) {
          pal_col <- metadata(colData(se))$palette$ggplot[[colvars]]
          pl <- pl + scale_colour_gradient(low=pal_col[1], high=pal_col[2])
        } else {
          pl <- pl + scale_colour_manual(values=metadata(colData(se))$palette$ggplot[[colvars]])
        }
      }
      my_caption <- paste("Coloured by", colvars)
      pl <- pl+labs(colour=colvars)
    } else {
      my_caption <- ""
    }
    regress_vars <- parse_aes_fml(focus[[j]])$drop_terms
    if (length(regress_vars)) {
      my_caption <- paste(my_caption, "regressing", knitr::combine_words(regress_vars))
    }
    hull_vars <- parse_aes_fml(focus[[j]])$hull
    if (length(hull_vars)) {
      hull_dat <- group_by(pc.df,across(hull_vars)) %>%
        summarise(i=chull(PC1,PC2), PC1=PC1[i], PC2=PC2[i]) %>%
        ungroup()
      pl <- pl+geom_polygon(my_aes, data=hull_dat, alpha=0.2, group=hull_dat[[hull_vars]])
    }
    
    print(pl)
    caption(my_caption)
  }
  return()

  
  cat("<h3> Partial Residuals of Associated PCs</h3>\n", sep="")
  factors_1 <- attr(terms(fit1),"factors")
  many_levels <- sapply(colDat, function(x) length(unique(x)))
  for (j in unique(plotFrame$Covariate)) {
    this_covar <- subset(plotFrame, Covariate==j & !is.na(Assoc) & PC!=PC[nrow(plotFrame)])
    if (nrow(this_covar)==0) next
    pc_first <- as.integer(as.character(this_covar$PC[1]))
    this_covar <- this_covar[order(this_covar$Assoc, decreasing=TRUE),]
    pc_strong <- as.integer(as.character(this_covar$PC[1]))
    pc.df <- subset(pc_frame, PC %in% c(pc_first, pc_strong))
    pc.df$label <- as.character(pc.df$PC)
    pc.df$label[pc.df$PC==pc_first] <- paste0(pc.df$label[pc.df$PC==pc_first], "+First")
    pc.df$label[pc.df$PC==pc_strong] <- paste0(pc.df$label[pc.df$PC==pc_strong], "+Best")
    pc.df$label <- paste0(pc.df$label, " (", percentVar[pc.df$PC], "%)")
    inter_vars <- rownames(factors_1)[factors_1[,j]!=0]
    inter_vars <- inter_vars[order(many_levels[inter_vars], decreasing=TRUE)]
    pc.df$X <- colDat[,inter_vars[1]]
    for (ipc in unique(c(pc_first, pc_strong))) {
      pc.df$coord[pc.df$PC==ipc] <- part.resid(fit_selected[[ipc]])[,j]
    }
    if (length(inter_vars)>1) {
      pc.df$col <-  colDat[,inter_vars[2]]
      if (length(inter_vars)==2) {
        pl <- ggplot(pc.df, aes(x=X, y=coord, colour=col, group=col)) +
          stat_summary(fun = mean, geom="line") + 
          labs(
            x=inter_vars[1],
            colour=paste(inter_vars[-1],collapse="x"),
            y="Residual")+ theme_bw()
        if (is.numeric(pc.df$col)) {
          pal_col <- metadata(colData(se))$palette$ggplot[[inter_vars[2]]]
          pl <- pl + scale_colour_gradient(low=pal_col[1], high=pal_col[2])
        } else {
          pl <- pl + scale_colour_manual(values=metadata(colData(se))$palette$ggplot[[inter_vars[2]]])
        }
      } else {
        pc.df$grp <- Reduce(interaction, colDat[,inter_vars[-1]])
        pl <- ggplot(pc.df, aes(x=X, y=coord, colour=col, group=grp)) +
          stat_summary(fun = mean, geom="line") + 
          labs(
            x=inter_vars[1],
            colour=paste(inter_vars[-1],collapse="x"),
            y="Residual")+ theme_bw()
        if (is.numeric(pc.df$col)) {
          pal_col <- metadata(colData(se))$palette$ggplot[[inter_vars[2]]]
          pl <- pl + scale_colour_gradient(low=pal_col[1], high=pal_col[2])
        } else {
          pl <- pl + scale_colour_manual(values=metadata(colData(se))$palette$ggplot[[inter_vars[2]]])
        }
      }
    } else {
      pl <- ggplot(pc.df, aes(x=X, y=coord, colour=X, group=1)) + 
        stat_summary(fun = mean, geom="line", colour="black") + 
        labs(colour=j,x=inter_vars[1], y="Residual")+ theme_bw()
      if (is.numeric(pc.df$X)) {
        pal_col <- metadata(colData(se))$palette$ggplot[[inter_vars[1]]]
        pl <- pl + scale_colour_gradient(low=pal_col[1], high=pal_col[2])
      } else {
        pl <- pl + scale_colour_manual(values=metadata(colData(se))$palette$ggplot[[inter_vars[1]]])
      }
    }
    pl <- pl  + geom_point(size=2) +
      facet_wrap(~label, scales="free_y") 
    if (do_labels) {pl <- pl + geom_text_repel(aes(label=sample))}
    print(pl)
    caption(paste0("Against ", j))
  }
}

get_terms <- function(model) {
  ret <- list(fixed=NULL, groups=NULL)
  ret$fixed <- all.vars(model)
  return(ret)
}

part.resid <- function(fit) {
  pterms <- predict(fit, type="terms")
  apply(pterms,2,function(x)x+resid(fit))
}
  
  
  


residual_heatmap_transform <- function(mat, cdata, fml) {
  ind <- !apply(is.na(mat), 1, any)
  mat <- mat[ind,]
  assign("tmat", t(mat), envir=environment(fml))
  
  fml <- stats::update(fml, tmat ~ .)
  fit <- lm(fml, data=cdata)
  fit1 <- fit
  class(fit1) <- "lm"
  ind <- c("coefficients","residuals","effects","fitted.values")
  for (i in 1:nrow(mat)) {
    if (nrow(mat)==1) {
      fit1 <- fit
    } else {
      fit1[ind] <- lapply(fit[ind], function(x) x[,i])
    }
    pred <- predict(fit1, type="terms")
    if (i==1) {
      out <- array(0, c(rev(dim(mat)), ncol(pred)), dimnames=c(rev(dimnames(mat)), list(colnames(pred))))
      const <- numeric(dim(out)[2])
    }
    out[,i,] <- pred
    const[i] <- attr(pred, "constant")
  }
  list(terms=out, const=const, resid=fit$residuals)
}

##' Generate MA Plots
##'
##' MA plots of each differential genelist
##' @title Generate MA Plots
##' @param ddsList DESdemonA-generated list of DESeqDataSets
##' @param caption The function to use to create captions
##' @return 
##' @author Gavin Kelly
#' @export
differential_MA <- function(ddsList, caption) {
  for (i in names(ddsList)) {
    res <- as.data.frame(mcols(ddsList[[i]])$results)
    md <- metadata(ddsList[[i]])
    pal <- RColorBrewer::brewer.pal(12, "Set3")
    if (is_formula(md$comparison)) {
      res$class <- ifelse(grepl("\\*", res$class),res$class, "None")
      colind <- (seq(along=unique(res$class), from=0)) %% length(pal) + 1
      yax <- "Largest fold change"
      if (length(colind)>12) {
        res$class <- ifelse(res$class=="None","None","Sig")
        cols <- setNames(c("blue","grey"), c("Sig","None"))
        lpos <- "none"
      } else {
        cols <- setNames(c(pal[colind]), unique(res$class))
        cols["None"] <- "grey"
        lpos <- "bottom"
      }
    } else {
      res$class <- ifelse(grepl("\\*", res$class), "Sig", "Not")
      cols <- setNames(c("blue","grey"), c("Sig","Not"))
      yax <- "Log fold change (shrunk)"
      lpos <- "none"
    }
    pl <- ggplot(res,
                aes(x=baseMean,
                    y=shrunkLFC,
                    colour=class,
                    size=ifelse(class %in% c("Not","None"),.1 , .5),
                    label=ifelse(class %in% c("Not","None"), "" , symbol)
                    )
                ) +
      geom_point() +
      scale_x_log10() + scale_colour_manual(values=cols) + scale_size_identity() +
      theme(legend.position=lpos) +
      labs(x="Mean of normalised counts", y=yax)+ theme_bw()
    ind <- order(res$shrunkLFC)[c(1:5, nrow(res)-(0:4))]
    pl <- pl+ggrepel::geom_text_repel(size=4, data=res[ind,])
    print(pl)
    caption(paste0("MA plot of differential genes ", sub(".*\\t", "", i)))
  }
}

##' Heatmap colour-scheme generator
##'
##' For each column in a dataframe, generate a sensible colour palette
##' for each column
##' @title Heatmap colour-scheme generator
##' @param df
##' @return
##' @author Gavin Kelly
df2colorspace <- function(df, palette="Set1") {
  pal <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[palette, "maxcolors"], palette)
  if (ncol(df)==0) return(list(Heatmap=list(), ggplot=list()))
  nm <- names(df)
  df <- dplyr::mutate_if(setNames(as.data.frame(df),nm), is.character, as.factor)
  seq_cols <-c("Blues", "Greens", "Oranges", "Purples", "Reds")
  df <- df[,order(sapply(df, is.numeric)),drop=FALSE] # move factors to the front
  # for factors, zero-based starting index for colours
  start_levels <- cumsum(c(0,sapply(df, nlevels)))[1:length(df)] 
  is_num <- sapply(df, is.numeric)
  # for numerics, which seq palette shall we use for this factor
  start_levels[is_num] <- (cumsum(is_num[is_num])-1) %% length(seq_cols) + 1
  res <- list()
  res$Heatmap <- purrr::map2(df, start_levels,
              function(column, start_level) {
                if (is.factor(column)) {
                  setNames(pal[(seq(start_level, length=nlevels(column)) %% length(pal)) + 1],
                                     levels(column))
                } else {
                  my_cols <- RColorBrewer::brewer.pal(3, seq_cols[start_level])[-2]
                  circlize::colorRamp2(range(column, na.rm=TRUE), my_cols)
                }
              }
              )
  res$ggplot <- purrr::map2(df, start_levels,
              function(column, start_level) {
                if (is.factor(column)) {
                  setNames(pal[(seq(start_level, length=nlevels(column)) %% length(pal)) + 1],
                                     levels(column))
                } else {
                  RColorBrewer::brewer.pal(3, seq_cols[start_level])[-2]
                }
              }
              )
  res
}
  

parse_aes_fml <- function(fml) {
  a <- eval(fml[[2]])
  if (any(names(a)=="regress")) {
    regress <- rlang::quo_get_expr(a$regress)
    if (is.call(regress)) {
      regress <- as.character(regress)[-1]
    } else {
      regress <- as.character(regress)[1]
    }
    a$regress <- NULL
  } else {
    regress=character()
  }
  if (any(names(a)=="hull")) {
    hull <- rlang::quo_get_expr(a$hull)
    if (is.call(hull)) {
      hull <- as.character(hull)[-1]
    } else {
      hull <- as.character(hull)[1]
    }
    a$hull <- NULL
  } else {
    hull=character()
  }
  list(model=update(fml, NULL ~ .),  aes=a, drop_terms=regress, hull=hull)
}

  

