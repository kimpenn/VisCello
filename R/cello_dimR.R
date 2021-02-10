
#' @export
compute_pca_cello <- function(eset, cello, num_dim =100, residualModelFormulaStr = NULL) {
    FM <- eset@assayData$norm_exprs
    if (!is.null(residualModelFormulaStr)) {
        X.model_mat <- Matrix::sparse.model.matrix(as.formula(residualModelFormulaStr), data = pData(eset), drop.unused.levels = TRUE)
        fit <- limma::lmFit(FM, X.model_mat)
        beta <- fit$coefficients[, -1, drop = FALSE]
        beta[is.na(beta)] <- 0
        FM <- as.matrix(FM) - beta %*% Matrix::t(X.model_mat[, -1])
    }
    xm <- Matrix::rowMeans(FM)
    xsd <- sqrt(Matrix::rowMeans((FM - xm)^2))
    FM <- FM[xsd > 0, ]
    irlba_res <- prcomp_irlba(t(as.matrix(FM)), n = min(num_dim, min(dim(FM)) - 1), center = TRUE, scale. = TRUE) # Change to recent sparse version if necessary
    pca_proj <- as.data.frame(irlba_res$x)
    rownames(pca_proj) <- colnames(eset)
    cello@proj[["PCA"]] <- pca_proj
    return(cello)
}

#' @export
compute_tsne_cello <- function(eset, cello, use_dim=30, n_component = 2, perplexity=30, ...) {
    if(is.null(cello@proj[["PCA"]])) stop("Compute PCA first.")
    pca_proj <- cello@proj[["PCA"]]
    if(ncol(pca_proj) < use_dim) stop("Compute PCA with dimension greater than that specified in use_dim.")
    
    tsne_res <- Rtsne::Rtsne(as.matrix(pca_proj[,1:use_dim]), dims = n_component, pca = F, perplexity=perplexity, ...)
    tsne_proj <- as.data.frame(tsne_res$Y[, 1:n_component])
    colnames(tsne_proj) <- paste0("TSNE_", 1:n_component)
    rownames(tsne_proj) <- colnames(eset)
    cello@proj[[paste0("TSNE-",n_component, "D [", use_dim, "PC]" )]] <- tsne_proj
    return(cello)
}

#' @export
compute_umap_cello <- function(eset, cello, use_dim = 30, 
                               n_component=2, 
                               metric = "cosine",
                               min_dist = 0.1,
                               n_neighbors = 15L,
                               fast_sgd = FALSE,
                               nn_method = "annoy", 
                               cores=1,
                               verbose=T, ...) {
    if(is.null(cello@proj[["PCA"]])) stop("Compute PCA first.")
    pca_proj <- cello@proj[["PCA"]]
    if(ncol(pca_proj) < use_dim) stop("Compute PCA with dimension greater than that specified in use_dim.")
    
    umap_proj <- uwot::umap(as.matrix(pca_proj[,1:use_dim]),
               n_components = n_component,
               metric = metric,
               min_dist = min_dist,
               n_neighbors = n_neighbors,
               fast_sgd = fast_sgd,
               n_threads=cores,
               verbose=verbose,
               nn_method = nn_method,
               ...)
    colnames(umap_proj) <- paste0("UMAP_", 1:n_component)
    rownames(umap_proj) <- colnames(eset)
    cello@proj[[paste0("UMAP-",n_component, "D [", use_dim, "PC]" )]] <- umap_proj
    return(cello)
}

