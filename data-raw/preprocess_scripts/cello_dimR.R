




# PCA computation for creating cello

compute_pca_cello <- function(eset, cello, num_dim =100) {
    FM <- eset@assayData$norm_exprs
    xm <- Matrix::rowMeans(FM)
    xsd <- sqrt(Matrix::rowMeans((FM - xm)^2))
    FM <- FM[xsd > 0, ]
    irlba_res <- prcomp_irlba(t(as.matrix(FM)), n = min(num_dim, min(dim(FM)) - 1), center = TRUE, scale. = TRUE) # Change to recent sparse version if necessary
    pca_proj <- as.data.frame(irlba_res$x)
    rownames(pca_proj) <- colnames(eset)
    cello@proj[["PCA"]] <- pca_proj
    return(cello)
}


compute_tsne_cello <- function(eset, cello, use_dim=30, n_component = 2, perplexity=30, ...) {
    if(is.null(cello@proj[["PCA"]])) stop("Compute PCA first.")
    pca_proj <- cello@proj[["PCA"]]
    if(ncol(pca_proj) < use_dim) stop("Compute PCA with dimension greater than that specified in use_dim.")
    
    tsne_res <- Rtsne::Rtsne(as.matrix(pca_proj), dims = n_component, pca = F, perplexity=perplexity, ...)
    tsne_proj <- as.data.frame(tsne_res$Y[, 1:n_component])
    colnames(tsne_proj) <- paste0("TSNE_", 1:n_component)
    rownames(tsne_proj) <- colnames(eset)
    cello@proj[[paste0("TSNE-",n_component, "D [", use_dim, "PC]" )]] <- tsne_proj
    return(cello)
}


compute_umap_cello <- function(eset, cello, use_dim = 30, n_component=2, umap_path = "src/python_src/umap.py") {
    if(is.null(cello@proj[["PCA"]])) stop("Compute PCA first.")
    pca_proj <- cello@proj[["PCA"]]
    if(ncol(pca_proj) < use_dim) stop("Compute PCA with dimension greater than that specified in use_dim.")
    
    extra_arguments = list(
        n_neighbors = 20,
        min_dist = 0.1,
        metric = "cosine")
    umap_args <- c(list(
        X = pca_proj,
        log = F,
        n_component = n_component,
        verbose = T,
        return_all = T,
        umap_path =umap_path),
        extra_arguments[names(extra_arguments) %in%
                            c("python_home", "n_neighbors", "metric", "n_epochs",
                              "negative_sample_rate", "alpha", "init", "min_dist",
                              "spread", "set_op_mix_ratio", "local_connectivity",
                              "bandwidth", "gamma", "a", "b", "random_state",
                              "metric_kwds", "angular_rp_forest", "verbose")]
    )
    umap_args$X <- pca_proj[,1:use_dim]
    tmp <- do.call(monocle_UMAP, umap_args)
    tmp$embedding_ <- (tmp$embedding_ - min(tmp$embedding_))/max(tmp$embedding_)
    umap_proj <- as.data.frame(tmp$embedding_)
    colnames(umap_proj) <- paste0("UMAP_", 1:n_component)
    rownames(umap_proj) <- colnames(eset)
    cello@proj[[paste0("UMAP-",n_component, "D [", use_dim, "PC]" )]] <- umap_proj
    return(cello)
}

# Monocle 3 umap code
monocle_UMAP <- function (X, python_home = system("which python", intern = TRUE),
                          log = TRUE, n_neighbors = 15L, n_component = 2L, metric = "correlation",
                          n_epochs = NULL, negative_sample_rate = 5L, learning_rate = 1,
                          init = "spectral", min_dist = 0.1, spread = 1, set_op_mix_ratio = 1,
                          local_connectivity = 1L, repulsion_strength = 1, a = NULL,
                          b = NULL, random_state = 0L, metric_kwds = reticulate::dict(),
                          angular_rp_forest = FALSE, target_n_neighbors = -1L, target_metric = "categorical",
                          target_metric_kwds = reticulate::dict(), target_weight = 0.5,
                          transform_seed = 42L, verbose = FALSE, return_all = FALSE,
                          umap_path = paste(system.file(package = "monocle"), "umap.py", sep = "/"))
{
    reticulate::use_python(python_home)
    tryCatch({
        reticulate::import("umap")
    }, warning = function(w) {
    }, error = function(e) {
        print(e)
        stop("please pass the python home directory where umap is installed with python_home argument!")
    }, finally = {
    })
    reticulate::source_python(umap_path)
    if (length(grep("Matrix", class(X))) == 0) {
        X <- as(as.matrix(X), "TsparseMatrix")
    }
    else {
        X <- as(X, "TsparseMatrix")
    }
    i <- as.integer(X@i)
    j <- as.integer(X@j)
    if (log) {
        val <- log(X@x + 1)
    }
    else {
        val <- X@x
    }
    dim <- as.integer(X@Dim)
    if (is.null(n_epochs) == F) {
        n_epochs <- as.integer(n_epochs)
    }
    if (is.null(a) == F) {
        a <- as.numeric(a)
    }
    if (is.null(b) == F) {
        n_epochs <- as.numeric(b)
    }
    if (is.list(metric_kwds) == F) {
        metric_kwds <- reticulate::dict()
    }
    else {
        metric_kwds <- reticulate::dict(metric_kwds)
    }
    if (is.list(target_metric_kwds) == F) {
        target_metric_kwds <- reticulate::dict()
    }
    else {
        target_metric_kwds <- reticulate::dict(target_metric_kwds)
    }
    umap_res <- umap(i, j, val, dim, as.integer(n_neighbors),
                     as.integer(n_component), as.character(metric), n_epochs,
                     as.integer(negative_sample_rate), as.numeric(learning_rate),
                     as.character(init), as.numeric(min_dist), as.numeric(spread),
                     as.numeric(set_op_mix_ratio), as.integer(local_connectivity),
                     as.numeric(repulsion_strength), a, b, as.integer(random_state),
                     metric_kwds, as.logical(angular_rp_forest), as.integer(target_n_neighbors),
                     as.character(target_metric), target_metric_kwds, as.numeric(target_weight),
                     as.integer(transform_seed), as.logical(verbose))
    if (return_all) {
        return(umap_res)
    }
    else {
        umap_res$embedding_
    }
}
