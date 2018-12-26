

#' @export
filter_cds <- function(cds, min_detect=1, min_numc_expressed = 10, min_disp_ratio=1){
    numc_expressed <- Matrix::rowSums(exprs(cds) > min_detect)
    g_filter <- numc_expressed >= min_numc_expressed
    sum(g_filter)
    cds_oidx <- cds[g_filter,]
    #cds_oidx@normalized_data_projection <- matrix() # bug in monocle3 cellset dataset class definition
    cds_oidx <- estimateDispersions(cds_oidx)
    disp_subset = dispersionTable(cds_oidx)
    disp_list = subset(disp_subset,(dispersion_empirical/dispersion_fit)>min_disp_ratio)
    disp_genes = disp_list$gene_id
    message(paste0("Final VEG number:", length(disp_genes)))
    cds_oidx <- setOrderingFilter(cds_oidx, disp_genes)
    return(cds_oidx)
}

#' @export
compute_pca_cds <- function(cds, num_dim =100, scvis=NULL, use_order_gene = T, residualModelFormulaStr = NULL, return_type=c("irlba","scvis","proj")) {
    FM <- normalize_expr_data2(cds, "log", 1, use_order_gene = use_order_gene)
    xm <- Matrix::rowMeans(FM)
    xsd <- sqrt(Matrix::rowMeans((FM - xm)^2))
    FM <- FM[xsd > 0, ]

    if (!is.null(residualModelFormulaStr)) {
        X.model_mat <- sparse.model.matrix(as.formula(residualModelFormulaStr), data = pData(cds), drop.unused.levels = TRUE)
        fit <- limma::lmFit(FM, X.model_mat)
        beta <- fit$coefficients[, -1, drop = FALSE]
        beta[is.na(beta)] <- 0
        FM <- as.matrix(FM) - beta %*% t(as.matrix(X.model_mat[, -1]))
    }
    #print(class(FM))
    irlba_res <- prcomp_irlba2(t(as.matrix(FM)), n = min(num_dim, min(dim(FM)) - 1), center = TRUE, scale. = TRUE) # Change to recent sparse version if necessary
    irlba_sdev <- irlba_res$sdev
    names(irlba_sdev) <- paste0("PC",1:length(irlba_sdev))
    pca_proj <- as.data.frame(irlba_res$x)
    rownames(pca_proj) <- colnames(cds)

    if(return_type == "scvis") {
        scvis@pca <- pca_proj
        return(scvis)
    } else if(return_type == "irlba"){
        return(irlba_res)
    }else {
        return(pca_proj)
    }
}

#' @export
prcomp_irlba2 <- function (x, n = 3, retx = TRUE, center = TRUE, scale. = FALSE,
                           ...)
{
    a <- names(as.list(match.call()))
    ans <- list(scale = scale.)
    if ("tol" %in% a)
        warning("The `tol` truncation argument from `prcomp` is not supported by\n`prcomp_irlba`. If specified, `tol` is passed to the `irlba` function to\ncontrol that algorithm's convergence tolerance. See `?prcomp_irlba` for help.")
    if (!is.matrix(x))
        x <- as.matrix(x)
    args <- list(A = x, nv = n)
    if (is.logical(center)) {
        if (center)
            args$center <- colMeans(x)
    }
    else args$center <- center
    if (is.logical(scale.)) {
        if (is.numeric(args$center)) {
            f <- function(i) sqrt(sum((x[, i] - args$center[i])^2)/(nrow(x) -
                                                                        1L))
            scale. <- vapply(seq(ncol(x)), f, pi, USE.NAMES = FALSE)
            ans$scale_val <- scale.
            if (ans$scale)
                ans$totalvar <- ncol(x)
            else ans$totalvar <- sum(scale.^2)
        }
        else {
            if (ans$scale) {
                scale. <- apply(x, 2L, function(v) sqrt(sum(v^2)/max(1,
                                                                     length(v) - 1L)))
                ans$scale_val <- scale.
                f <- function(i) sqrt(sum((x[, i]/scale.[i])^2)/(nrow(x) -
                                                                     1L))
                ans$totalvar <- sum(vapply(seq(ncol(x)), f, pi,
                                           USE.NAMES = FALSE)^2)
            }
            else {
                f <- function(i) sum(x[, i]^2)/(nrow(x) - 1L)
                ans$totalvar <- sum(vapply(seq(ncol(x)), f, pi,
                                           USE.NAMES = FALSE))
            }
        }
        if (ans$scale)
            args$scale <- scale.
    }
    else {
        args$scale <- scale.
        f <- function(i) sqrt(sum((x[, i]/scale.[i])^2)/(nrow(x) -
                                                             1L))
        ans$totalvar <- sum(vapply(seq(ncol(x)), f, pi, USE.NAMES = FALSE))
    }
    if (!missing(...))
        args <- c(args, list(...))
    s <- do.call(irlba, args = args)
    ans$sdev <- s$d/sqrt(max(1, nrow(x) - 1))
    ans$rotation <- s$v
    colnames(ans$rotation) <- paste("PC", seq(1, ncol(ans$rotation)),
                                    sep = "")
    ans$center <- args$center
    if (retx) {
        ans <- c(ans, list(x = sweep(s$u, 2, s$d, FUN = `*`)))
        colnames(ans$x) <- paste("PC", seq(1, ncol(ans$rotation)),
                                 sep = "")
    }
    class(ans) <- c("irlba_prcomp", "prcomp")
    ans
}


#' @export
compute_umap_pca <- function(pca_proj, num_dim = 30,  n_component=2) {
    umap_res <- list()
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
        umap_path = "src/python_src/umap.py"),
        extra_arguments[names(extra_arguments) %in%
                            c("python_home", "n_neighbors", "metric", "n_epochs",
                              "negative_sample_rate", "alpha", "init", "min_dist",
                              "spread", "set_op_mix_ratio", "local_connectivity",
                              "bandwidth", "gamma", "a", "b", "random_state",
                              "metric_kwds", "angular_rp_forest", "verbose")]
    )
    cur_dim <- num_dim
    umap_args$X <- pca_proj[,1:cur_dim]
    tmp <- do.call(monocle_UMAP, umap_args)
    tmp$embedding_ <- (tmp$embedding_ - min(tmp$embedding_))/max(tmp$embedding_)
    umap_proj <- as.data.frame(tmp$embedding_)
    row.names(umap_proj) <- rownames(pca_proj)
    return(umap_proj)
}


#' @export
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

#' @export
normalize_expr_data2 <-function (cds, norm_method = c("log", "vstExprs", "none"), pseudo_expr = 1, use_order_gene = TRUE, relative_expr = TRUE)
{
    FM <- exprs(cds)
    if(!use_order_gene) {
        fData(cds)$use_for_ordering <- NULL
    }
    if (is.null(fData(cds)$use_for_ordering) == FALSE && nrow(subset(fData(cds),
                                                                     use_for_ordering == TRUE)) > 0) {
        FM <- FM[fData(cds)$use_for_ordering, ]
    }
    norm_method <- match.arg(norm_method)
    if (cds@expressionFamily@vfamily %in% c("negbinomial", "negbinomial.size")) {
        if (is.null(pseudo_expr)) {
            if (norm_method == "log")
                pseudo_expr = 1
            else pseudo_expr = 0
        }
        checkSizeFactors(cds)
        if (norm_method == "vstExprs") {
            if (relative_expr == FALSE)
                message("Warning: relative_expr is ignored when using norm_method == 'vstExprs'")
            if (is.null(fData(cds)$use_for_ordering) == FALSE &&
                nrow(subset(fData(cds), use_for_ordering == TRUE)) >
                0) {
                VST_FM <- vstExprs(cds[fData(cds)$use_for_ordering,
                                       ], round_vals = FALSE)
            }
            else {
                VST_FM <- vstExprs(cds, round_vals = FALSE)
            }
            if (is.null(VST_FM) == FALSE) {
                FM <- VST_FM
            }
            else {
                stop("Error: set the variance-stabilized value matrix with vstExprs(cds) <- computeVarianceStabilizedValues() before calling this function with use_vst=TRUE")
            }
        }
        else if (norm_method == "log") {
            if (relative_expr)
                FM <- Matrix::t(Matrix::t(FM)/sizeFactors(cds))
            if (is.null(pseudo_expr))
                pseudo_expr <- 1
            FM <- FM + pseudo_expr
            FM <- log2(FM)
        }
        else if (norm_method == "none") {
            FM <- Matrix::t(Matrix::t(FM)/sizeFactors(cds))
            FM <- FM + pseudo_expr
        }
    }
    else if (cds@expressionFamily@vfamily == "binomialff") {
        if (norm_method == "none") {
            ncounts <- FM > 0
            ncounts[ncounts != 0] <- 1
            FM <- Matrix::t(Matrix::t(ncounts) * log(1 + ncol(ncounts)/rowSums(ncounts)))
        }
        else {
            stop("Error: the only normalization method supported with binomial data is 'none'")
        }
    }
    else if (cds@expressionFamily@vfamily == "Tobit") {
        FM <- FM + pseudo_expr
        if (norm_method == "none") {
        }
        else if (norm_method == "log") {
            FM <- log2(FM)
        }
        else {
            stop("Error: the only normalization methods supported with Tobit-distributed (e.g. FPKM/TPM) data are 'log' (recommended) or 'none'")
        }
    }
    else if (cds@expressionFamily@vfamily == "gaussianff") {
        if (norm_method == "none") {
            FM <- FM + pseudo_expr
        }
        else {
            stop("Error: the only normalization method supported with gaussian data is 'none'")
        }
    }
    return(FM)
}

#' @export
checkSizeFactors <- function (cds)
{
    if (cds@expressionFamily@vfamily %in% c("negbinomial", "negbinomial.size")) {
        if (is.null(sizeFactors(cds))) {
            stop("Error: you must call estimateSizeFactors() before calling this function.")
        }
        if (sum(is.na(sizeFactors(cds))) > 0) {
            stop("Error: one or more cells has a size factor of NA.")
        }
    }
}

