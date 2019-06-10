

# Code adapted from CellrangerRkit package
#' @export
runsSeq <- function(dat, group, fdata, order_by, p_cutoff, min_mean, min_log2fc, id_col = "gene_id", name_col = "gene_name") {
    t_mat <- t(dat)
    uni_labels <- sort(unique(group))
    cat("Computing differential expression parameters...\n")
    sseq_params <- compute_sseq_params2(t_mat)
    ordered_genes <- lapply(uni_labels, function(cluster_label) {
        de_result <- get_ordered_gene_list_sseq2(fdata, group,
                                                cluster_label, sseq_params, t_mat, id_col = id_col, name_col= name_col)
        de_result$ix <- 1:nrow(de_result)
        de_result$id <- de_result[[id_col]]
        de_result$symbol <- de_result[[name_col]]
        de_result$significant <- with(de_result, common_mean >=
                                          min_mean & p_adj < p_cutoff & log2fc >= min_log2fc)
        if (order_by == "pvalue") {
            ord <- order(with(de_result, ifelse(significant,
                                                p, Inf)))
        }
        else if (order_by == "log2fc") {
            ord <- order(-with(de_result, ifelse(significant,
                                                 log2fc, -Inf)))
        }
        de_result[ord, ]
    })
    names(ordered_genes) <- uni_labels
    return(ordered_genes)
}

#' @export
compute_sseq_params2 <- function (x, zeta_quantile = 0.995)
{
    params <- list()
    N <- nrow(x)
    G <- ncol(x)
    grand_median <- median(rowSums(x))
    s_ij <- rowSums(x)/grand_median
    params$s_ij <- s_ij
    x_size_norm <- x/s_ij
    mu_g <- colMeans(x_size_norm)
    v_g <- apply(x_size_norm, 2, var)
    use_g <- v_g > 0
    params$mu_g <- mu_g
    params$v_g <- v_g
    params$use_g <- use_g
    phi_mm_g <- pmax(0, (N * v_g - mu_g * sum(1/s_ij))/(mu_g^2 *
                                                            sum(1/s_ij)))
    params$phi_mm_g <- phi_mm_g
    zeta_hat <- quantile(params$phi_mm_g, zeta_quantile, na.rm = T)
    params$zeta_hat <- zeta_hat
    mean_phi_mm_g <- mean(phi_mm_g[use_g])
    delta <- (sum((phi_mm_g[use_g] - mean_phi_mm_g)^2)/(G - 1))/(sum((phi_mm_g[use_g] -
                                                                          zeta_hat)^2)/(G - 2))
    params$delta <- delta
    phi_g <- rep(NA, G)
    phi_g[use_g] <- (1 - delta) * phi_mm_g[use_g] + delta * zeta_hat
    params$phi_g <- phi_g
    params
}

#' @export
get_ordered_gene_list_sseq2 <- function (fdata, cluster_labels, test_ID, sseq_params, t_mat, id_col = "gene_id", name_col = "gene_name")
{
    cat("Comparing Cluster", test_ID, "against the other clusters...\n")
    group0 <- cluster_labels == test_ID
    group1 <- cluster_labels != test_ID
    de_result <- sseq_differential_expression2(t_mat, group0,
                                              group1, sseq_params, gene_ids = fdata[[id_col]], gene_symbols = fdata[[name_col]])
    return(de_result)
}

#' @export
sseq_differential_expression2 <- function (x, cond0, cond1, sseq_params, gene_ids, gene_symbols)
{
    x_a <- x[cond0, , drop = F]
    x_b <- x[cond1, , drop = F]
    G <- ncol(x)
    s_a <- sum(sseq_params$s_ij[cond0])
    s_b <- sum(sseq_params$s_ij[cond1])
    x_ga <- colSums(x_a)
    x_gb <- colSums(x_b)
    p_res <- rep(NA, G)
    p_res[sseq_params$use_g] <- sapply(which(sseq_params$use_g),
                                       function(g) nb_exact_test2(x_ga[g], x_gb[g], s_a, s_b,
                                                                 sseq_params$mu_g[g], sseq_params$phi_g[g]))
    p_adj <- rep(1, G)
    p_adj[sseq_params$use_g] <- p.adjust(p_res[sseq_params$use_g],
                                         method = "BH")
    data.frame(gene_id = gene_ids, gene_name = gene_symbols,
               tested = sseq_params$use_g, sum_a = x_ga, sum_b = x_gb,
               common_mean = sseq_params$mu_g, dispersion = sseq_params$phi_g,
               mean_a_sizenorm = x_ga/s_a, mean_b_sizenorm = x_gb/s_b,
               log2fc = log2((1 + x_ga)/(1 + s_a)) - log2((1 + x_gb)/(1 +
                                                                          s_b)), p = p_res, p_adj = p_adj)
}


#' @export
nb_exact_test2 <- function (x_a, x_b, s_a, s_b, mu, phi)
{
    all_x_a <- seq(0, x_a + x_b, 1)
    all_x_b <- seq(x_a + x_b, 0, -1)
    .prob <- function(x, s) dnbinom(x, mu = s * mu, size = 1/(phi/s))
    p_obs <- .prob(x_a, s_a) * .prob(x_b, s_b)
    p_all <- .prob(all_x_a, s_a) * .prob(all_x_b, s_b)
    sum(p_all[p_all <= p_obs])/sum(p_all)
}




#' @export
order_cell_by_clusters2 <- function (pmeta, clu)
{
    uni_labels <- unique(clu) # Keep the order
    sorted_clu_id <- sort(clu, index.return = TRUE)
    index_list <- lapply(uni_labels, function(x) sorted_clu_id$ix[sorted_clu_id$x ==
                                                                      x])
    cell_info <- as.character(pmeta$barcode)
    ordered_cells <- lapply(index_list, function(x) {
        list(barcode = cell_info[x], ix = x)
    })
    names(ordered_cells) <- uni_labels
    return(ordered_cells)
}

#' @export
gbm_pheatmap2 <- function (dat, genes_to_plot, cells_to_plot, n_genes = 50, group = NULL, group_colour = NULL, log = T, pseudocount = 1,
                           heatmap_color =colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
                           limits = c(-3, 3), fontsize=10, scale = TRUE, cluster_rows = FALSE, cluster_cols = FALSE, clustering_distance_rows = "euclidean", clustering_method = "complete")
{
    if (!is.list(genes_to_plot)) {
        cat("Plotting one gene set instead of multiple cluster-specific gene sets\\n")
        gene_indices <- match(genes_to_plot, rownames(dat))
        gene_annotation <- NULL
    }
    else {
        if ("significant" %in% names(genes_to_plot[[1]])) {
            gene_indices <- unlist(lapply(genes_to_plot, function(x) with(x,
                                                                          head(ix[significant], n_genes))))
        }
        else {
            gene_indices <- unlist(lapply(genes_to_plot, function(x) x$ix[1:n_genes]))
        }
    }
    gene_indices <- gene_indices[!is.na(gene_indices)]
    if(is.null(cells_to_plot)) {
        cell_indices <- 1:ncol(dat)
    } else {
        cell_indices <- unlist(lapply(cells_to_plot, function(x) x$ix))
    }

    if(log) {
        value <- log10(dat[gene_indices, cell_indices]+pseudocount)
    } else {
        value <- dat[gene_indices, cell_indices]
    }
    if(scale) {
        value <- t(scale(t(as.matrix(value))))
    } else {
        value <- as.matrix(dat)[gene_indices, cell_indices]
    }

    if(!is.null(limits)) {
        value[value < limits[1]] <- limits[1]
        value[value > limits[2]] <- limits[2]
    }

    rownames(value) <- make.unique(rownames(dat)[gene_indices])
    if(!is.null(group)) {
        cell_grouping <- as.character(group)
    } else {
        cell_grouping <- unlist(lapply(1:length(cells_to_plot), function(x) {
            rep(names(cells_to_plot)[x], length(cells_to_plot[[x]]$barcode))
        }))
    }

    if(!is.null(cell_grouping)){
        cell_annotation <- data.frame(ID = cell_grouping)
        rownames(cell_annotation) <- colnames(dat)
    } else {
        cell_annotation <- NULL
    }
    #assign("cell_annotation2", cell_annotation, env=.GlobalEnv)
    #assign("value2", value, env=.GlobalEnv)
    if (is.null(group_colour)) {
        anno_colors <- NULL
    } else {
        anno_colors <- list(ID=group_colour)
    }

    value <- value[complete.cases(value),]
    pheatmap::pheatmap(value,
                       color = heatmap_color,
                       cluster_rows = cluster_rows, cluster_cols = cluster_cols,
                       clustering_distance_rows = clustering_distance_rows, clustering_method = clustering_method,
                       show_colnames = FALSE, annotation_row = NULL,
                       annotation_col = cell_annotation, annotation_names_row = FALSE,
                       annotation_names_col = FALSE, annotation_colors = anno_colors, fontsize = fontsize)
}






