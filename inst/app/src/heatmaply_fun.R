



# Wrapper for heatmaply plot

#' @export
heatmaply_plot <- function (dat, genes_to_plot, cells_to_plot, n_genes = 50, group = NULL, group_colour = NULL, log = T, pseudocount=1,
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
            gene_grouping <- unlist(lapply(names(genes_to_plot),
                                           function(nm) rep(nm, with(genes_to_plot[[nm]],
                                                                     length(head(ix[significant], n_genes))))))
        }
        else {
            gene_indices <- unlist(lapply(genes_to_plot, function(x) x$ix[1:n_genes]))
            gene_grouping <- rep(names(genes_to_plot), each = n_genes)
        }
        gene_annotation <- data.frame(ID = as.factor(gene_grouping))
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
        rownames(cell_annotation) <- colnames(value)
    } else {
        cell_annotation <- NULL
    }
    if (!is.null(gene_annotation)) {
        rownames(gene_annotation) <- rownames(value)
    }
    if (is.null(group_colour)) {
        anno_colors <- NULL
    } else {
        anno_colors <- group_colour
    }
    value <- value[complete.cases(value),]
    suppressWarnings({
            old_dev <- grDevices::dev.cur()
            on.exit(utils::capture.output({
                grDevices::dev.off()
                grDevices::dev.set(old_dev)
            }))
        heatmaply(value,
                  colors = heatmap_color,
                  Rowv = cluster_rows, Colv = cluster_cols,
                  showticklabels = c(F,T),
                  col_side_colors = cell_annotation,
                  col_side_palette = anno_colors,
                  subplot_heights=c(0.03, 0.97),
                  fontsize_row = fontsize)
    })
}


#heatmaply(val, Rowv=F, Colv=F, colors = get_numeric_color( pal="RdYlBu"), showticklabels = c(F,T), col_side_colors = ca, col_side_palette = NULL)
#gbm_pheatmap2(hexpr,genes_to_plot = deg, cells_to_plot=cells_to_plot, group=group, colour=colour, n_genes=50, fontsize=6, limits=c(-2,2))


