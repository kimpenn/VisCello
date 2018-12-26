


#' @export
get_color_vector <-function (labels, pal = "Set1", maxCol = 9, nogrey = T)
{
    unq <- unique(labels)
    hmcol <- RColorBrewer::brewer.pal(maxCol, pal)
    if(nogrey) {
        hmcol <- hmcol[!hmcol %in% c("#999999","#B3B3B3")]
    }
    colv <- rep(NA, length(labels))
    #if (length(unq) > maxCol) {
        cp <- colorRampPalette(hmcol)
        hmcol <- cp(length(unq))
    #}
    for (i in 1:length(unq)) {
        colv[labels == unq[i]] <- hmcol[i]
    }
    return(colv)
}

#' @export
get_numeric_color <- function(palette) {
    if(palette %in% get_brewer_set(c("sequential", "diverging"))) {
        colorRampPalette(rev(RColorBrewer::brewer.pal(9,palette)))(100)
    } else if(palette %in% list("viridis" = "viridis", "magma" = "magma", "plasma" = "plasma", "inferno" = "inferno")) {
        viridis(n = 100, option = palette)
    } else if(palette == "diverge_hcl") {
        colorRampPalette(colorspace::diverge_hcl(7))(100)
    } else if(palette == "redgreen") {
        rev(gplots::redgreen(75))
    } else if(palette == "rainbow") {
        colorRampPalette(rev(rainbow(10)))(100)
    } else if(palette == "rainbow2") {
        c("#CCCCCCCC",rainbow(500)[50:500])
    } else if(palette == "grey&red") {
        colorRampPalette(c("grey", "red"))(100)
    }
}

#' @export
gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}


#' @export
numeric_rgb_range <- function(col = NULL, zgrey=F) {
    rgb_scale <- apply(col2rgb(col),2,function(x){paste0(x,collapse =",")})
    rgb_scale_text<-paste0("rgb(", rgb_scale, ")")
    if(zgrey) {
        cus_range <- seq(1e-10, 1, length.out =length(rgb_scale))
        rgb_scale_list<- list(c(0, "rgb(178, 178, 178)"))
    } else {
        cus_range <- seq(0, 1, length.out =length(rgb_scale))
        rgb_scale_list<- list()
    }
    for(i in 1:length(rgb_scale)) {
        rgb_scale_list[[length(rgb_scale_list) + 1]] <- c(cus_range[i], rgb_scale_text[i])
    }
    return(rgb_scale_list)
}

#' @export
get_brewer_set <- function(palette = c("sequential", "diverging", "qualitative")) {
    match.arg(palette,
              several.ok = TRUE)

    sequential_palette <- c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys',
                            'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                            'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')
    names(sequential_palette) <- sequential_palette
    diverging_palette <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
    names(diverging_palette) <- diverging_palette
    qualitative_palette <- c('Accent','Dark2','Paired', 'Pastel1', 'Pastel2','Set1', 'Set2', 'Set3')
    names(qualitative_palette) <- qualitative_palette
    return_palette = list()
    if("qualitative" %in% palette) {
        return_palette <- c(return_palette, as.list(qualitative_palette))
    }
    if("diverging" %in% palette) {
        return_palette <- c(return_palette, as.list(diverging_palette))
    }
    if("sequential" %in% palette) {
        return_palette <- c(return_palette, as.list(sequential_palette))
    }

    return(return_palette)
}

# Change the default color scale by overriding the function
#' @export
visualize_umi_counts <- function (gbm, projection, limits = c(0, 10), marker_size = 0.1)
{
    gene_values <- log(colSums(as.matrix(Biobase::exprs(gbm))), base = 10)
    gene_values[gene_values < limits[1]] <- limits[1]
    gene_values[gene_values > limits[2]] <- limits[2]
    projection_names <- colnames(projection)
    #colnames(projection) <- c("Component.1", "Component.2")
    proj_gene <- data.frame(cbind(projection, gene_values))
    p <- ggplot(proj_gene, aes_string(projection_names[1], projection_names[2])) + geom_point(aes(colour = gene_values),
                                                                                              size = marker_size) +
        #scale_colour_gradient(low = "blue", high = "red", name = "log10") +
        scale_color_gradientn(colours = rev(rainbow(5)), name = "log10") + # Changed to rainbow
        labs(x = projection_names[1],
             y = projection_names[2]) + ggtitle("Total number of UMIs") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    return(p)
}


# Change the default color scale by overriding the function
#' @export
visualize_gene_counts <- function (gbm, projection, limits = c(0, 4), marker_size = 0.1)
{
    gene_values <- log(colSums(as.matrix(Biobase::exprs(gbm)) > 0), base = 10)
    gene_values[gene_values < limits[1]] <- limits[1]
    gene_values[gene_values > limits[2]] <- limits[2]
    projection_names <- colnames(projection)
    #colnames(projection) <- c("Component.1", "Component.2")
    proj_gene <- data.frame(cbind(projection, gene_values))
    p <- ggplot(proj_gene, aes_string(projection_names[1], projection_names[2])) + geom_point(aes(colour = gene_values),
                                                                                              size = marker_size) +
        #scale_colour_gradient(low = "blue", high = "red", name = "log10") +
        scale_color_gradientn(colours = rev(rainbow(5)), name = "log10") + # Changed to rainbow
        labs(x = projection_names[1],
             y = projection_names[2]) + ggtitle("Total number of Genes") +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    return(p)
}


#' @export
visualize_gene_expression <- function (gene_values, gene_probes, projection, limits = c(0, 10), marker_size = 0.1,
                                       title = NULL, ncol = NULL, title_size = 10, alpha=NULL, alpha_manual=NULL, binary = F, pal="rainbow2", na_col = "lightgrey")
{
    gene_values[gene_values < limits[1]] <- limits[1]
    gene_values[gene_values > limits[2]] <- limits[2]
    colnames(gene_values) <- gene_probes
    projection_names <- colnames(projection)
    colnames(projection) <- c("Component.1", "Component.2")
    if(!binary) {
        proj_gene <- data.frame(cbind(projection[c(1,2)], gene_values))
        proj_gene_melt <- melt(proj_gene, id.vars = c("Component.1",
                                                      "Component.2"))
        idx_region <- which(proj_gene_melt$value > 0)
        use_color <- get_numeric_color(pal)
        proj_gene_melt$alpha <- rep(alpha, length(gene_probes))
        #assign("proj_gene_melt", proj_gene_melt, env=.GlobalEnv)
        p <- ggplot(proj_gene_melt, aes_string(x="Component.1", y="Component.2", alpha="alpha")) +
            geom_point(size=marker_size,color=na_col,show.legend=FALSE, stroke=0) +
            geom_point(data=proj_gene_melt[idx_region,], aes_string(x="Component.1", y="Component.2", colour = "value"), size = marker_size, stroke=0)+
            scale_alpha_manual(values=alpha_manual) +
            guides(alpha=F)

        p <- p+facet_wrap(~variable, ncol=ncol) +
            scale_color_gradientn(colours = use_color) +
            labs(x = projection_names[1], y = projection_names[2])
        if (!is.null(title)) {
            p <- p + ggtitle(title)
        }
        p <- p + theme_bw() + theme(plot.title = element_text(hjust = 0.5), strip.text = element_text(size=title_size),
                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    } else {
        gene_values <- data.frame(gene_values, expr_stats=as.character(apply(gene_values, 1, function(x){
            expr_idx <- which(x > 0)
            if(length(expr_idx) == length(gene_probes)){
                paste(gene_probes, collapse = ", ")
            } else if(length(expr_idx)){
                gene_probes[which(x==max(x))][1]
            } else{
                "Not Expressed"
            }
        })))

        unique_vals <- as.character(unique(gene_values$expr_stats[which(gene_values$expr_stats != "Not Expressed")]))
        lev = c(paste(gene_probes, collapse = ", "), gene_probes, "Not Expressed")

        gene_values$expr_stats <- factor(gene_values$expr_stats, levels = lev)

        colors = c("firebrick", "dodgerblue3", "forestgreen", "orchid4", "#FF7F00", "gold", "#FFFF33", "#F781BF")
        use_color <- c(colors[1:(length(lev)-1)],na_col)
        names(use_color) <- c(lev[1:(length(lev)-1)], "Not Expressed")

        proj_gene <- data.frame(cbind(projection, gene_values))
        proj_gene$alpha <- alpha
        p = ggplot(proj_gene,  aes_string(x="Component.1", y="Component.2", alpha="alpha", color = "expr_stats")) +
            geom_point(size = marker_size+1, stroke=0) +
            scale_color_manual(values = use_color)

        p = p +
            scale_alpha_manual(values=alpha_manual) +
            guides(color = guide_legend(title = "Expression:", override.aes = list(size = 4)),
                   alpha = F) +
            monocle:::monocle_theme_opts() +
            theme(
                legend.position = c("top"))
    }
    return(p)
}

#' @export
visualize_expression_plotly <- function(expr, projection, ds, gene_probes, limits = c(0,2), marker_size=1, source = NULL, pal = NULL) {
    z_form <- if(length(ds) == 3) {as.formula(paste0("~", ds[3]))} else {NULL}
    if(is.null(gene_probes)) {
        plotly::plot_ly(projection,
                        x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])), z=z_form,
                        source = source,
                        key = row.names(projection),
                        marker = list(size = marker_size,color = "grey")) %>%
            plotly::add_markers() %>%
            layout(legend = list(orientation = 'h'))
    } else {
        gene_values <- expr
        gene_values[gene_values < limits[1]] <- limits[1]
        gene_values[gene_values > limits[2]] <- limits[2]
        colnames(gene_values) <- gene_probes
        if(length(gene_probes) == 1) {
            proj_gene <- data.frame(cbind(projection[,ds], gene_values))
            colnames(proj_gene) <- c(colnames(projection[,ds]), gene_probes)
            rgb_scale_list<- numeric_rgb_range(col = get_numeric_color(pal), zgrey=T)
            plotly::plot_ly(proj_gene,
                            x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])), z=z_form,
                            text=proj_gene[[gene_probes]],
                            hoverinfo="text",
                            source = source,
                            key = row.names(proj_gene),
                            marker = list(size = marker_size,
                                          color = as.formula(paste0("~`", gene_probes, "`")),
                                          colorscale=rgb_scale_list)) %>%
                plotly::add_markers()%>%
                layout(legend = list(orientation = 'h'))
        } else {
            if(length(gene_probes) > 5) {
                showNotification("Do not support more than 5 genes.", type="error", duration=10)
            }
            gene_values <- data.frame(gene_values, expr_stats=as.character(apply(gene_values, 1, function(x){
                expr_idx <- which(x > 0)
                if(length(expr_idx) == length(gene_probes)){
                    paste(gene_probes, collapse = ", ")
                } else if(length(expr_idx)){
                    gene_probes[which(x==max(x))][1]
                } else{
                    "Not Expressed"
                }
            })))
            unique_vals <- as.character(unique(gene_values$expr_stats[which(gene_values$expr_stats != "Not Expressed")]))
            lev = c(paste(gene_probes, collapse = ", "), gene_probes, "Not Expressed")

            gene_values$expr_stats <- factor(gene_values$expr_stats, levels = lev)

            colors = c("firebrick", "dodgerblue3", "forestgreen", "orchid4", "#FF7F00", "gold", "#FFFF33", "#F781BF")
            use_color <- c(colors[1:(length(lev)-1)],"grey60")
            names(use_color) <- c(lev[1:(length(lev)-1)], "Not Expressed")
            proj_gene <- data.frame(cbind(projection[,ds], gene_values))
            plotly::plot_ly(proj_gene,
                            x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])), z=z_form,
                            text=proj_gene$expr_stats,
                            hoverinfo="text",
                            marker = list(size = marker_size),
                            source = source,
                            key = row.names(proj_gene),
                            color = as.formula(paste0("~expr_stats")),
                            colors=use_color) %>%
                plotly::add_markers()%>%
                layout(legend = list(orientation = 'h'))
        }
    }
}

#' @export
gg.overlay <- function(df) {  # produces 2 color channels and the overlay
    require(ggplot2)
    require(gridExtra)
    gg.z1 <- ggplot(df, aes(x,y))+
        geom_tile(fill=rgb(red=df$z1.scale,green=0,blue=0))+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_fixed()

    gg.z2 <- ggplot(df, aes(x,y))+
        geom_tile(fill=rgb(red=0,green=df$z2.scale,blue=0))+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_fixed()

    gg <- ggplot(df, aes(x,y))+
        geom_tile(fill=rgb(red=df$z1.scale,green=df$z2.scale,blue=0))+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        coord_fixed()

    library(gridExtra)
    grid.arrange(gg.z1, gg.z2, gg, ncol=3)
}

# Allow customization of # cols
#' @export
visualize_gene_mixture <- function (gbm, gene_probes, projection, limits = c(0, 10), marker_size = 0.1,
                                    title = NULL, ncol = NULL, colorset = "Set1", alpha=0.8)
{
    gbm_trunc <- trunc_gbm_by_genes(gbm, gene_probes)
    gene_values <- t(as.matrix(Biobase::exprs(gbm_trunc)))
    gene_values[gene_values < limits[1]] <- limits[1]
    gene_values[gene_values > limits[2]] <- limits[2]
    colnames(gene_values) <- gene_probes
    projection_names <- colnames(projection)
    colnames(projection) <- c("Component.1", "Component.2")
    col_scale <- apply(gene_values,2, function(x){(x - min(x)) / (max(x) - min(x))})
    proj_gene <- data.frame(cbind(projection, col_scale))
    proj_gene$alpha <- ifelse(rowSums(col_scale) == 0, alpha, 1)
    if(length(gene_probes) == 1) {
        p <- ggplot(proj_gene, aes(Component.1, Component.2, alpha = alpha)) +
            geom_point(color = rgb(red=col_scale[,1],green=0,blue=0), size = marker_size)
    } else if(length(gene_probes) == 2) {
        p <- ggplot(proj_gene, aes(Component.1, Component.2, alpha = alpha)) +
            geom_point(color = rgb(red=col_scale[,1],green=col_scale[,2],blue=0), size = marker_size)
    } else if(length(gene_probes) == 3) {
        p <- ggplot(proj_gene, aes(Component.1, Component.2, alpha = alpha)) +
            geom_point(color = rgb(red=col_scale[,1],green=col_scale[,2],blue=col_scale[,3]), size = marker_size)
    } else {
        return(NULL)
    }

    if (!is.null(title)) {
        p <- p + ggtitle(title)
    }
    p <- p + theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                                panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        guides(alpha=FALSE)
    return(p)
}

#' @export
plotProj <- function (proj, dim_col = c(1,2), group.by=NULL, pal=NULL, size = 1, plot_title=NULL, legend_title = NULL, na_col = "lightgrey", alpha=NULL, alpha_level=0.1, legend=T, trans = "identity", onplotText=F, onplotTextSize = 2,  legend.size = 4, legend.position = "top", legend.title = waiver(), keywidth=0.1, keyheight=0.1, ncol = NULL, nudge_x = 0, nudge_y = 0,...) {
    plot_col <- colnames(proj)[dim_col]
    if(!is.null(alpha)) {
        proj$alpha <- alpha
    } else {
        proj$alpha <- rep("f", length(nrow(proj)))
    }
    alpha_manual <- c("f"=1,"t"=alpha_level)

    pp<-ggplot(proj, aes_string(plot_col[1],plot_col[2])) +
        geom_point(aes_string(color=group.by, alpha="alpha"), size=size, stroke = 0) +
        scale_alpha_manual(values=alpha_manual) +
        theme_bw() +
        ggtitle(plot_title) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = legend.position)
    if(onplotText) {
        pp<- pp + geom_label(aes_string(
            label = group.by
            #color = group.by
        ),
        size = onplotTextSize,
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        data = proj %>% group_by_at(group.by) %>% summarize_at(plot_col, median))
    }
    if(legend) {
        pp <- pp + guides(alpha=F, color = guide_legend(override.aes = list(size=legend.size), title = legend.title, keywidth=keywidth, keyheight=keyheight, ncol = ncol))
    } else {
        pp <- pp + guides(alpha=F, color = F)
    }
    if(!is.null(names(pal))) {
        pp<-pp + scale_color_manual(values = pal, na.value=na_col)
    } else {
        if(!is.null(pal)) {
            if(is.factor(proj[[group.by]]) || is.character(proj[[group.by]])) {
                pp<-pp + scale_color_manual(values = get_color_vector(unique(na.omit(proj[[group.by]])), pal=pal, ...), na.value=na_col)
            } else {
                pp<-pp +scale_colour_gradientn(colours=get_numeric_color(pal), trans=trans) + guides(color = guide_colorbar(barwidth = 10, barheight = 1))
            }
        }
    }
    return(pp)
}


#' @export
find_conditonal_idx <- function(idx, which_list) {
    shared_which<-Reduce(intersect, which_list[vapply(which_list, Negate(function(x) {length(x)==0}), NA)])
    idx[shared_which]
}


#' @export
feature_plot <- function(df, selected_gene, plot_by = "sample", meta = NULL, palette = "Set1", style = "box", log_scale = F, legend_pos = "top", textSize = 15, pointSize = 3){
    if(is.null(df) || nrow(df) == 0 || is.null(meta) || is.null(palette)) {
        return()
    }
    colnames(df) <- "expression_level"
    df <- cbind(df, meta)
    g1 <- ggplot(df, aes_string(x=plot_by, y="expression_level"))
    if(style == "bar") {
        g1 <- ggplot(df, aes_string(x="sample", y="expression_level"))
        g1 <- g1 + geom_bar(stat = "identity", aes_string(fill = plot_by))
    } else if(style %in% c("points", "box", "violin")) {
        g1 <- g1 + geom_point(position=position_jitter(w=0.1,h=0), size = pointSize, aes_string(colour = plot_by, group = plot_by))
        if(style == "box") {
            g1 <- g1 + geom_boxplot(aes_string(fill = plot_by, alpha = 0.2))
        } else if(style == "violin") {
            g1 <- g1 + geom_violin(aes_string(fill = plot_by, alpha = 0.2), trim = F)
        }
    }
    col_man <- get_color_vector(unique(df[[plot_by]]), pal = palette)
    names(col_man) <- unique(df[[plot_by]])
    g1 <- g1 + scale_color_manual(values = col_man) +
        scale_fill_manual(values = col_man) +
        ggtitle(paste0("Expression level of gene ", selected_gene))  +
        theme(text = element_text(size=textSize), legend.position=legend_pos, plot.title = element_text(hjust = 0.5)) +
        guides(alpha = F, fill=F)
    if(log_scale) {
        g1 <- g1 + scale_y_log10(breaks=c(25,100,400))
    }

    return(g1 + theme_minimal())
}

#' @export
plotGraph <- function(g, color.by=NULL, pal=NULL, label=NULL, alpha = NULL, type = NULL, background="grey20", node.text.size = 1) {
    p1<-ggraph(g,layout = 'partition', circular = TRUE) +
        geom_node_arc_bar(aes_string(fill = color.by, alpha=alpha)) +
        theme_graph(background = background, text_colour = 'white') +
        geom_node_text(aes_string(label = label), colour = 'white', vjust = 0.4, size=node.text.size) +
        theme(legend.position = 'top') +
        guides(alpha=F)
    if(type == "numeric") {
        p1<-p1+scale_fill_gradientn(colours = get_numeric_color(pal), na.value="grey20")
    } else{
        unique_factor <- unique(as.data.frame(g)[[color.by]])
        p1<-p1+scale_fill_manual(values = pal, na.value="grey20")
    }
    return(p1)
}


