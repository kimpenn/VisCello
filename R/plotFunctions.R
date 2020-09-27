
#' @export
factor_color_opt <- function() {
    allowed_pals <- c("Set1", "Set2", "Paired", "Dark2", "Accent")
    return(allowed_pals)
}

#' @export
get_factor_color <-function (labels, pal = "Set1", maxCol = 9, nogrey = T)
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
numeric_bin_color_opt <- function() {
    allowed_pals <- c('gg_color_hue', 'rainbow', 'RdYlBu', 'RdBu', 'viridis', 'magma', 'plasma', 'inferno')
    return(allowed_pals)
}

#' @export
gg_color_hue2 <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

#' @export
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)

#' @export
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

#' @export
get_numeric_bin_color <-function (bins, palette = "RdYlBu", maxCol = 9)
{
    allowed_pals <- numeric_bin_color_opt()
    if(!palette %in% allowed_pals) stop(paste0("Please specify one of '", paste(allowed_pals, collapse = "', '"), "'."))
    
    unq <- unique(bins)
    if(palette %in% get_brewer_set(c("sequential", "diverging"))) {
        colorRampPalette(rev(RColorBrewer::brewer.pal(maxCol, palette)))(length(unq))
    } else if(palette %in% list("viridis" = "viridis", "magma" = "magma", "plasma" = "plasma", "inferno" = "inferno")) {
        viridis(n = length(unq), option = palette)
    } else if(palette == "rainbow") {
        colorRampPalette(rev(rainbow(10)))(length(unq))
    } else if(palette == "gg_color_hue") {
        gg_color_hue2(length(unq))
    }
}

#' @export
numeric_color_opt <- function() {
    allowed_pals <- c("BlueGreenRed", 'RdYlBu', 'RdBu','RdOgYl', 'rainbow2', 'viridis', 'magma', 'plasma', 'inferno', 'rainbow', 'gg_color_hue', 'grey&red')
    return(allowed_pals)
}

#' @export
get_numeric_color <- function(palette = NULL) {
    if(is.null(palette)) stop("please specify a palette")
    allowed_pals <- numeric_color_opt()
    if(!palette %in% allowed_pals) stop(paste0("Please specify one of '", paste(allowed_pals, collapse = "', '"), "'."))
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
        c("grey", "#b2182b")
    } else if(palette == "RdOgYl") {
        c("grey85", "red", "orange", "yellow")
    } else if(palette == "gg_color_hue") {
        gg_color_hue2(10)
    } else if(palette == "blue_green_gold"){
        c("grey85", "blue", "green", "#FFD200", "gold")
    } else if(palette == "black_red_gold"){
        c("grey85", "black", "red", "#FFD200")
    } else if(palette == "black_red") {
        c("grey85", "black", "red")
    } else if(palette == "red_yellow") {
        c("grey85",  "red", "yellow")
    } else if(palette == "black_yellow") {
        c("grey85",  "black", "yellow")
    } else if(palette == "black_yellow_gold") {
        c("grey85",  "black", "yellow", "gold")
    } else if(palette == "BlueGreenRed") {
        colorRampPalette(c("midnightblue", "dodgerblue", "seagreen", "#00C000", "gold2", "darkorange1", "red1"))(10)
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


#' @export
visualize_gene_expression <- function (gene_values, gene_probes, projection, limits = c(0, 10), marker_size = 0.1,
                                       title = NULL, ncol = NULL, title_size = 10, alpha=NULL, alpha_manual=NULL, binary = F, pal="rainbow2", na.col = "lightgrey", legend = T, legend_name = "Expression")
{
    colnames(gene_values) <- gene_probes
    projection_names <- colnames(projection)
    #colnames(projection) <- c("Component.1", "Component.2")
    if(!binary) {
        gene_values[gene_values < limits[1]] <- limits[1]
        gene_values[gene_values > limits[2]] <- limits[2]
        proj_gene <- data.frame(cbind(projection[c(1,2)], gene_values))
        proj_gene_melt <- melt(proj_gene, id.vars = colnames(projection))
        idx_region <- which(proj_gene_melt$value > 0)
        use_color <- get_numeric_color(pal)
        proj_gene_melt$alpha <- rep(alpha, length(gene_probes))
        #assign("proj_gene_melt", proj_gene_melt, env=.GlobalEnv)
        p <- ggplot(proj_gene_melt, aes_string(x=colnames(projection)[1], y=colnames(projection)[2], alpha="alpha")) +
            geom_point(size=marker_size,color=na.col,show.legend=FALSE, stroke=0) +
            geom_point(data=proj_gene_melt[idx_region,], aes_string(x=colnames(projection)[1], y=colnames(projection)[2], colour = "value"), size = marker_size, stroke=0)+
            scale_alpha_manual(values=alpha_manual) +
            guides(alpha=F)

        p <- p +
            #facet_wrap(~variable, ncol=ncol) +
            scale_color_gradientn(colours = use_color) +
            labs(x = projection_names[1], y = projection_names[2])
        if (!is.null(title)) {
            p <- p + ggtitle(title)
        }
        if(legend) {
            p <- p + 
                guides(colour = guide_colorbar(title = legend_name))
        } else {
            p <- p + guides(colour = F)
        }
        p <- p + theme_bw() + 
            theme(legend.position = c("top"), 
                  plot.title = element_text(hjust = 0.5), 
                  strip.text = element_text(size=title_size), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())
    } else {
        if(ncol(gene_values) > 2) return()
        gene_values <- gene_values > 0
        gene_values <- data.frame(
            expr_stats = apply(gene_values, 1, function(x){
                x_max <-max(x)
                if(x_max == 0) return("Not Expressed") else paste0(gene_probes[which(x == x_max)], collapse = ", ")
            })
        )

        unique_vals <- as.character(unique(gene_values$expr_stats[which(gene_values$expr_stats != "Not Expressed")]))
        lev = unique(c(paste(gene_probes, collapse = ", "), gene_probes, "Not Expressed"))
        gene_values$expr_stats <- factor(gene_values$expr_stats, levels = lev)

        colors = c("firebrick", "dodgerblue3", "forestgreen", "orchid4", "#FF7F00", "gold", "#FFFF33", "#F781BF")
        use_color <- c(colors[1:(length(lev)-1)],na.col)
        names(use_color) <- c(lev[1:(length(lev)-1)], "Not Expressed")

        proj_gene <- data.frame(cbind(projection, gene_values))
        proj_gene$alpha <- alpha
        idx_region <- which(proj_gene$expr_stats != "Not Expressed")
        p = ggplot(proj_gene,  aes_string(x=colnames(projection)[1], y=colnames(projection)[2], alpha="alpha")) +
            geom_point(size=marker_size,color=na.col,show.legend=FALSE, stroke=0) +
            geom_point(data=proj_gene[idx_region,], aes_string(x=colnames(projection)[1], y=colnames(projection)[2], colour = "expr_stats"), size = marker_size, stroke=0)+
            scale_color_manual(values = use_color)

        p = p +
            scale_alpha_manual(values=alpha_manual) +
            monocle:::monocle_theme_opts() +
            theme(legend.position = c("top"))
        if(legend) {
            p <- p+ guides(color = guide_legend(title = "Expression:", override.aes = list(size = 4)),
                                      alpha = F)
        } else {
            p <- p + guides(color = F, alpha = F)
        }
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
        colnames(gene_values) <- gene_probes

        if(length(gene_probes) == 1) {
            show_value <- round(gene_values[,1],3)
            gene_values[gene_values < limits[1]] <- limits[1]
            gene_values[gene_values > limits[2]] <- limits[2]
            
            proj_gene <- data.frame(cbind(projection[,ds], gene_values))
            colnames(proj_gene) <- c(colnames(projection[,ds]), gene_probes)
            proj_gene$show_value <- show_value
            rgb_scale_list<- numeric_rgb_range(col = get_numeric_color(pal), zgrey=T)
            plotly::plot_ly(proj_gene,
                            x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])), z=z_form,
                            text=proj_gene$show_value,
                            hoverinfo="text",
                            source = source,
                            key = row.names(proj_gene),
                            marker = list(size = marker_size,
                                          color = as.formula(paste0("~`", gene_probes, "`")),
                                          colorscale=rgb_scale_list)) %>%
                plotly::add_markers()%>%
                layout(legend = list(orientation = 'h'))
        } else {
            if(ncol(gene_values) > 2) return()
            gene_values <- gene_values > 0
            gene_values <- data.frame(
                expr_stats = apply(gene_values, 1, function(x){
                    x_max <-max(x)
                    if(x_max == 0) return("Not Expressed") else paste0(gene_probes[which(x == x_max)], collapse = ", ")
                })
            )
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



#' @export
plotProj <- function (proj, dim_col = c(1,2), group.by=NULL, pal=NULL, size = 1, plot_title=NULL, na.col = "lightgrey", alpha=NULL, alpha_level=0.1, legend=T, trans = "identity", onplotAnnot=NULL, onplotAnnotSize = 2,  legend.size = 4, legend.text.size = 3, legend.position = "top", legend.title = waiver(), keywidth=0.1, keyheight=0.1, ncol = NULL, nudge_x = 0, nudge_y = 0, limits = NULL, breaks = waiver(), cover0 = F, ...) {
    plot_col <- colnames(proj)[dim_col]
    if(!is.null(alpha)) {
        proj$alpha <- alpha
    } else {
        proj$alpha <- rep("f", length(nrow(proj)))
    }
    alpha_manual <- c("f"=1,"t"=alpha_level)
    if(!is.null(limits)) {
        proj[[group.by]][proj[[group.by]] < limits[1]] <- limits[1]
        proj[[group.by]][proj[[group.by]] > limits[2]] <- limits[2]
    }
    pp<-ggplot(proj, aes_string(color=group.by)) +
        scale_alpha_manual(values=alpha_manual) +
        theme_bw() +
        ggtitle(plot_title) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = legend.position)
    
    if(cover0) {
        is_layer2 <- proj[[group.by]] == "unannotated" | is.na(proj[[group.by]]) | proj[[group.by]] == 0
        idx_region <- which(!is_layer2)
        pp<- pp + geom_point(aes_string(plot_col[1],plot_col[2], alpha="alpha"), size=size,color=na.col,show.legend=FALSE, stroke=0) +
            geom_point(data=proj[idx_region,],aes_string(plot_col[1],plot_col[2], alpha="alpha"), size=size, stroke = 0)
    } else {
        pp<- pp + geom_point(aes_string(plot_col[1],plot_col[2], alpha="alpha"), size=size, stroke = 0)
    }

    if(!is.null(onplotAnnot)) {
        label_data <- proj %>% group_by_at(group.by) %>% summarize_at(plot_col, median)
        if(length(breaks) > 0) {
            label_data <- label_data[label_data[[group.by]] %in% breaks,,drop=F]
        }
        if(onplotAnnot == "text") {
            pp<- pp + geom_text(
                aes_string(
                    x = plot_col[1], y= plot_col[2], 
                    label = group.by
                ),
                size = onplotAnnotSize,
                nudge_x = nudge_x,
                nudge_y = nudge_y,
                data = label_data
            )
        } else {
            pp<- pp + geom_label(
                aes_string(
                    x=plot_col[1],y=plot_col[2], 
                    label = group.by
                ),
                color = "black",
                size = onplotAnnotSize,
                nudge_x = nudge_x,
                nudge_y = nudge_y,
                data = label_data
            )
        }
    }
    if(legend) {
        pp <- pp + guides(alpha=F, color = guide_legend(override.aes = list(size=legend.size),
                                                        title.theme = element_text(size = legend.text.size*1.2),
                                                        label.theme = element_text(size = legend.text.size),
                                                        title = legend.title, 
                                                        keywidth=keywidth, 
                                                        keyheight=keyheight, ncol = ncol))
    } else {
        pp <- pp + guides(alpha=F, color = F)
    }
    if(!is.null(names(pal))) {
        pp<-pp + scale_color_manual(values = pal, na.value=na.col, breaks = breaks)
    } else {
        if(!is.null(pal)) {
            if(is.factor(proj[[group.by]]) || is.character(proj[[group.by]])) {
                pp<-pp + scale_color_manual(values = get_factor_color(unique(na.omit(proj[[group.by]])), pal=pal, ...), na.value=na.col, breaks = breaks)
            } else {
                pp<-pp + scale_colour_gradientn(colours=get_numeric_color(pal), trans=trans)
                if(legend) {
                    pp<-pp + guides(color = guide_colorbar(barwidth = 10, barheight = 1))
                }
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
feature_plot <- function(df, selected_gene, group.by = "sample", meta = NULL, pal = "Set1", style = "box", log_scale = F, legend = T, legend.title = waiver(), legend.pos = "top", legend.point.size=4, text.size = 15, pointSize = 3, na.col = "lightgrey", breaks = waiver(),  axis.text.angle = 90, order.by = "none", ylab.label = "expression"){
    if(is.null(df) || nrow(df) == 0 || is.null(meta) || is.null(palette)) {
        return()
    }
    colnames(df) <- "expression_level"
    df <- cbind(df, meta)

    if(order.by == "mean") {
        group_mean <- df %>% dplyr::group_by_at(group.by) %>% dplyr::summarize(mean = mean(expression_level))
        group_order <- group_mean[[group.by]][order(group_mean$mean, decreasing = T)]
        df[[group.by]] <- factor(as.character(df[[group.by]]), levels = group_order, ordered=T)
    }
    
    g1 <- ggplot(df, aes_string(x=group.by, y="expression_level"))
    
    g1 <- g1 + geom_point(position=position_jitter(w=0.1,h=0), size = pointSize, aes_string(colour = group.by, group = group.by))
    if(style == "box") {
        g1 <- g1 + geom_boxplot(aes_string(fill = group.by, alpha = 0.2))
    } else if(style == "violin") {
        g1 <- g1 + geom_violin(aes_string(fill = group.by, alpha = 0.2), trim = F)
    }
    
    if(!is.null(names(pal))) {
        g1<-g1 + 
            scale_color_manual(values = pal, na.value=na.col, breaks = breaks) +
            scale_fill_manual(values = pal, na.value=na.col, breaks = breaks)
    } else {
        if(!is.null(pal)) {
            if(is.factor(df[[group.by]]) || is.character(df[[group.by]])) {
                pals <- get_factor_color(unique(na.omit(df[[group.by]])), pal=pal)
                g1<-g1 + 
                    scale_color_manual(values = pals, na.value=na.col, breaks = breaks) + 
                    scale_fill_manual(values = pals, na.value=na.col, breaks = breaks)
            } 
        }
    } 
    
    g1 <- g1 + 
        ggtitle(paste0("Expression level of gene ", selected_gene)) +
        xlab(legend.title) +
        ylab(ylab.label) + 
        theme(text = element_text(size=text.size), axis.text.x = element_text(angle=axis.text.angle, hjust=1), legend.position=legend.pos, plot.title = element_text(hjust = 0.5)) 
    if(legend) {
        g1 <- g1 + guides(alpha = F, fill=F, color = guide_legend(override.aes = list(size=legend.point.size), title = legend.title))
    } else {
        g1 <- g1 + guides(alpha = F, fill=F, color= F)
    }
    if(log_scale) {
        g1 <- g1 + scale_y_log10(breaks=c(25,100,400))
    }

    return(g1 + monocle:::monocle_theme_opts())
}

#' @export
plotGraph <- function(g, color.by=NULL, pal=NULL, label=NULL, alpha = NULL, type = NULL, background="grey20", border.size = 0.1, node.text.size = 1, legend.title = waiver()) {
    p1<-ggraph(g,layout = 'partition', circular = TRUE) +
        geom_node_arc_bar(aes_string(fill = color.by, alpha=alpha), size = border.size) +
        theme_graph(background = background, text_colour = 'white') +
        theme(legend.position = 'top') +
        guides(alpha=F, size = F, fill = guide_colorbar(title = legend.title))
    if(!is.null(label)) {
        p1 <- p1 +geom_node_text(aes_string(label = label, size="text.size"), colour = 'white', vjust = 0.4) 
    }
    
    if(type == "numeric") {
        p1<-p1+scale_fill_gradientn(colours = get_numeric_color(pal), na.value="grey20")
    } else{
        unique_factor <- unique(as.data.frame(g)[[color.by]])
        p1<-p1+scale_fill_manual(values = pal, na.value="grey20")
    }
    return(p1)
}



