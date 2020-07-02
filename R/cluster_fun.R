

#' @export
louvain_clus <- function (data, k = 20, weight = F, louvain_iter = 1, resolution = NULL, random_seed = 0L, verbose = F, ...) 
{
    extra_arguments <- list(...)
    cell_names <- row.names(data)
    if (is.data.frame(data)) 
        data <- as.matrix(data)
    if (!is.matrix(data)) 
        stop("Wrong input data, should be a data frame of matrix!")
    if (k < 1) {
        stop("k must be a positive integer!")
    }
    else if (k > nrow(data) - 2) {
        stop("RANN counts the point itself, k must be smaller than\nthe total number of points - 1 (all other points) - 1 (itself)!")
    }
    if (verbose) {
        message("Run kNN based graph clustering starts:", "\n", 
                "  -Input data of ", nrow(data), " rows and ", ncol(data), 
                " columns", "\n", "  -k is set to ", k)
    }
    if (verbose) {
        cat("  Finding nearest neighbors...")
    }
    t1 <- system.time(tmp <- RANN::nn2(data, data, k + 1, searchtype = "standard"))
    neighborMatrix <- tmp[[1]][, -1]
    distMatrix <- tmp[[2]][, -1]
    if (verbose) {
        cat("DONE ~", t1[3], "s\n", " Compute jaccard coefficient between nearest-neighbor sets ...")
    }
    t2 <- system.time(links <- monocle:::jaccard_coeff(neighborMatrix, 
                                                       weight))
    if (verbose) {
        cat("DONE ~", t2[3], "s\n", " Build undirected graph from the weighted links ...")
    }
    links <- links[links[, 1] > 0, ]
    relations <- as.data.frame(links)
    colnames(relations) <- c("from", "to", "weight")
    relations$from <- cell_names[relations$from]
    relations$to <- cell_names[relations$to]
    t3 <- system.time(g <- igraph::graph.data.frame(relations, 
                                                    directed = FALSE))
    if (verbose) {
        cat("DONE ~", t3[3], "s\n", " Run louvain clustering on the graph ...\n")
    }
    t_start <- Sys.time()
    Qp <- -1
    optim_res <- NULL
    best_max_resolution <- "No resolution"
    if (louvain_iter >= 2) {
        random_seed <- NULL
    }
    for (iter in 1:louvain_iter) {
        if (verbose) {
            cat("Running louvain iteration ", iter, "...\n")
        }
        if (!is.null(resolution)) {
            for (i in 1:length(resolution)) {
                cur_resolution <- resolution[i]
                louvain_args <- c(list(X = igraph::get.adjacency(g), 
                                       res = as.numeric(cur_resolution), random_seed = random_seed, 
                                       verbose = verbose), extra_arguments[names(extra_arguments) %in% c("python_home", "partition_method", "initial_membership", "weights", "node_sizes", "return_all", "louvain_path")])
                Q <- do.call(louvain_R, louvain_args)
                Qt <- max(Q$modularity)
                if (verbose) {
                    message("Current iteration is ", iter, "; current resolution is ", 
                            cur_resolution, "; Modularity is ", Qt, "; Number of clusters are ", 
                            max(Q$membership))
                }
                if (Qt > Qp) {
                    optim_res <- Q
                    Qp <- Qt
                    best_max_resolution <- cur_resolution
                }
            }
        }
        else {
            Q <- igraph::cluster_louvain(g)
        }
        if (is.null(optim_res)) {
            Qp <- max(Q$modularity)
            optim_res <- Q
        }
        else {
            Qt <- max(Q$modularity)
            if (Qt > Qp) {
                optim_res <- Q
                Qp <- Qt
            }
        }
    }
    if (verbose) 
        message("Maximal modularity is ", Qp, "; corresponding resolution is ", 
                best_max_resolution)
    t_end <- Sys.time()
    if (verbose) {
        message("\nRun kNN based graph clustering DONE, totally takes ", 
                t_end - t_start, " s.")
        cat("  -Number of clusters:", length(unique(igraph::membership(optim_res))), 
            "\n")
    }
    if (igraph::vcount(g) < 3000) {
        coord <- NULL
        edge_links <- NULL
    }
    else {
        coord <- NULL
        edge_links <- NULL
    }
    V(g)$names <- as.character(V(g))
    #return(list(g = g, relations = relations, distMatrix = distMatrix, 
    #            coord = coord, edge_links = edge_links, optim_res = optim_res))
    return(factor(igraph::membership(optim_res)))
}

#' @export
dens_clus<-function(tsne_data, rho_threshold=5, delta_threshold=10, peaks=NULL){
    set.seed(1)
    dataDist <- dist(tsne_data)
    dataClust <- densityClust::densityClust(dataDist, gaussian = T)
    dataClust <- densityClust::findClusters(dataClust,
                                            rho = rho_threshold, delta = delta_threshold,
                                            peaks = peaks)
    return(factor(dataClust$clusters))
}




