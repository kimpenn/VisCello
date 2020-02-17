



#' @export
run_mw <- function(dat, group = NULL, min_fdr = 0.01, fdata = NULL,  detRate=0.01, id_col = "gene_id", name_col = "gene_name") {
    expressed_count<-apply(dat, 1, function(row){sum(row>0)})
    expressed_prop <- expressed_count /ncol(dat)
    keep_idx <- which(expressed_prop >= detRate) 
    dat <- dat[keep_idx,]
    group <- factor(group, unique(group))
    idx1 <- which(group == levels(group)[1])
    idx2 <- which(group == levels(group)[2])
    mw_res<-as.data.frame(t(sapply(1:nrow(dat), function(i){
        message(i)
        g <- dat[i,]
        g1_nonzero <- sum(g[idx1] > 0)
        g1_zero <- length(idx1) - g1_nonzero
        g2_nonzero <- sum(g[idx2] > 0)
        g2_zero <- length(idx2) - g2_nonzero
        suppressWarnings(res <- wilcox.test(g[idx1], g[idx2], alternative = 'two.sided' , paired = FALSE, conf.int = T , conf.level = 0.95 ))
        return( c ("statistic" = res$statistic [[ 1 ]] , "p.value" = res$p.value [[ 1 ]], "proportion1" = g1_nonzero/length(idx1), "proportion2" = g2_nonzero/length(idx2),
                   "effectSize" = as.numeric(res$estimate) , "effectSize.CI.lower" = res$conf.int [ 1 ] , "effectSize.CI.upper" = res$conf.int [ 2 ] ))
    })))
    rownames(mw_res) <- rownames(dat)
    mw_res$FDR <- p.adjust(mw_res$p.value, 'fdr')
    gdata <- fdata[, c(id_col, name_col)][match(rownames(mw_res), rownames(fdata)),]
    mw_res <- cbind(gdata,mw_res)
    mw_res <- mw_res[order(mw_res$FDR),]
    mw_res$significant = mw_res$FDR <= min_fdr
    mw_res1 <- mw_res %>% dplyr::filter(effectSize > 0)
    mw_res2 <- mw_res %>% dplyr::filter(effectSize < 0)
    mw_de <- list(mw_res1, mw_res2)
    names(mw_de) <- levels(group)
    return(mw_de)
}



