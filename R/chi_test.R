


#' @export
run_chisq <- function(dat, group = NULL, min_fdr = 0.01, min_lfc = 2, fdata = NULL, detRate=0.01, comp_prop = T, id_col = "gene_id", name_col = "gene_name") {
    group <- factor(group, unique(group))
    expressed_count<-apply(dat, 1, function(row){sum(row>0)})
    expressed_prop <- expressed_count /ncol(dat)
    keep_idx <- which(expressed_prop >= detRate) 
    dat <- dat[keep_idx,]
    idx1 <- which(group == levels(group)[1])
    idx2 <- which(group == levels(group)[2])
    chi_res<-as.data.frame(t(apply(as.matrix(dat), 1, function(g){
        g1_nonzero <- sum(g[idx1] > 0)
        g1_zero <- length(idx1) - g1_nonzero
        g2_nonzero <- sum(g[idx2] > 0)
        g2_zero <- length(idx2) - g2_nonzero
        test_mtx<-matrix(c(g1_nonzero, g1_zero, g2_nonzero, g2_zero), nrow=2)
        suppressWarnings(res <- chisq.test(test_mtx))
        return(c(proportion1 = g1_nonzero/length(idx1), proportion2 = g2_nonzero/length(idx2), statistic = round(res$statistic,3), p.value = res$p.value))
    })))
    assign("chi_res", chi_res, env=.GlobalEnv)
    chi_res$FDR <- p.adjust(chi_res$p.value, 'fdr')
    gdata <- fdata[, c(id_col, name_col)][match(rownames(chi_res), rownames(fdata)),]
    chi_res <- cbind(gdata,chi_res)
    chi_res$LFC_abs <- abs(log2(chi_res$proportion1/chi_res$proportion2))
    chi_res$significant = chi_res$FDR <= min_fdr & chi_res$LFC_abs >= min_lfc
    chi_res <- chi_res[order(chi_res$FDR),]
    # chi_res <- chi_res %>% dplyr::filter(FDR < fdr) %>% dplyr::arrange(FDR) %>% 
    #     dplyr::mutate_each(dplyr::funs(round(.,3)), proportion1, proportion2, p.value, FDR)
    chi_res1 <- chi_res %>% dplyr::filter(proportion1>proportion2)
    chi_res2 <- chi_res %>% dplyr::filter(proportion2>proportion1)
    chi_de <- list(chi_res1, chi_res2)
    names(chi_de) <- levels(group)
    return(chi_de)
}










