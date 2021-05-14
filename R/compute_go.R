


#' @export
compute_go <- function(de_list, bg_list, type = "BP", organism = c("mmu", "cel", "hsa"), idcol = "gene_name") {
    if(organism == "cel") {
        orgdb <- "celegans.db"
        fromType = "SYMBOL"
        idcol <- idcol
    } else if(organism =="mmu") {
        orgdb <- "org.Mm.eg.db"
        fromType = "SYMBOL"
        idcol <- idcol
    } else if(organism == "hsa") {
        orgdb <- "org.Hs.eg.db"
        fromType = "SYMBOL"
        idcol <- idcol
    }

    bg.df <- bitr(bg_list, fromType = fromType,
                  toType = c("SYMBOL", "ENTREZID"),
                  OrgDb = orgdb)

    gene_list <- lapply(1:length(de_list), function(i){
        if(nrow(de_list[[i]]) == 0) return(NULL)
        tbl<-de_list[[i]]
        gene.df <- bitr(de_list[[i]][[idcol]], fromType = fromType,
                        toType = c("SYMBOL", "ENTREZID"),
                        OrgDb = orgdb)
        base::merge(x = tbl, gene.df, by.x = idcol, by.y = "SYMBOL")
    })

    names(gene_list) <- names(de_list)
    if(type == "kegg") {
        kegg_list <-   lapply(1:length(gene_list), function(i){
            if(is.null(gene_list[[i]])) return(NULL)
            if(organism == "cel") {
                kegg_gene <- paste0("CELE_", toupper(gene_list[[i]]$gene_name))
                kegg_bg <- paste0("CELE_", toupper(bg.df$SYMBOL))
            }  else if(organism %in% c("mmu", "hsa")) {
                kegg_gene <- gene_list[[i]]$ENTREZID
                kegg_bg <- bg.df$ENTREZID
            }
            enrichKEGG(
                gene          = kegg_gene,
                universe      = kegg_bg,
                organism      = organism,
                pAdjustMethod = "BH")
        })
        enrich_list <- lapply(1:length(gene_list), function(i){
            if(is.null(kegg_list[[i]])) return(NULL)
            kegg_list[[i]]@result
        })
        if(organism != "cel") {
        for(i in 1:length(gene_list)){
            if(is.null(enrich_list[[i]])) return(NULL)
            enrich_list[[i]]$geneID <- unlist(lapply(enrich_list[[i]]$geneID, function(s){
                tbl <- bitr(geneID=unlist(strsplit(s, split="/")), fromType="ENTREZID", toType="SYMBOL", OrgDb = orgdb)
                paste0(tbl$SYMBOL, collapse = "/")
            }))
        }
        }
    } else {
        print(1)
        ego_bplist <- lapply(1:length(gene_list), function(i){
            if(is.null(gene_list[[i]])) return(NULL)
            enrichGO(gene        = gene_list[[i]]$ENTREZID,
                     universe      = bg.df$ENTREZID,
                     OrgDb         = orgdb,
                     ont           = type,
                     pAdjustMethod = "BH",
                     pvalueCutoff  = 0.01,
                     qvalueCutoff  = 0.05,
                     readable      = TRUE)
        })
        print(2)
        enrich_list <- lapply(1:length(gene_list), function(i){
            if(is.null(ego_bplist[[i]])) return(NULL)
            ego_bplist[[i]]@result
        })
        print(3)
    }
    names(enrich_list) <- names(de_list)
    return(enrich_list)
}




#' export
mouse_to_human_symbol <- function(gene_symbol, in.type = c("mm", "hs"), HMD_HumanPhenotype = NULL) {
    hs_symbol <- HMD_HumanPhenotype$V1
    mm_symbol <- HMD_HumanPhenotype$V5
    
    if(in.type == "mm") {
        return(hs_symbol[match(gene_symbol, mm_symbol)])
    } else if(in.type == "hs"){
        return(mm_symbol[match(gene_symbol, hs_symbol)])
    }
}







