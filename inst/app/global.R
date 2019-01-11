

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

mainTitle = "Endothelial to Hematopoietic Transition"


cello <- readRDS("data/cello.rds")
clist <- readRDS("data/clist.rds")


meta_order <- c(
    "Dataset" = "Dataset", 
    "Combined Dataset" = "Combine_Dataset", 
    "Gene expression" = "gene.expr",
    "Total_mRNAs" = "Total_mRNAs",
    "num_genes_expressed" = "num_genes_expressed",
    "Size_Factor" = "Size_Factor",
    "Cluster" = "Cluster"
)

pmeta_attr <- data.frame(meta_id = meta_order, meta_name = names(meta_order), stringsAsFactors=FALSE)

pmeta_attr$is_numeric <- sapply(as.character(pmeta_attr$meta_id), function(x) {
    if(x %in% colnames(pData(cello))) {
        is.numeric(pData(cello)[[x]])
    } else if(x %in% colnames(clist[[1]]@pmeta)) {
        is.numeric(clist[[1]]@pmeta[[x]])
    } else if(x == "gene.expr") {
        T
    } else {
        NA
    }
})


# Which meta to show 
pmeta_attr$dpal <- ifelse(pmeta_attr$is_numeric, "RdBu", "Set1")
pmeta_attr$dscale <- ifelse(pmeta_attr$is_numeric, "log10", NA)
pmeta_attr$dscale[which(pmeta_attr$meta_id %in% c("Size_Factor"))] <- "identity"
pData(cello) <- pData(cello)[,which(colnames(pData(cello)) %in% pmeta_attr$meta_id)]


### Which meta data to show, and in what order ###
showcols_meta <- pmeta_attr$meta_id
names(showcols_meta) <- pmeta_attr$meta_name

bp_colorBy_choices <- showcols_meta[!pmeta_attr$is_numeric]

de_meta_options <- showcols_meta[!pmeta_attr$is_numeric]

numeric_palettes <- numeric_color_opt()
names(numeric_palettes) <- numeric_palettes

heatmap_palettes <- numeric_palettes[numeric_palettes %in% c("RdYlBu", "RdBu", "viridis", "magma", "plasma", "inferno")]

dataset_color<-c(
    "E9.5 AGM-Endo 1"="#B2DF8A",
    "E9.5 AGM-Endo 2"= "#99D8C9", 
    "E9.5 AGM-HE 1"="#1b7837",
    "E9.5 AGM-HE 2" ="#33A02C", 
    "E9.5 Entire Endothelium" = "#74c476",
    "E10.5 AGM-Endo 1"="#A6CEE3", 
    "E10.5 AGM-Endo 2"="#67A9CF",
    "E10.5 AGM-HE 1"="#1F78B4", 
    "E10.5 AGM-HE 2"="#1c9099",
    "E10.5 Entire Endothelium" = "#41b6c4",
    "E10.5 Cluster 1"="#B15928",
    "E10.5 Cluster 2" = "#d8b365",
    "E10.5 Cluster 3" = "#cab2d6",
    "E10.5 Cluster CD45+"="#6A3D9A", 
    "E10.5 Cluster CD45-"="#cc4c02", 
    "E11.5 Cluster 1"="#fa9fb5", 
    "E11.5 Cluster 2"="#e7298a",
    "E11.5 Cluster Cd45+Cd27+"="#E31A1C", 
    "E11.5 Cluster Cd45+Cd27-"="#FDBF6F", 
    "E11.5 FL-LMPP" = "#91003f",
    "E14.5 FL-HSC"="#FF7F00",
    "E9.5 YS-EMP" = "#003366",
    "E10.5 YS-Endo" = "#f1b6da",
    "E10.5 YS-HE" = "#9e9ac8"
)


cdataset_color<-c(
    "E9.5 AGM-Endo"="#B2DF8A",
    "E9.5 AGM-HE"="#1b7837",
    "E9.5 Entire Endothelium" = "#74c476",
    "E10.5 AGM-Endo"="#A6CEE3", 
    "E10.5 AGM-HE"="#1F78B4", 
    "E10.5 Entire Endothelium" = "#41b6c4",
    "E10.5 Cluster" = "#cab2d6",
    "E10.5 Cluster CD45+"="#6A3D9A", 
    "E10.5 Cluster CD45-"="#cc4c02", 
    "E11.5 Cluster"="#fa9fb5",
    "E11.5 Cluster Cd45+Cd27+"="#E31A1C", 
    "E11.5 Cluster Cd45+Cd27-"="#FDBF6F", 
    "E11.5 FL-LMPP" = "#91003f",
    "E14.5 FL-HSC"="#FF7F00",
    "E9.5 YS-EMP" = "#003366",
    "E10.5 YS-Endo" = "#f1b6da",
    "E10.5 YS-HE" = "#9e9ac8"
)


gene_symbol_choices <- rownames(fData(cello))
names(gene_symbol_choices) <- fData(cello)$symbol

gene_id_symbol <- names(gene_symbol_choices)
names(gene_id_symbol) <- gene_symbol_choices

max_pc_show <- 10


