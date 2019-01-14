

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

mainTitle = "VisCello"


eset <- readRDS("data/eset.rds")
clist <- readRDS("data/clist.rds")

# This part can be customized if necessary, basically which meta columns you want to show + Gene Expression (gene.expr)

# meta_order <- c(
#     "Dataset" = "Dataset", 
#     "Gene expression" = "gene.expr",
#     "Total_mRNAs" = "Total_mRNAs",
#     "num_genes_expressed" = "num_genes_expressed",
#     "Size_Factor" = "Size_Factor",
#     "Cluster" = "Cluster"
# )

meta_order <- colnames(pData(eset))
names(meta_order) <- colnames(pData(eset))

# Don't show factors that's not useful to the user
meta_order <- meta_order[!meta_order %in% c("Amp_batch_ID", "well_coordinates", "Number_of_cells", "Plate_ID", "Batch_desc", "Pool_barcode", "Cell_barcode", "RMT_sequence", "no_expression")]

# Do not edit below
meta_order["Gene Expression"] = "gene.expr"
pmeta_attr <- data.frame(meta_id = meta_order, meta_name = names(meta_order), stringsAsFactors=FALSE)
pData(eset) <- pData(eset)[,which(colnames(pData(eset)) %in% pmeta_attr$meta_id)]


pmeta_attr$is_numeric <- sapply(as.character(pmeta_attr$meta_id), function(x) {
    if(x %in% colnames(pData(eset))) {
        is.numeric(pData(eset)[[x]])
    } else if(x %in% colnames(clist[[1]]@pmeta)) {
        is.numeric(clist[[1]]@pmeta[[x]])
    } else if(x == "gene.expr") {
        T
    } else {
        NA
    }
})


# Edit if necessary, Which meta to show 
pmeta_attr$dpal <- ifelse(pmeta_attr$is_numeric, "RdBu", "Set1")
pmeta_attr$dscale <- ifelse(pmeta_attr$is_numeric, "log10", NA)
pmeta_attr$dscale[which(pmeta_attr$meta_id %in% c("Size_Factor"))] <- "identity"




### Which meta data to show, and in what order ###
showcols_meta <- pmeta_attr$meta_id
names(showcols_meta) <- pmeta_attr$meta_name

bp_colorBy_choices <- showcols_meta[!pmeta_attr$is_numeric]

de_meta_options <- showcols_meta[!pmeta_attr$is_numeric]

numeric_palettes <- numeric_color_opt()
names(numeric_palettes) <- numeric_palettes

heatmap_palettes <- numeric_palettes[numeric_palettes %in% c("RdYlBu", "RdBu", "viridis", "magma", "plasma", "inferno")]

gene_symbol_choices <- rownames(fData(eset))
names(gene_symbol_choices) <- fData(eset)$symbol

gene_id_symbol <- names(gene_symbol_choices)
names(gene_id_symbol) <- gene_symbol_choices

max_pc_show <- 10


