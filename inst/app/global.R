
# Deploy
lapply(list.files("src/", pattern = "\\.(r|R)$", recursive = F, full.names = TRUE), function(x){source(file = x)})

library(shiny);library(shinyBS);library(shinyWidgets);library(shinycssloaders);library(htmlwidgets);library(shinyBS);library(R.utils);library(purrr);library(ggplot2);library(RColorBrewer);library(reshape2);library(gridExtra);library(shinythemes);library(plotly);library(heatmaply);library(pheatmap);library(DT);library(dplyr);library(openxlsx);library(knitr);library(monocle);library(clusterProfiler);library(org.Mm.eg.db);library(org.Hs.eg.db);library(ggraph);library(tidygraph);library(limma);library(irlba);library(data.table);library(VisCello.base)



# Global.R
if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

mainTitle = "VisCello"
organism = "mmu"
id_col = "symbol"; name_col = "symbol"

eset <- readRDS("data/eset.rds")
clist <- readRDS("data/clist.rds")

meta_order <- c(colnames(pData(eset)), colnames(clist[[1]]@pmeta))
names(meta_order) <- meta_order

# Don't show factors that's not useful to the user
meta_order <- meta_order[!meta_order %in% c("barcode")]

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
pmeta_attr$dpal <- ifelse(pmeta_attr$is_numeric, "rainbow2", "Set1")
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

feature_options <- data.table(features = names(gene_symbol_choices))
max_pc_show <- 10

HMD_HumanPhenotype <- read.delim("data/HMD_HumanPhenotype.rpt", header=FALSE)

