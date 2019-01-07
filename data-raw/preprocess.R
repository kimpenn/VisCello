




clist <- readRDS("data-raw/clist.rds")
elist <- readRDS("data-raw/elist.rds")
all_cds <- readRDS("data-raw/all_cds.rds")
g_all <- readRDS("data-raw/g_all.rds")
time_umap_list <- readRDS("data-raw/time_umap_list.rds")
time_deg_list <- readRDS("data-raw/time_deg_list.rds")
tf_tbl <- read.csv("data-raw/TFcisBP_dataCSV839.csv")
cell_type_markers <- read.csv("data-raw/Celltypemarkers.csv")
graph_genes <- readRDS("data-raw/graph_genes.rds")

names(clist[[1]]@proj)[2] <- "UMAP-2D [Paper]"

max_pc <- 10
clist_cvis <- lapply(1:length(clist), function(i) {
    x <- clist[[i]]
    cvis <- new("cvis")
    cvis@name <- names(clist)[i]
    cvis@idx <- x@idx
    cvis@proj <- x@proj
    # Also reduce PCA projection dimension to reduce size
    if(!is.null(x@proj[["PCA"]]))    cvis@proj[["PCA"]] <- x@proj[["PCA"]][,1:max_pc]
    local_pmeta <- data.frame(Cluster = x@cluster)
    rownames(local_pmeta) <- colnames(all_cds)[cvis@idx]
    #print(identical(rownames(local_pmeta), rownames(x@proj[[1]])))
    cvis@pmeta <- local_pmeta
    #cvis@notes <- "some notes"
    return(cvis)
})
names(clist_cvis) <- names(clist)
clist <- clist_cvis


names(clist)[which(names(clist) == "Early Embryo Germline Rectum")] <- "Early Embryo, Germline and Rectum"
names(clist)[which(names(clist) == "Non Ciliated Neurons")] <- "Non-Ciliated Neurons"
names(clist) <- tools::toTitleCase(names(clist))


keep_elist <- c(
    "Time 150min Lineage"="time150_disp1_bc_regress", 
    "Time 200min Lineage"="time200_disp1_bc_regress", 
    "Time 250min Lineage"="time250_disp1_bc_regress", 
    "Time 250min AB Neuron + Glia"="ABneuronglia[time250]", 
    "Time 250min AB Pharynx"="ABpharynx[time250]", 
    "Time 250min AB Hypodermis"="AB_HYP[time250]", 
    "Time 250min MSxa"="MSXa[time250]", 
    "Time 250min MSxp"="MSXp[time250]", 
    "Time 250min MSxa + MSxp"="MSXa+MSXp[time250]",
    "Time 250min CD BWM"="CD_BWM[time250]")
elist <- elist[keep_elist]
elist_cvis <- lapply(1:length(elist), function(i) {
    x <- elist[[i]]
    cvis <- new("cvis")
    cvis@name <- names(elist)[i]
    cvis@idx <- x@idx
    cvis@proj <- x@proj
    # Also reduce PCA projection dimension to reduce size
    if(!is.null(x@proj[["PCA"]]))    cvis@proj[["PCA"]] <- x@proj[["PCA"]][,1:max_pc]
    local_pmeta <- data.frame(Cluster = x@cluster)
    rownames(local_pmeta) <- colnames(all_cds)[cvis@idx]
    #print(identical(rownames(local_pmeta), rownames(x@proj[[1]])))
    cvis@pmeta <- local_pmeta
    #cvis@notes <- "some notes"
    return(cvis)
})
names(elist_cvis) <- names(keep_elist)
elist <- elist_cvis




sexpr <- exprs(all_cds)
any(duplicated(fData(all_cds)$gene_short_name))
# If duplicate exists, make.names
rownames(sexpr) <- fData(all_cds)$gene_short_name
all_cds@assayData$exprs <- sexpr
rownames(fData(all_cds)) <- fData(all_cds)$gene_short_name
sexpr_nmlog <- all_cds@auxOrderingData$normalize_expr_data
rownames(sexpr_nmlog) <- fData(all_cds)$gene_short_name
all_cds@auxOrderingData$normalize_expr_data <- sexpr_nmlog



# convert all non-numeric meta into factor, and replace _ with space to make names look nicer
for (x in as.character(pmeta_attr$meta_id)) {
    if(grepl("time.bin",x)) next
    if(x %in% colnames(pData(all_cds))) {
        if(!is.numeric(pData(all_cds)[[x]])) {
            print(x)
            unique_levels<- as.character(unique(pData(all_cds)[[x]]))
            if("unannotated" %in% unique_levels) unique_levels <- c(unique_levels[unique_levels!="unannotated"], "unannotated")
            replace_levels <- gsub("_", " ", unique_levels, fixed=TRUE)
            names(replace_levels) <- unique_levels
            pData(all_cds)[[x]] <- replace_levels[as.character(pData(all_cds)[[x]])]
            pData(all_cds)[[x]] <- factor(pData(all_cds)[[x]], levels = replace_levels)
        }
    } 
}

pData(all_cds)$embryo.time.bin[which(pData(all_cds)$embryo.time.bin == "unannotated")] <- NA
pData(all_cds)$raw.embryo.time.bin[which(pData(all_cds)$embryo.time.bin == "unannotated")] <- NA
first_num <- as.numeric(stringi::stri_extract_first_regex(unique(pData(all_cds)$embryo.time.bin), "[0-9]+"))
bin_level <- unique(pData(all_cds)$embryo.time.bin)[order(first_num)]
pData(all_cds)$embryo.time.bin <- factor(pData(all_cds)$embryo.time.bin, levels = bin_level[!is.na(bin_level)])
pData(all_cds)$raw.embryo.time.bin <- factor(pData(all_cds)$raw.embryo.time.bin, levels = bin_level[!is.na(bin_level)])
# Now construct a "meta attribute dataframe" to store the info of what each column is about, and which ones you want to show
# This will be used in the 'Color By' option
colorby_order <- c(
    "Cell type (broad)" = "cell.type",
    "Cell type + cell subtype" = "plot.cell.type",
    "Cell subtype" = "cell.subtype",
    
    "Gene expression" = "gene.expr", # This will not be in any meta column
    "UMI count" = "n.umi",
    "Number of expressed Genes" = "num.genes.expressed",
    "Size factor" = "Size_Factor",
    "Likely doublets/debris" = "to.filter",
    
    "Batch" = "batch",
    "Time point" = "time.point",
    "Embryo time" = "raw.embryo.time",
    "Embryo time bin" = "raw.embryo.time.bin",
    
    "150min early lineage" = "t150.lineages",
    "250min early lineage" = "t250.lineages",
    "Muscle mesoderm early lineage" = "mm.lineage",
    "Abala early lineage" = "temp.ABala.250",
    
    "Cluster" = "Cluster" # Note only this meta data is taken from local cvis object, 'Cluster' is now allowed in global meta colnames.
)

pmeta_attr <- data.frame(meta_id = colorby_order, meta_name = names(colorby_order), stringsAsFactors=FALSE)

pmeta_attr$is_numeric <- sapply(as.character(pmeta_attr$meta_id), function(x) {
    if(x %in% colnames(pData(all_cds))) {
        is.numeric(pData(all_cds)[[x]])
    } else if(x %in% colnames(clist[[1]]@pmeta)) {
        is.numeric(clist[[1]]@pmeta[[x]])
    } else if(x == "gene.expr") {
        T
    } else {
        NA
    }
})



# Which meta to show in (hide from) basic menu [this is set in global.R]
# pmeta_attr$basic_menu <- ifelse(pmeta_attr$meta_name %in% c("Cell type", "Cell subtype", "Gene Expression", "Embryo time bin"), T, F)

# optional, default_pal of each meta
pmeta_attr$dpal <- ifelse(pmeta_attr$is_numeric, "rainbow2", "Set1")
pmeta_attr$dpal[grepl("time.bin", pmeta_attr$meta_id)] <- "gg_color_hue"
pmeta_attr$dscale <- ifelse(pmeta_attr$is_numeric, "log10", NA)
pmeta_attr$dscale[which(pmeta_attr$meta_id %in% c("Size_Factor", "raw.embryo.time", "embryo.time"))] <- "identity"
# For now you need to manually specify
pData(all_cds) <- pData(all_cds)[,which(colnames(pData(all_cds)) %in% colorby_order)]




# Load Sup Table S1 and modify
ct_marker_tbl <- read.csv("data-raw/Table S1_ marker genes for terminal cell types - Sheet1.csv")
ct_marker_tbl$X <- NULL
cell_type_markers <- ct_marker_tbl
cur_umap_names<-as.character(unique(cell_type_markers$UMAP))
names(cur_umap_names) <- cur_umap_names
cur_umap_names<-tools::toTitleCase(cur_umap_names)
cur_umap_names[which(!cur_umap_names %in% names(clist))]
cur_umap_names["Hypodermis and seam cells"] <- "Hypodermis and Seam"
cur_umap_names["Early embryo, germline, and rectum"] <- "Early Embryo, Germline and Rectum"
cur_umap_names[which(!cur_umap_names %in% names(clist))]

cell_type_markers$UMAP <- cur_umap_names[as.character(cell_type_markers$UMAP)]


# Graph plot

graph_genes <- graph_genes[order(graph_genes)]
g_meta_list<- readRDS("data-raw/image_gene_meta_list.rds")
g_meta_list <- lapply(g_meta_list, function(x) {
    colnames(x) <- c("Gene", "Series", "Allele", "Strain", "Reporter type", "Data source")
    x$Gene <- NULL
    x[["Data source"]][which(x[["Data source"]] == "EPIC")] <- "EPiC"
    x <- x[,c("Series", "Reporter type", "Strain", "Allele", "Data source")]
    return(x)
})

usethis::use_data(clist, overwrite = T)
usethis::use_data(elist, overwrite = T)
usethis::use_data(all_cds, overwrite = T)
usethis::use_data(pmeta_attr, overwrite = T)

usethis::use_data(g_all, overwrite = T)
usethis::use_data(g_meta_list, overwrite = T)

usethis::use_data(tf_tbl, overwrite = T)
usethis::use_data(cell_type_markers, overwrite = T)
usethis::use_data(graph_genes, overwrite = T)

devtools::load_all()


