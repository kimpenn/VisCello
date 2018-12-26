

enableBookmarking(store = "server")
if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)


clist <- readRDS("src/data/clist.rds")
elist <- readRDS("src/data/elist.rds")
all_cds <- readRDS("src/data/all_cds.rds")
avg_expr <- readRDS("src/data/avg_expr.rds")
g_all <- readRDS("src/data/g_all.rds")
time_umap_list <- readRDS("src/data/time_umap_list.rds")
time_deg_list <- readRDS("src/data/time_deg_list.rds")
tf_tbl <- read.csv("src/data/TFcisBP_dataCSV839.csv")
cell_type_markers <- read.csv("src/data/Celltypemarkers.csv")
graph_genes <- readRDS("src/data/graph_genes.rds")

sexpr <- exprs(all_cds)
rownames(sexpr) <- fData(all_cds)$gene_short_name
sexpr_nmlog <- all_cds@auxOrderingData$normalize_expr_data
rownames(sexpr_nmlog) <- fData(all_cds)$gene_short_name
pmeta <- pData(all_cds)




### Which meta data to show, and in what order ###

# Which not to show
not_show_meta <- colnames(pmeta) %in% c('cell', 'embryo.time.yanai')|
                   grepl('loading', colnames(pmeta))|
                   grepl('umap', colnames(pmeta))|
                   grepl('_SD', colnames(pmeta))|
                   grepl('_DR', colnames(pmeta))
# Which are shown as advanced menu for cell type explorer
ctype_cols_advanced <- c(
    "Cell type" = "cell.type",
    "Cell subtype" = "cell.subtype",
    "Cell type & subtype" = "plot.cell.type",

    "Gene Expression" = "Gene Expression",
    "UMI count" = "n.umi",
    "Number of Expressed Genes" = "num.genes.expressed",
    "Size factor" = "Size_Factor",
    "Problematic cells" = "to.filter",

    "Batch" = "batch",
    "Time point" = "time.point",
    "Raw embryo time" = "raw.embryo.time",
    "Smoothed embryo time" = "embryo.time",
    "Smoothed embryo time bin" = "embryo.time.bin",
    "Raw embryo time bin" = "raw.embryo.time.bin",

    "150min Early Lineage" = "t150.lineages",
    "250min Early Lineage" = "t250.lineages",
    "Muscle mesoderm early lineage" = "mm.lineage",
    "Abala Early Lineage" = "temp.ABala.250",

    "Cluster" = "Cluster" # Note only this meta data is taken from local cvis object, 'Cluster' is now allowed in global meta colnames.
)

# Which are shown as basic menu for cell type explorer
ctype_cols_basic <- ctype_cols_advanced[c("Cell type", "Cell subtype", "Gene Expression", "Smoothed embryo time bin")]
elin_cols_basic <- ctype_cols_advanced[c("150min Early Lineage","250min Early Lineage","Gene Expression", "Raw embryo time bin")]
elin_cols_advanced <- c(elin_cols_basic, ctype_cols_advanced[!ctype_cols_advanced %in% elin_cols_basic])

pmeta <- pmeta[,which(colnames(pmeta) %in% ctype_cols_advanced)]







samples <- names(clist)
names(samples) <- samples
early_samples <- names(elist)
names(early_samples) <- early_samples
numeric_palettes <- c("RdYlBu", "RdBu", "viridis", "rainbow","rainbow2", "Spectral", "diverge_hcl", "redgreen", "grey&red")
names(numeric_palettes) <- numeric_palettes

factor_palettes <- c("Set1", "Set2", "Paired", "Dark2", "Accent")
names(factor_palettes) <- factor_palettes

cvis <- setClass("cvis",
                  slots = c(
                      idx = "numeric",
                      proj = "list",
                      cluster = "factor"
                  )
)



gene_symbol_choices <- rownames(sexpr)
names(gene_symbol_choices) <- gene_symbol_choices


image_colorBy_choices <- c("time"="time", "lin16"="lin16", graph_genes)

lin16 <- c("ABala", "ABalp", "ABara", "ABarp", "ABpla", "ABplp", "ABpra", "ABprp", "MSa", "MSp", "Ea", "Ep", "Ca", "Cp", "D", "P4")
lin16_color <- get_color_vector(lin16, pal = "Paired")
names(lin16_color) <- lin16
lin16_color["Ca"] <- "#7D54A5"
lin16_color["Cp"] <- "#C3AAD2"
lin16_color["D"] <- "#EAD27A"
lin16_color["Ea"] <- "#B15928"
lin16_color["P4"] <- "#B9A499"
lin16_color["ABarp"] <- "#4356ba"
lin16_color["ABprp"] <- "#0c6329"
lin16_color["ABpra"] <- "#0a961a"
lin16_color["MSa"] <- "#E62F27"
lin16_color["MSp"] <- "#F16667"




