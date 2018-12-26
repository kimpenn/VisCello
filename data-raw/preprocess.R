

clist <- readRDS("data-raw/clist.rds")
elist <- readRDS("data-raw/elist.rds")
all_cds <- readRDS("data-raw/all_cds.rds")
avg_expr <- readRDS("data-raw/avg_expr.rds")
g_all <- readRDS("data-raw/g_all.rds")
time_umap_list <- readRDS("data-raw/time_umap_list.rds")
time_deg_list <- readRDS("data-raw/time_deg_list.rds")
tf_tbl <- read.csv("data-raw/TFcisBP_dataCSV839.csv")
cell_type_markers <- read.csv("data-raw/Celltypemarkers.csv")
graph_genes <- readRDS("data-raw/graph_genes.rds")


usethis::use_data(clist, overwrite = T)
usethis::use_data(elist, overwrite = T)
usethis::use_data(all_cds, overwrite = T)
usethis::use_data(avg_expr, overwrite = T)
usethis::use_data(g_all, overwrite = T)
usethis::use_data(time_umap_list, overwrite = T)
usethis::use_data(time_deg_list, overwrite = T)
usethis::use_data(tf_tbl, overwrite = T)
usethis::use_data(cell_type_markers, overwrite = T)
usethis::use_data(graph_genes, overwrite = T)

devtools::load_all()


