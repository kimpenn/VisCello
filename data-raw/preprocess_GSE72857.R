# Script to preprocess sample data files, based on Qin's readme

library(Matrix)
library(Biobase)
raw_df <- read.table("data-raw/GSE72857_raw.txt")
norm_df <- read.table("data-raw/GSE72857_log2norm.txt")
pmeta <- read.table("data-raw/GSE72857_pmeta.txt")
fmeta <- read.table("data-raw/GSE72857_fmeta.txt")
# feature meta Must contain a column id - ensemble id, and correspoinding symbol, the matrix rownames should be ensemble id.
colnames(fmeta) <- c("id", "symbol") 
rownames(fmeta) <- fmeta$id

eset <- new("ExpressionSet",
             assayData = assayDataNew("environment", exprs=Matrix(as.matrix(raw_df), sparse = T), norm_exprs = Matrix(as.matrix(norm_df), sparse = T)),
             phenoData =  new("AnnotatedDataFrame", data = pmeta),
             featureData = new("AnnotatedDataFrame", data =fmeta))

factor_cols <- c("Mouse_ID", "cluster", "State")
for(c in factor_cols) {
    pData(eset)[[c]] <- factor(pData(eset)[[c]])
}

saveRDS(eset, "inst/app/data/eset.rds")

library(irlba)
source("data-raw/preprocess_scripts/cello.R")
source("data-raw/preprocess_scripts/cello_dimR.R")
# Creating a cello for all the cells
cello <- new("Cello", name = "All Data", idx = 1:ncol(eset)) # Index is basically the column index, here all cells are included 
# Code for computing dimension reduction, not all of them is necessary, and you can input your own dimension reduction result into the cello@proj list.
# It is also recommended that you first filter your matrix to remove low expression genes and cells, and input a matrix with variably expressed genes
cello <- compute_pca_cello(eset, cello, num_dim = 50) # Compute PCA 
cello <- compute_tsne_cello(eset, cello, use_dim = 30, n_component = 2, perplexity = 30) # Compute t-SNE
cello <- compute_umap_cello(eset, cello, use_dim = 30, n_component = 2, umap_path = "data-raw/preprocess_scripts/python/umap.py") # Compute UMAP, need reticulate and UMAP (python package) to be installed
cello <- compute_umap_cello(eset, cello, use_dim = 30, n_component = 3, umap_path = "data-raw/preprocess_scripts/python/umap.py") # 3D UMAP
clist <- list()
clist[["All Data"]] <- cello
saveRDS(clist, "inst/app/data/clist.rds") # Note if you change the file name from clist.rds to other name, you need to change the readRDS code in global.R