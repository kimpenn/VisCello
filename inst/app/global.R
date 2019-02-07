library(Biobase)
library(VisCello.base)
library(shiny)
library(reticulate)
py_install("pandas")
py_install("git+https://github.com/informatics-isi-edu/deriva-py.git")

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

mainTitle = "VisCello"

Cello <- setClass("Cello",
                  slots = c(
                      name = "character", # The name of cvis
                      idx = "numeric", # The index of global cds object
                      proj = "list", # The projections as a list of data frame
                      pmeta = "data.frame", # The local meta data
                      notes = "character" # Other information to display to the user
                  )
)

organism = "mmu" # Set species


# This part can be customized if necessary, basically which meta columns you want to show + Gene Expression (gene.expr)

# meta_order <- c(
#     "Dataset" = "Dataset", 
#     "Gene expression" = "gene.expr",
#     "Total_mRNAs" = "Total_mRNAs",
#     "num_genes_expressed" = "num_genes_expressed",
#     "Size_Factor" = "Size_Factor",
#     "Cluster" = "Cluster"
# )



max_pc_show <- 10


