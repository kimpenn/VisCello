if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

mainTitle = "VisCello"


# This part can be customized if necessary, basically which meta columns you want to show + Gene Expression (gene.expr)

# meta_order <- c(
#     "Dataset" = "Dataset", 
#     "Gene expression" = "gene.expr",
#     "Total_mRNAs" = "Total_mRNAs",
#     "num_genes_expressed" = "num_genes_expressed",
#     "Size_Factor" = "Size_Factor",
#     "Cluster" = "Cluster"
# )

numeric_palettes <- numeric_color_opt()
names(numeric_palettes) <- numeric_palettes

heatmap_palettes <- numeric_palettes[numeric_palettes %in% c("RdYlBu", "RdBu", "viridis", "magma", "plasma", "inferno")]


max_pc_show <- 10


