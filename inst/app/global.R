

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)


### Which meta data to show, and in what order ###
ctype_cols_advanced <- pmeta_attr$meta_id
names(ctype_cols_advanced) <- pmeta_attr$meta_name
ctype_cols_basic <- ctype_cols_advanced[c("Cell type (broad)", "Cell type + cell subtype", "Gene expression", "Embryo time")]
elin_cols_basic <- ctype_cols_advanced[c("150min early lineage","250min early lineage","Gene expression", "Embryo time")]
elin_cols_advanced <- c(elin_cols_basic, ctype_cols_advanced[!ctype_cols_advanced %in% elin_cols_basic])

bp_colorBy_choices <- ctype_cols_advanced[c("Cell type (broad)", "Cell subtype", "Embryo time bin")]

de_meta_options <- ctype_cols_advanced[c("Cell type (broad)", "Cell subtype", "150min early lineage", "250min early lineage", "Muscle mesoderm early lineage", "Abala early lineage", "Cluster")]

numeric_palettes <- numeric_color_opt()
names(numeric_palettes) <- numeric_palettes

image_palettes <- numeric_palettes[numeric_palettes %in% c("RdYlBu", "RdBu", "viridis", "magma", "plasma", "inferno")]
heatmap_palettes <- image_palettes
gene_symbol_choices <- rownames(all_cds)
names(gene_symbol_choices) <- gene_symbol_choices

image_colorBy_choices <- graph_genes

max_pc_show <- 10


