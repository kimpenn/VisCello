

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)


### Which meta data to show, and in what order ###
ctype_cols_advanced <- pmeta_attr$meta_id
names(ctype_cols_advanced) <- pmeta_attr$meta_name
ctype_cols_basic <- ctype_cols_advanced[c("Cell type", "Cell subtype", "Gene Expression", "Smoothed embryo time bin")]
elin_cols_basic <- ctype_cols_advanced[c("150min Early Lineage","250min Early Lineage","Gene Expression", "Raw embryo time bin")]
elin_cols_advanced <- c(elin_cols_basic, ctype_cols_advanced[!ctype_cols_advanced %in% elin_cols_basic])



samples <- names(clist)
names(samples) <- samples
early_samples <- names(elist)
names(early_samples) <- early_samples
numeric_palettes <- numeric_color_opt()
names(numeric_palettes) <- numeric_palettes


gene_symbol_choices <- rownames(all_cds)
names(gene_symbol_choices) <- gene_symbol_choices


image_colorBy_choices <- c("time"="time", "lin16"="lin16", graph_genes)

lin16 <- c("ABala", "ABalp", "ABara", "ABarp", "ABpla", "ABplp", "ABpra", "ABprp", "MSa", "MSp", "Ea", "Ep", "Ca", "Cp", "D", "P4")
lin16_color <- get_factor_color(lin16, pal = "Paired")
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

max_pc_show <- 10


