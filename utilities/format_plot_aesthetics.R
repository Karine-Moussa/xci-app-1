# Formatting plotting labels
# Tab 1 Plot X Axis 
# x_labels_genes_start - the first gene for each region
x_labels_genes_start <- c("PLCXD1", "WWC3", "PHKA2", "PDHA1", "SMS", "TBCAP1", "DYNLT3", "FUNDC1", "CFP", 
                          "GSPT2", "MAGED2", "LAS1L", "PJA1", "TSIX", "ABCB7", "ATRX", 
                          "CHM", "BEX2", "NXT2", "WDR44", "THOC2", "SASH3", "RAP2C", 
                          "FAM122B", "ATP11C", "IDS")
# x_labels_genes_mid - the median gene for each region
x_labels_genes_mid <- c("P2RY8", "PIGA", "PHKA2", "MAP7D2", "EIF2S3", "CYBB", "MED14", "USP11", "TIMM17B", 
                        "HUWE1", "MAGEH1", "MSN", "ZMYM3", "FTX", "UPRT", "P2RY10", 
                        "HNRNPH2", "MORF4L2", "PRPS1", "TMEM164", "RPL39", "THOC2", 
                        "ELF4", "MBNL3", "SLC9A6", "ATP11C", "IRAK1")
# choose either x..mid or x..start to use as breakpoints for the plot
x_labels_genes <- x_labels_genes_mid

x_labels_pos <- unique(x_expr_mod[,"ChromPos"])
x_labels_bp <- rep("",length(x_labels_pos))
i = 0
for (pos in x_labels_pos){
    i = i + 1
    x_labels_bp[i] <- sort(decreasing = T, x_expr_mod[x_expr_mod$ChromPos==pos,"start"])[1]
}
rm(i)

x_region_breaks <- (xchrom_map_colored$bp_start)
x_region_labels <- list()
i = 0
for (row in 1:nrow(xchrom_map_colored)){
    i <- i + 1
    s <- paste0(xchrom_map_colored$arm[i],xchrom_map_colored$band[i])
    x_region_labels <- c(x_region_labels, s)
}
rm(i)

# Tab 1 Par Region Shading (in bp)
par1_boundaries <- c(100001,2781479)
par2_boundaries <- c(155701383,156030895)
centre_boundaries <- c(58100001,63800000)

# Tab 1 Xchrom Map
xchrom_png <- readPNG('images/xchrom-850bp-annotated.png')

# Tab 1 Create Shape Vector depending on whether 
# the -log10(p-value) is actually higher than shown
# If the p value is actually lower than represented, 
# change the shape to a triangle (17). Otherwise keep the 
# shape as a circle (16)
shape_vector <- ifelse(x_expr_mod$p_mod_flag == TRUE, 17, 16)
shape_vector <- as.factor(shape_vector)

## Create chromosome image
colormap_df <- xchrom_map_colored[,c('bp_start','bp_stop','BandColor')]
chrom_segments <- vector('list')
chrom_segments_colored <- vector('list')
chrom_size <- 8
y_place <- -1.5
# Create start and end segments
chrom_segments$start <- geom_segment(aes(x = colormap_df$bp_start[1], y = y_place, xend = colormap_df$bp_stop[1], yend = y_place),
                                  size = chrom_size, color = colormap_df$BandColor[1])
chrom_segments$end <- geom_segment(aes(x = colormap_df$bp_start[length(colormap_df$bp_start)], y = y_place, 
                                         xend = colormap_df$bp_stop[length(colormap_df$bp_stop)], yend = y_place),
                                     size = chrom_size, color = colormap_df$BandColor[length(colormap_df$BandColor)])

# Create middle segements
i = 1
for(i in 2:dim(colormap_df)[1]-2){
    i <- i + 1
    segment_entry = paste0("chrom_segments$seg",i," <- geom_segment(aes(x = ",colormap_df$bp_start[i],", y = ",y_place,", xend = ",colormap_df$bp_stop[i],
                           ", yend = ",y_place,"), size = ",chrom_size,", color = '",colormap_df$BandColor[i],"')")
    eval(parse(text = segment_entry))
}
rm(i)
# Now overlay with PAR and CENTROMERE shading
# First shade ends, then overlay rounded areas
chrom_segments_colored$start <- geom_segment(aes(x = 0, y = y_place, xend = par1_boundaries[2], yend = y_place),
                                            size = chrom_size, color = "lightblue", alpha=0.25, lineend = "round")
chrom_segments_colored$end <- geom_segment(aes(x = par2_boundaries[1], y = y_place, xend = max(colormap_df$bp_stop), yend = y_place),
                                            size = chrom_size, color = "lightblue", alpha=0.25, lineend = "round")

# Regularly shade
chrom_segments_colored$par1 <- geom_segment(aes(x = 0, y = y_place, xend = par1_boundaries[2], yend = y_place),
                                    size = chrom_size, color = "lightblue", alpha=0.25)
chrom_segments_colored$par2 <- geom_segment(aes(x = par2_boundaries[1], y = y_place, xend = max(colormap_df$bp_stop), yend = y_place),
                                     size = chrom_size, color = "lightblue", alpha=0.25)
chrom_segments_colored$centre <- geom_segment(aes(x = centre_boundaries[1], y = y_place, xend = centre_boundaries[2], yend = y_place),
                                     size = chrom_size, color = "pink", alpha=0.25)

# Incorporate other studies with annotations/shading
cott_carr_will_df <- data.frame(gene = tuketal_suppl_table_1_combined$`Gene name`,
                                start = as.numeric(tuketal_suppl_table_1_combined$`Start position`),
                                end = as.numeric(tuketal_suppl_table_1_combined$`End position`),
                                status = tuketal_suppl_table_1_combined$`Combined XCI status`,
                                color = ifelse(tuketal_suppl_table_1_combined$`Combined XCI status` == "escape", "purple", 
                                               ifelse(tuketal_suppl_table_1_combined$`Combined XCI status` == "variable", 
                                                      "turquoise3","white")))
