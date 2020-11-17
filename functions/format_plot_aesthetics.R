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

# clean up variables
rm(i)