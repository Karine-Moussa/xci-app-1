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
#x_labels_bp <- sort(unique(x_expr[which(x_expr[,"GENE"] %in% x_labels_genes),"start"]))
# Create a vector of chromosomal positions for each gene in the list
x_labels_pos <- rep("",length(x_labels_genes))
for (i in 1:length(x_labels_genes)){
    x_labels_pos[i] = unique(x_expr_mod[x_expr_mod$GENE==x_labels_genes[i],"ChromPos"])
}

# Tab 1 Par Region Shading
par1_boundaries <- c("PLCXD1","TBCAP1")
par2_boundaries <- c("IDS","TMLHE")
centre_boundaries <- c("MAGED2","RRAGB")

# Tab 1 Xchrom Map
xchrom_png <- readPNG('images/xchrom-850bp-annotated.png')

# clean up variables
rm(i)