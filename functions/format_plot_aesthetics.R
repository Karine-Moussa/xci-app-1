# Formatting plotting labels
# Tab 1 Plot X Axis 
x_labels_genes <- unique(x_expr[which(x_expr[,"start"] %in% x_labels_bp),"GENE"])
x_labels_genes_start <- c("PLCXD1", "WWC3", "SMS", "TBCAP1", "DYNLT3", "FUNDC1", "CFP", 
                          "GSPT2", "MAGED2", "LAS1L", "PJA1", "TSIX", "ABCB7", "ATRX", 
                          "CHM", "BEX2", "NXT2", "WDR44", "THOC2", "SASH3", "RAP2C", 
                          "FAM122B", "ATP11C", "IDS")
x_labels_genes_mid <- c("P2RY8", "PIGA", "EIF2S3", "CYBB", "MED14", "USP11", "TIMM17B", 
                        "HUWE1", "MAGEH1", "MSN", "ZMYM3", "FTX", "UPRT", "P2RY10", 
                        "HNRNPH2", "MORF4L2", "PRPS1", "TMEM164", "RPL39", "THOC2", 
                        "ELF4", "MBNL3", "SLC9A6", "ATP11C", "IRAK1")
x_labels_genes <- x_labels_genes_mid
#x_labels_bp <- sort(unique(x_expr[which(x_expr[,"GENE"] %in% x_labels_genes),"start"]))
x_labels_pos <- rep("",length(x_labels_genes))
for (i in 1:length(x_labels_genes)){
    x_labels_pos[i] = unique(x_expr_mod[x_expr_mod$GENE==x_labels_genes[i],"ChromPos"])
}
