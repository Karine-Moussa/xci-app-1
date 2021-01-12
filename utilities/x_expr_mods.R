# Adding to x_expr data
# This will return x_expr_mod, a modified version of 
# the original x_expr_mod data frame.

# Create vector of BandColors based on gene_stat_table
BandColor <- rep("",nrow(x_expr))
ChromPos <- rep("",nrow(x_expr))
i <- 0
for (gene in x_expr$GENE){
    i <- i + 1
    BandColor[i] <- gene_stat_table[gene_stat_table$GENE == gene, "BAND_COLOR"]
    ChromPos[i] <- gene_stat_table[gene_stat_table$GENE == gene, "CHROM_POS"]
}

# Reformat p-values if they are too low
# Set any p = 0 to p = P_MIN
# Create a flag if this occurs
p_value_mod <- ifelse(x_expr$p_value == 0, P_MIN, x_expr$p_value)
p_mod_flag <- ifelse(x_expr$p_value == 0, TRUE, FALSE)

# Add -log10 column (for testing)
p_value_mod_neglog10 <- -log10(p_value_mod)

# Reformat skew values to be characters
skew <- format(x_expr$f, digits = 3)

# Add the new elements to x_expr_mod
x_expr_mod <- data.frame()
x_expr_mod <- cbind(x_expr, BandColor, ChromPos, 
                    p_value_mod, p_mod_flag, skew,
                    p_value_mod_neglog10)

# add an escape color column
for(i in 1:nrow(x_expr_mod)){
    status <- x_expr_mod$status[i]
    color <- ifelse(status == "E", "purple", 
                    ifelse(status == "S", 
                           "lightsteelblue3", "turquoise3"))
    x_expr_mod$color[i] <- color
}

# clean up variables 
rm(i) 
rm(gene)
rm(BandColor)
rm(ChromPos)
rm(p_value_mod)
rm(p_mod_flag)
rm(p_value_mod_neglog10)
rm(skew)
