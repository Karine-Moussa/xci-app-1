# Adding to x_expr data
# This will return x_expr_mod, a modified version of 
# the original x_expr_mod data frame.
x_expr_mod <- data.frame()

#### Create x_expr data table with colors
# Create vector of BandColors based on gene_stat_table
BandColor <- rep("",nrow(x_expr))
ChromPos <- rep("",nrow(x_expr))
i <- 0
for (gene in x_expr$GENE){
    i <- i + 1
    BandColor[i] <- gene_stat_table[gene_stat_table$GENE == gene, "BAND_COLOR"]
    ChromPos[i] <- gene_stat_table[gene_stat_table$GENE == gene, "CHROM_POS"]
}
# clean up variables
rm(i) 
# Add BandColor vector to x_expr matrix 
x_expr_mod <- cbind(x_expr, BandColor, ChromPos)

# Format p-values if too low

# clean up variables
rm(BandColor)
rm(ChromPos)
rm(gene)