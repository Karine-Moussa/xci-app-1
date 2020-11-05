# Import data needed
x_expr <- read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
xchrom_map_colored <- read.delim("data_intermediate/xchrom_map_colored",header=T,sep="\t")
gene_stat_table <- read.csv("data_intermediate/gene_stat_table.csv",header=T)

#### Create x_expr data table with colors
# Create vector of BandColors based on gene_stat_table
BandColor <- rep("",nrow(x_expr))
i <- 0
for (gene in x_expr$GENE){
    i <- i + 1
    BandColor[i] <- gene_stat_table[gene_stat_table$GENE == gene, "BAND_COLOR"]
}
# clean up variables
rm(i) 
# Add BandColor vector to x_expr matrix 
# First remove the column from the x_expr matrix
# if it is already there.
if("BandColor" %in% colnames(x_expr)){
    x_expr <- test[, !names(x_expr) %in% "BandColor"]
}
x_expr <- cbind(x_expr, BandColor)
# clean up variables
rm(BandColor) 

# Format x_expr
attach(x_expr)
STATUS <- as.factor(status)
GENE <- as.factor(GENE)

# List of samples
SAMPLE_NAMES <- c(unique(x_expr[,"sample"]))

# P-value significance
P_SIG <- 5e-10