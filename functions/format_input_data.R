# Import data
x_expr = read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
# xci_status=read.csv("data_sources/Suppl.Table.1.csv",header=T,na.strings="?")
attach(x_expr)
# Sort by gene start position
x_expr <- x_expr[order(start),]
# Format categorical data
STATUS <- as.factor(status)
GENE <- as.factor(GENE)
# List of samples
SAMPLE_NAMES <- c(unique(x_expr[,"sample"]))
# P-value significance
P_SIG <- 5e-10
# XCHROM_MAP_COLORED
XCHROM_MAP_COLORED <- read.delim("data_intermediate/xchrom_map_colored",header=T,sep="\t")
