# Libraries
library(readxl)
# Import data needed
x_expr <- read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
x_expr_tauplus <- x_expr[x_expr$f < 0.25,]  # only data with strong skew values
GWAS_ASSOCIATIONS <- read.csv("data_intermediate/gwas_assoc_v1_02_xonly.csv", header=T,na.strings="?")
list_of_diseases <- unique(GWAS_ASSOCIATIONS$DISEASE.TRAIT)
xchrom_map_colored <- read.delim("data_intermediate/xchrom_map_colored",header=T,sep="\t")
gene_stat_table <- read.csv("data_intermediate/gene_stat_table.csv",header=T)
