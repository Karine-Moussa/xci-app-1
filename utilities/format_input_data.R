# Libraries
library(readxl)

# Import data needed
## x_expr table
x_expr <- read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
x_expr_tauplus <- x_expr[x_expr$f < 0.25,]  # only data with strong skew values

## supplementary tables
suppl_table_1 <- read.csv("resources_studies/Tuketal2017/Suppl.Table.1.csv", header=F,na.strings="?")
suppl_table_1 <- suppl_table_1[-1,]  # remove top row
names(suppl_table_1) <- suppl_table_1[1,] # make "new top row" the headers
suppl_table_1 <- suppl_table_1[-1,] # remove "new top row"
suppl_table_1 <- suppl_table_1[,-c(15:ncol(suppl_table_1))] # remove NA colums 

suppl_table_1_combined <- suppl_table_1[,1:7]       # section suppl table
suppl_table_1_combined <- suppl_table_1_combined[-which(suppl_table_1_combined == ""), ] # remove blank rows
suppl_table_1_cottonetal <- suppl_table_1[,8:11]    # section suppl table
suppl_table_1_cottonetal <- suppl_table_1_cottonetal[-which(suppl_table_1_cottonetal == ""), ] # remove blank rows
suppl_table_1_carrwillard <- suppl_table_1[,12:14]  # section suppl table
suppl_table_1_carrwillard <- suppl_table_1_carrwillard[-which(suppl_table_1_carrwillard == ""), ] # remove blank rows

rm(suppl_table_1)   # remove full suppl_table_1

## GWAS assocations
GWAS_ASSOCIATIONS <- read.csv("data_intermediate/gwas_assoc_v1_02_xonly.csv", header=T,na.strings="?")
list_of_diseases <- unique(GWAS_ASSOCIATIONS$DISEASE.TRAIT)

## X-chromosome staining
xchrom_map_colored <- read.delim("data_intermediate/xchrom_map_colored",header=T,sep="\t")

## Table of genes and escape status
gene_stat_table <- read.csv("data_intermediate/gene_stat_table.csv",header=T)

