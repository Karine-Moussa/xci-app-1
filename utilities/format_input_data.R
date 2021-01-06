# Libraries
library(readxl)

# Import data needed
## x_expr table
x_expr <- read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
x_expr_tauplus <- x_expr[x_expr$f < 0.25,]  # only data with strong skew values

## Additional studies
tuketal_suppl_table_1 <- read.csv("resources_studies/Tuketal2017/Suppl.Table.1.csv", header=F,na.strings="?")
tuketal_suppl_table_1 <- tuketal_suppl_table_1[-1,]  # remove top row
names(tuketal_suppl_table_1) <- tuketal_suppl_table_1[1,] # make "new top row" the headers
tuketal_suppl_table_1 <- tuketal_suppl_table_1[-1,] # remove "new top row"
tuketal_suppl_table_1 <- tuketal_suppl_table_1[,-c(15:ncol(tuketal_suppl_table_1))] # remove NA colums 
tuketal_suppl_table_1_combined <- tuketal_suppl_table_1[,1:7]       # section suppl table
tuketal_suppl_table_1_combined <- tuketal_suppl_table_1_combined[-which(tuketal_suppl_table_1_combined == ""), ] # remove blank rows
tuketal_suppl_table_1_cottonetal <- tuketal_suppl_table_1[,8:11]    # section suppl table
tuketal_suppl_table_1_cottonetal <- tuketal_suppl_table_1_cottonetal[-which(tuketal_suppl_table_1_cottonetal == ""), ] # remove blank rows
tuketal_suppl_table_1_carrwillard <- tuketal_suppl_table_1[,12:14]  # section suppl table
tuketal_suppl_table_1_carrwillard <- tuketal_suppl_table_1_carrwillard[-which(tuketal_suppl_table_1_carrwillard == ""), ] # remove blank rows
rm(tuketal_suppl_table_1)   # remove full tuketal_suppl_table_1

## GWAS associations
GWAS_ASSOCIATIONS <- read.csv("data_intermediate/gwas_assoc_v1_02_xonly.csv", header=T,na.strings="?")
LIST_OF_TRAITS_GWAS <- tolower(unique(GWAS_ASSOCIATIONS$DISEASE.TRAIT))

## Nelson et al associations
NELSON_ASSOCIATIONS_1 <- read.delim("resources_studies/Nelsonetal2015/nelson_supp_dataset1",header=T,sep="\t",na.strings="?")
LIST_OF_TRAITS_NELSON <- tolower(unique(NELSON_ASSOCIATIONS_1$Disease))

## Phenotype Occurrence Rates from UK Biobank
PHENO_RATES_UKBIO <- read.delim("data_sources/phenotypes_anno.tsv",header=T,sep="\t",na.strings="?")
LIST_OF_TRAITS_UKBIO <- tolower(unique(PHENO_RATES_UKBIO$des))

## X-chromosome staining
xchrom_map_colored <- read.delim("data_intermediate/xchrom_map_colored",header=T,sep="\t")

## Table of genes and escape status
gene_stat_table <- read.csv("data_intermediate/gene_stat_table.csv",header=T)

