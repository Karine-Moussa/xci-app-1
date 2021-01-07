# Libraries
library(readxl)

# Import data needed
## x_expr table
x_expr <- read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
x_expr_tauplus <- x_expr[x_expr$f < 0.25,]  # only data with strong skew values

## Import phenotype conversion table
pheno_path <- "data_intermediate/diseasetrait_naming_conversion.xlsx"
pheno_conv_UKBIO_list <- read_excel(pheno_path, sheet = "UKBIO_list")
pheno_conv_GWAS_list <- read_excel(pheno_path, sheet = "GWAS_list")
pheno_conv_NELSON1_list <- read_excel(pheno_path, sheet = "NELSON1_list")
rm(pheno_path)

## GWAS associations
GWAS_ASSOCIATIONS <- read.csv("data_intermediate/gwas_assoc_v1_02_xonly.csv", header=T,na.strings="?")
#LIST_OF_TRAITS_GWAS <- data.frame(GWAS_NAME = tolower(unique(GWAS_ASSOCIATIONS$DISEASE.TRAIT)))
df <- data.frame(GWAS_NAME = tolower(pheno_conv_GWAS_list$`DISEASE/TRAIT`),
                 UKBIO_NAME = tolower(pheno_conv_GWAS_list$UKBIO))
cols <- c("GWAS_NAME","UKBIO_NAME")
colnames(df) <- cols
LIST_OF_TRAITS_GWAS <- df
rm(df)

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

