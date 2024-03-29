# Libraries
library(readxl)

# Import data needed
## x_expr table
x_expr <- read.delim("data_sources/x_expr.tsv",header=T,sep="\t",na.strings="?")
x_expr_tauplus <- x_expr[x_expr$f < 0.25,]  # only data with strong skew values

## Import phenotype conversion table
#pheno_path <- "data_intermediate/diseasetrait_naming_conversion.xlsx"
#pheno_conv_UKBIO_list <- read_excel(pheno_path, sheet = "UKBIO_list")
#pheno_conv_GWAS_list <- read_excel(pheno_path, sheet = "GWAS_list")
#rm(pheno_path)

## GWAS associations
GWAS_ASSOCIATIONS <- read.csv("data_intermediate/gwas_assoc_v1_02_xonly.csv", header=T,na.strings="?")
LIST_OF_TRAITS_GWAS <- data.frame(GWAS_NAME = tolower(unique(GWAS_ASSOCIATIONS$DISEASE.TRAIT)))
#df <- data.frame(GWAS_NAME = tolower(pheno_conv_GWAS_list$`DISEASE/TRAIT`),
#                 UKBIO_NAME = tolower(pheno_conv_GWAS_list$UKBIO))
#cols <- c("GWAS_NAME","UKBIO_NAME")
#colnames(df) <- cols
#LIST_OF_TRAITS_GWAS <- df
#rm(df, cols)

## Phenotype Occurrence Rates from UK Biobank
PHENO_RATES_UKBIO <- read.delim("data_sources/phenotypes_anno.tsv",header=T,sep="\t",na.strings="?")
LIST_OF_TRAITS_UKBIO <- tolower(unique(PHENO_RATES_UKBIO$des))

# GWAS - UKBIOBANK name mapping
GWAS_UKBIO_MAPPING <- readRDS("data_intermediate/gwas_ukbio_mapping_xchrom.rds")

## X-chromosome staining
xchrom_map_colored <- read.delim("data_intermediate/xchrom_map_colored",header=T,sep="\t")

## Table of genes and escape status
gene_stat_table <- read.csv("data_intermediate/gene_stat_table.csv",header=T)

# Save everything as RDS for easier compiling 
saveRDS(x_expr, "rds/x_expr.rds")
saveRDS(x_expr_tauplus, "rds/x_expr_tauplus.rds")
#saveRDS(pheno_conv_UKBIO_list, "rds/pheno_conv_UKBIO_list.rds")
#saveRDS(pheno_conv_GWAS_list, "rds/pheno_conv_GWAS_list.rds")
saveRDS(GWAS_ASSOCIATIONS, "rds/GWAS_ASSOCIATIONS.rds")
saveRDS(LIST_OF_TRAITS_GWAS, "rds/LIST_OF_TRAITS_GWAS.rds")
saveRDS(PHENO_RATES_UKBIO, "rds/PHENO_RATES_UKBIO.rds")
saveRDS(LIST_OF_TRAITS_UKBIO, "rds/LIST_OF_TRAITS_UKBIO.rds")
saveRDS(GWAS_UKBIO_MAPPING, "rds/GWAS_UKBIO_MAPPING.rds")
saveRDS(xchrom_map_colored, "rds/xchrom_map_colored.rds")
saveRDS(gene_stat_table, "rds/gene_stat_table.rds")

# Remove variables
#rm(x_expr, x_expr_tauplus, pheno_conv_UKBIO_list,
#   pheno_conv_GWAS_list, GWAS_ASSOCIATIONS,
#   LIST_OF_TRAITS_GWAS, PHENO_RATES_UKBIO,
#   LIST_OF_TRAITS_UKBIO, GWAS_UKBIO_MAPPING,
#   xchrom_map_colored, gene_stat_table)