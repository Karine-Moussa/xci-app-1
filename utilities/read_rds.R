# Loads RDS objects

# format_input_data.R
x_expr <- readRDS("rds/x_expr.rds")
x_expr_tauplus <- readRDS("rds/x_expr_tauplus.rds")
pheno_conv_UKBIO_list <- readRDS("rds/pheno_conv_UKBIO_list.rds")
pheno_conv_GWAS_list <- readRDS("rds/pheno_conv_GWAS_list.rds")
pheno_conv_NELSON1_list <- readRDS("rds/pheno_conv_NELSON1_list.rds")
pheno_conv_NELSON2_list <- readRDS("rds/pheno_conv_NELSON2_list.rds")
GWAS_ASSOCIATIONS <- readRDS("rds/GWAS_ASSOCIATIONS.rds")
LIST_OF_TRAITS_GWAS <- readRDS("rds/LIST_OF_TRAITS_GWAS.rds")
PHENO_RATES_UKBIO <- readRDS("rds/PHENO_RATES_UKBIO.rds")
LIST_OF_TRAITS_UKBIO <- readRDS("rds/LIST_OF_TRAITS_UKBIO.rds")
GWAS_UKBIO_MAPPING <- readRDS("rds/GWAS_UKBIO_MAPPING.rds")
xchrom_map_colored <- readRDS("rds/xchrom_map_colored.rds")
gene_stat_table <- readRDS("rds/gene_stat_table.rds")

# create_global_variables.R
STATUS <- readRDS("rds/STATUS.rds")
GENE <- readRDS("rds/GENE.rds")
SAMPLE_NAMES <- readRDS("rds/SAMPLE_NAMES.rds")
LIST_OF_GENES <- readRDS("rds/LIST_OF_GENES.rds")
P_MIN <- readRDS("rds/P_MIN.rds")
P_SIG <- readRDS("rds/P_SIG.rds")
SV_threshold <- readRDS("rds/SV_threshold.rds")
VE_threshold <- readRDS("rds/VE_threshold.rds")

# x_expr_mods.R
x_expr_mod <- readRDS("rds/x_expr_mod.rds")

# format_additonal_studies.R
NELSON_ASSOCIATIONS_1 <- readRDS("rds/NELSON_ASSOCIATIONS_1.rds")
NELSON_ASSOCIATIONS_2 <- readRDS("rds/NELSON_ASSOCIATIONS_2.rds")
LIST_OF_TRAITS_NELSON_1 <- readRDS("rds/LIST_OF_TRAITS_NELSON_1.rds")
LIST_OF_TRAITS_NELSON_2 <- readRDS("rds/LIST_OF_TRAITS_NELSON_2.rds")
MANUAL_STUDIES <- readRDS("rds/MANUAL_STUDIES.rds")
cott_carr_will_df <- readRDS("rds/cott_carr_will_df.rds")
kat_lin_df <- readRDS("rds/kat_lin_df.rds")
kat_lin_df_fb <- readRDS("rds/kat_lin_df_fb.rds")
kat_lin_df_lb <- readRDS("rds/kat_lin_df_lb.rds")

