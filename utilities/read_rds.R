# Reads RDS objects

# format_input_data.R
x_expr <- readRDS("rds/x_expr.rds")
x_expr_tauplus <- readRDS("rds/x_expr_tauplus.rds")
#pheno_conv_UKBIO_list <- readRDS("rds/pheno_conv_UKBIO_list.rds")
#pheno_conv_GWAS_list <- readRDS("rds/pheno_conv_GWAS_list.rds")
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
ens_base_loc <- readRDS("rds/ens_base_loc.rds")
ens_base_gene <- readRDS("rds/ens_base_gene.rds")

# x_expr_mods.R
x_expr_mod <- readRDS("rds/x_expr_mod.rds")

# create_gene_links.R
MANUAL_STUDIES <- readRDS("rds/MANUAL_STUDIES.rds")
cott_carr_will_df <- readRDS("rds/cott_carr_will_df.rds")
kat_lin_df <- readRDS("rds/kat_lin_df.rds")
kat_lin_df_fb <- readRDS("rds/kat_lin_df_fb.rds")
kat_lin_df_lb <- readRDS("rds/kat_lin_df_lb.rds")
TukGTExMod <- readRDS("rds/TukGTExMod.rds")
TukDEG <- readRDS("rds/TukDEG.rds")
cotton_mDNA <- readRDS("rds/cotton_mDNA.rds")
balbrown_mCEMT <- readRDS("rds/balbrown_mCEMT.rds")
balbrown_CREST <- readRDS("rds/balbrown_CREST.rds")

# format_plot_aesthetics.R
x_labels_genes_start <- readRDS("rds/x_labels_genes_start.rds")
x_labels_genes_mid <- readRDS("rds/x_labels_genes_mid.rds")
x_labels_genes <- readRDS("rds/x_labels_genes.rds")
x_labels_pos <- readRDS("rds/x_labels_pos.rds")
x_labels_bp <- readRDS("rds/x_labels_bp.rds")
x_labels_genes <- readRDS("rds/x_labels_genes.rds")
x_labels_pos <- readRDS("rds/x_labels_pos.rds")
x_region_breaks <- readRDS("rds/x_region_breaks.rds")
x_region_labels <- readRDS("rds/x_region_labels.rds")
par1_boundaries <- readRDS("rds/par1_boundaries.rds")
par2_boundaries <- readRDS("rds/par2_boundaries.rds")
centre_boundaries <- readRDS("rds/centre_boundaries.rds")
chrom_segments <- readRDS("rds/chrom_segments.rds")
chrom_segments_colored <- readRDS("rds/chrom_segments_colored.rds")

# create_lists_of_genes.R
Xgenes <- readRDS("rds/Xgenes.rds")
study1_genes <- readRDS("rds/study1_genes.rds")
study2_genes <- readRDS("rds/study2_genes.rds")
study3_genes <- readRDS("rds/study3_genes.rds")
study4_genes <- readRDS("rds/study4_genes.rds")
study5_genes <- readRDS("rds/study5_genes.rds")
study6_genes <- readRDS("rds/study6_genes.rds")
study7_genes <- readRDS("rds/study7_genes.rds")
study8_genes <- readRDS("rds/study8_genes.rds")
study9_genes <- readRDS("rds/study9_genes.rds")
study10_genes <- readRDS("rds/study9_genes.rds")
all_genes <- readRDS("rds/all_genes.rds")

# consolidate_escape_states.R
meta_dt <- readRDS("rds/meta_dt.rds")
