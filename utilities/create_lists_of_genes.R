# Creates lists of genes in each study / biomart
#all_genes <- unique(unique(c(x_expr_mod$GENE, TukGTExMod$`Gene name`,cott_carr_will_df$gene),
#                           kat_lin_df$gene))

# Get x genes from biomart
#library(biomaRt)
#mart <- useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")
#results <- getBM(attributes = c("chromosome_name", "hgnc_symbol"),
#                 filters = "chromosome_name", values = "X", mart = mart)
#Xgenes <- results$hgnc_symbol[results$hgnc_symbol != ""]
#rm(mart, results)
Xgenes <- readRDS("rds/Xgenes.rds")

# Get study genes
study1_genes <- unique(x_expr_mod$GENE)
study2_genes <- cott_carr_will_df$gene[cott_carr_will_df$status_cott != "NA"]
study3_genes <- cott_carr_will_df$gene[cott_carr_will_df$status_carrwill != "NA"]
study4_genes <- unique(kat_lin_df_lb$gene)
study5_genes <- unique(kat_lin_df_fb$gene)
study6_genes <- unique(TukGTExMod$`Gene name`)
study7_genes <- unique(cotton_mDNA$GENE)
study8_genes <- unique(balbrown_mCEMT$GENE)
study9_genes <- unique(balbrown_CREST$GENE)

# All genes
all_genes <- unique(c(Xgenes, study1_genes, study2_genes, study3_genes,
                    study4_genes, study5_genes, study6_genes, study7_genes,
                    study8_genes, study9_genes))

# Save rds 
#saveRDS(Xgenes, "rds/Xgenes.rds")
saveRDS(study1_genes, "rds/study1_genes.rds")
saveRDS(study2_genes, "rds/study2_genes.rds")
saveRDS(study3_genes, "rds/study3_genes.rds")
saveRDS(study4_genes, "rds/study4_genes.rds")
saveRDS(study5_genes, "rds/study5_genes.rds")
saveRDS(study6_genes, "rds/study6_genes.rds")
saveRDS(study7_genes, "rds/study7_genes.rds")
saveRDS(study8_genes, "rds/study8_genes.rds")
saveRDS(study9_genes, "rds/study9_genes.rds")
saveRDS(all_genes, "rds/all_genes.rds")
