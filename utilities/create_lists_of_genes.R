# Creates lists of genes in each study
# Create lists
all_genes <- unique(unique(c(x_expr_mod$GENE, TukGTExMod$`Gene name`,cott_carr_will_df$gene),
                           kat_lin_df$gene))
study1_genes <- unique(x_expr_mod$GENE)
study2_genes <- cott_carr_will_df$gene[cott_carr_will_df$status_cott != "NA"]
study3_genes <- cott_carr_will_df$gene[cott_carr_will_df$status_carrwill != "NA"]
study4_genes <- unique(kat_lin_df_lb$gene)
study5_genes <- unique(kat_lin_df_fb$gene)
study6_genes <- unique(TukGTExMod$`Gene name`)

# Save rds 
saveRDS(all_genes, "rds/all_genes.rds")
saveRDS(study1_genes, "rds/study1_genes.rds")
saveRDS(study2_genes, "rds/study2_genes.rds")
saveRDS(study3_genes, "rds/study3_genes.rds")
saveRDS(study4_genes, "rds/study4_genes.rds")
saveRDS(study5_genes, "rds/study5_genes.rds")
saveRDS(study6_genes, "rds/study6_genes.rds")