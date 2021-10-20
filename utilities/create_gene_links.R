# This script creates the gene hyperlinks for all studies

## Comment out this section when not in development mode ##
setwd("~/xci-app-1-symlink")
x_expr_mod <- readRDS("rds/x_expr_mod.rds") # done
cott_carr_will_df <- readRDS("rds/cott_carr_will_df.rds") # done
kat_lin_df <- readRDS("rds/kat_lin_df.rds") # done
kat_lin_df_fb <- readRDS("rds/kat_lin_df_fb.rds") # done
kat_lin_df_lb <- readRDS("rds/kat_lin_df_lb.rds") # done
TukGTExMod <- readRDS("rds/TukGTExMod.rds") # done
TukDEG <- readRDS("rds/TukDEG.rds") # done
cotton_mDNA <- readRDS("rds/cotton_mDNA.rds") ####### need end positions
balbrown_mCEMT <- readRDS("rds/balbrown_mCEMT.rds") # done
balbrown_CREST <- readRDS("rds/balbrown_CREST.rds") # done

# biomaRt
library(biomaRt)
#ensembl <- useEnsembl(biomart = "genes", 
#                      dataset = "hsapiens_gene_ensembl") 
                      #mirror = "useast")
#saveRDS(ensembl, "rds/ensembl.rds")
ensembl <- readRDS("rds/ensembl.rds")
filters = listFilters(ensembl)
attributes = listAttributes(ensembl)

# Ensemble base links
ens_base_loc <- readRDS("rds/ens_base_loc.rds")
ens_base_gene <- readRDS("rds/ens_base_gene.rds")


# STUDY 1 GEUVADIS by location
for (i in 1:nrow(x_expr_mod)){
    x_expr_mod$gene_link[i] = paste0(ens_base_loc, 
                                     x_expr_mod$start[i],
                                     "-",
                                     x_expr_mod$end[i])
}

# STUDY 2 Cotton lymphoblast/fibroblast 
# STUDY 3 Carrel_Willard hybrid 
# by location
for (i in 1:nrow(cott_carr_will_df)){
    cott_carr_will_df$gene_link[i] = paste0(ens_base_loc, 
                                            cott_carr_will_df$start_mapped[i],
                                            "-",
                                            cott_carr_will_df$end_mapped[i])
}

# STUDY 4 Katsir Linial lymphoblast
# STUDY 5 Katsir Linial fibroblast
for (i in 1:nrow(kat_lin_df)){
    kat_lin_df$gene_link[i] = paste0(ens_base_loc, 
                                     kat_lin_df$start_mapped[i],
                                     "-",
                                     kat_lin_df$end_mapped[i])
}
for (i in 1:nrow(kat_lin_df_fb)){
    kat_lin_df_fb$gene_link[i] = paste0(ens_base_loc, 
                                        kat_lin_df_fb$start_mapped[i],
                                        "-",
                                        kat_lin_df_fb$end_mapped[i])
}
for (i in 1:nrow(kat_lin_df_lb)){
    kat_lin_df_lb$gene_link[i] = paste0(ens_base_loc, 
                                        kat_lin_df_lb$start_mapped[i],
                                     "-",
                                     kat_lin_df_lb$end_mapped[i])
}

# STUDY 5 TukGTEx (fully skewed individual) 
TukGTExMod_genes <- c(TukGTExMod$`Gene ID clean`)
pos_ids_1 = getBM(attributes = c('ensembl_gene_id', 'start_position', 'end_position'), 
            filters =  c('ensembl_gene_id', 'chromosome_name'),
            values = list(genes = c(TukGTExMod_genes), chromosome = "X"), 
              mart = ensembl)
colnames(pos_ids_1) <- c("Gene ID clean", "START", "STOP")
TukGTExMod_merge <- merge(TukGTExMod, pos_ids_1, all.x = T) # TukGTExMod with start and end
# manually fill in the three NA entries
pos_ids_2 = getBM(attributes = c('external_gene_name', 'start_position', 'end_position'), 
                filters =  c('external_gene_name', 'chromosome_name'),
                values = list(genes = c("SLC6A14", "SLC25A53"), chromosome = "X"), 
                mart = ensembl) # manually printed and copied results below
TukGTExMod_merge$START[TukGTExMod_merge$`Gene name` == "SLC6A14"] <- 116436606
TukGTExMod_merge$END[TukGTExMod_merge$`Gene name` == "SLC6A14"] <- 116461458
TukGTExMod_merge$START[TukGTExMod_merge$`Gene name` == "SLC25A53"] <- 104099214
TukGTExMod_merge$END[TukGTExMod_merge$`Gene name` == "SLC25A53"] <- 104157009
# Now get position
for (i in 1:nrow(TukGTExMod_merge)){
    TukGTExMod_merge$gene_link[i] = paste0(ens_base_loc, 
                                           TukGTExMod_merge$START[i],
                                           "-",
                                           TukGTExMod_merge$STOP[i])
}

# STUDY 7 Cotton mDNA
# This study required more involved efforts. 
# START and STOP position additions are in resources_studies/Cotton2014/format_cotton14.R

# pos_ids_3 = getBM(attributes = c('external_gene_name', 'start_position', 'end_position'), 
#                   filters =  c('external_gene_name', 'chromosome_name'),
#                   values = list(genes = c(cotton_mDNA$GENE), chromosome = "X"), 
#                   mart = ensembl)
# colnames(pos_ids_3) <- c("GENE", "START", "STOP")
# cotton_mDNA_merge_a <- merge(cotton_mDNA, pos_ids_3, all.x = T) # TukGTExMod with start and end
# # 47 TSSs may need to have locations manually added
# write.csv(cotton_mDNA_merge_a, "sandbox/cotton_mDNA_merge_a.csv", row.names = FALSE)
# # Current approach: set start and end value as POS
# cotton_mDNA_merge_b <- cotton_mDNA_merge_a
# cotton_mDNA_merge_b$START  <- ifelse(is.na(cotton_mDNA_merge_b$START), cotton_mDNA_merge_b$POS, cotton_mDNA_merge_b$START)  
# cotton_mDNA_merge_b$STOP  <- ifelse(is.na(cotton_mDNA_merge_b$STOP), cotton_mDNA_merge_b$POS, cotton_mDNA_merge_b$STOP)  

# Create gene links
for (i in 1:nrow(cotton_mDNA)){
    cotton_mDNA$gene_link[i] = paste0(ens_base_loc, 
                                      cotton_mDNA$START[i],
                                      "-",
                                      cotton_mDNA$STOP[i])
}

# STUDY 8 Balaton and Brown (mDNA)
for (i in 1:nrow(balbrown_mCEMT)){
    balbrown_mCEMT$gene_link[i] = paste0(ens_base_loc, 
                                         balbrown_mCEMT$START[i],
                                         "-",
                                         balbrown_mCEMT$STOP[i])
}

# STUDY 9 Balaton and Brown (CREST)
for (i in 1:nrow(balbrown_CREST)){
    balbrown_CREST$gene_link[i] = paste0(ens_base_loc, 
                                         balbrown_CREST$START[i],
                                         "-",
                                         balbrown_CREST$STOP[i])
}

# STUDY 10 TukMod (male vs female)
for (i in 1:nrow(TukDEG)){
    TukDEG$gene_link[i] = paste0(ens_base_loc, 
                                 TukDEG$START[i],
                                        "-",
                                 TukDEG$STOP[i])
}

# Save as RDS for easy compiling
saveRDS(x_expr_mod, "rds/x_expr_mod.rds")
saveRDS(cott_carr_will_df, "rds/cott_carr_will_df.rds")
saveRDS(kat_lin_df, "rds/kat_lin_df.rds")
saveRDS(kat_lin_df_fb, "rds/kat_lin_df_fb.rds")
saveRDS(kat_lin_df_lb, "rds/kat_lin_df_lb.rds")
#saveRDS(TukGTExMod, "rds/TukGTExMod.rds")
saveRDS(TukGTExMod_merge, "rds/TukGTExMod_merge.rds") # NEW
saveRDS(TukDEG, "rds/TukDEG.rds")
#saveRDS(cotton_mDNA, "rds/cotton_mDNA.rds")
saveRDS(cotton_mDNA, "rds/cotton_mDNA.rds")
saveRDS(balbrown_mCEMT, "rds/balbrown_mCEMT.rds")
saveRDS(balbrown_CREST, "rds/balbrown_CREST.rds")
