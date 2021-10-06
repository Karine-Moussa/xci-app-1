# This script creates the gene hyperlinks for all studies

## Comment out this section when not in development mode ##
setwd("~/xci-app-1-symlink")
cott_carr_will_df <- readRDS("rds/cott_carr_will_df.rds")
kat_lin_df <- readRDS("rds/kat_lin_df.rds")
kat_lin_df_fb <- readRDS("rds/kat_lin_df_fb.rds")
kat_lin_df_lb <- readRDS("rds/kat_lin_df_lb.rds")
TukGTExMod <- readRDS("rds/TukGTExMod.rds")
TukDEG <- readRDS("rds/TukDEG.rds")
cotton_mDNA <- readRDS("rds/cotton_mDNA.rds")
balbrown_mCEMT <- readRDS("rds/balbrown_mCEMT.rds")
balbrown_CREST <- readRDS("rds/balbrown_CREST.rds")
ens_base_loc <- readRDS("rds/ens_base_loc.rds")
ens_base_gene <- readRDS("rds/ens_base_gene.rds")

# STUDY 2 Cotton lymphoblast/fibroblast (by location)
cott_carr_will_df$gene_link <- rep(ens_base_loc,
                                   times = nrow(cott_carr_will_df))
for (i in 1:nrow(cott_carr_will_df)){
    cott_carr_will_df$gene_link[i] = paste0(ens_base_loc, 
                                            cott_carr_will_df$start_mapped[i],
                                            "-",
                                            cott_carr_will_df$end_mapped[i])
}


# Save as RDS for easy compiling
saveRDS(MANUAL_STUDIES, "rds/MANUAL_STUDIES.rds")
saveRDS(cott_carr_will_df, "rds/cott_carr_will_df.rds")
saveRDS(kat_lin_df, "rds/kat_lin_df.rds")
saveRDS(kat_lin_df_fb, "rds/kat_lin_df_fb.rds")
saveRDS(kat_lin_df_lb, "rds/kat_lin_df_lb.rds")
saveRDS(TukGTExMod, "rds/TukGTExMod.rds")
saveRDS(TukDEG, "rds/TukDEG.rds")
saveRDS(cotton_mDNA, "rds/cotton_mDNA.rds")
saveRDS(balbrown_mCEMT, "rds/balbrown_mCEMT.rds")
saveRDS(balbrown_CREST, "rds/balbrown_CREST.rds")
