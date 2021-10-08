# Create meta stat dt

# STUDY NUMBER | STUDY NAME | NUM_CALLS | NUM_GENES | NUM_SAMPLES

library(data.table)
# Read data tables for all studies
# STUDY 1
if (!exists("x_expr_mod")) {
    x_expr_mod <- readRDS("rds/x_expr_mod.rds")
}
# STUDY 2 & STUDY 3
if (!exists("cott_carr_will_df")) {
    cott_carr_will_df <- readRDS("rds/cott_carr_will_df.rds")
}
# STUDY 4
if (!exists("kat_lin_df_fb")) {
    kat_lin_df_fb <- readRDS("rds/kat_lin_df_fb.rds")
}
# STUDY 5
if (!exists("kat_lin_df_lb")) {
    kat_lin_df_lb <- readRDS("rds/kat_lin_df_lb.rds")
}
# STUDY 6
if (!exists("TukGTExMod")) {
    TukGTExMod <- readRDS("rds/TukGTExMod.rds")
}
# STUDY 7
if (!exists("cotton_mDNA_merge_b")) {
    cotton_mDNA_merge_b <- readRDS("rds/cotton_mDNA_merge_b.rds")
}
# STUDY 8
if (!exists("balbrown_mCEMT")) {
    balbrown_mCEMT <- readRDS("rds/balbrown_mCEMT.rds")
}
# STUDY 9
if (!exists("balbrown_CREST")) {
    balbrown_CREST <- readRDS("rds/balbrown_CREST.rds")
}
# STUDY 10
if (!exists("TukDEG")) {
    TukDEG<- readRDS("rds/TukDEG.rds")
}


# GET STATS FOR EACH STUDY
# STUDY 1
study1_dt <- data.table()
study1_dt$NUMBER <- 1
study1_dt$STUDY <- "Sauteraud et al. GEUVADIS (lymphoblast)"
study1_dt$NUM_CALLS <- nrow(x_expr_mod)
study1_dt$NUM_GENES <- length(unique(x_expr_mod$GENE))
study1_dt$NUM_TISS <- 1
study1_dt <- unique(study1_dt)

# STUDY 2
study2_dt <- data.table()
study2_dt$NUMBER <- 2
study2_dt$STUDY <- "Cotton et al. (lymphoblast & fibroblast)"
study2_dt$NUM_CALLS <-  sum(cott_carr_will_df$status_cott != "NA")
study2_dt$NUM_GENES <- length(unique(cott_carr_will_df$gene[cott_carr_will_df$status_cott != "NA"]))
study2_dt$NUM_TISS <- 2
study2_dt <- unique(study2_dt)

# STUDY 3
study3_dt <- data.table()
study3_dt$NUMBER <- 3
study3_dt$STUDY <- "Carrel/Willard (hybrid fibroblast)"
study3_dt$NUM_CALLS <- sum(cott_carr_will_df$status_carrwill != "NA")
study3_dt$NUM_GENES <- length(unique(cott_carr_will_df$gene[cott_carr_will_df$status_carrwill != "NA"]))
study3_dt$NUM_TISS <- 1
study3_dt <- unique(study3_dt)

# STUDY 4
study4_dt <- data.table()
study4_dt$NUMBER <- 4
study4_dt$STUDY <- "Katsir + Linial (lymphoblast)"
study4_dt$NUM_CALLS <- nrow(kat_lin_df_lb)
study4_dt$NUM_GENES <- length(unique(kat_lin_df_lb$gene))
study4_dt$NUM_TISS <- 1
study4_dt <- unique(study4_dt)

# STUDY 5
study5_dt <- data.table()
study5_dt$NUMBER <- 5
study5_dt$STUDY <- "Katsir + Linial (fibroblast)"
study5_dt$NUM_CALLS <- nrow(kat_lin_df_fb)
study5_dt$NUM_GENES <- length(unique(kat_lin_df_fb$gene))
study5_dt$NUM_TISS <- 1
study5_dt <- unique(study5_dt)

# STUDY 6
study6_dt <- data.table()
study6_dt$NUMBER <- 6
study6_dt$STUDY <- "Tukiainen et al. Fully Skewed Female (multi-tissue)"
study6_dt$NUM_CALLS <- nrow(TukGTExMod)
study6_dt$NUM_GENES <- length(unique(TukGTExMod$`Gene name`))
study6_dt$NUM_TISS <- 16
study6_dt <- unique(study6_dt)

# STUDY 7
study7_dt <- data.table()
study7_dt$NUMBER <- 7
study7_dt$STUDY <- "Cotton et al. mDNA (multi-tissue)"
study7_dt$NUM_CALLS <- nrow(cotton_mDNA_merge_b)
study7_dt$NUM_GENES <- length(unique(cotton_mDNA_merge_b$GENE))
study7_dt$NUM_TISS <- 27
study7_dt <- unique(study7_dt)

# STUDY 8
study8_dt <- data.table()
study8_dt$NUMBER <- 8
study8_dt$STUDY <- "Balaton + Brown DNAme (Cancer Cells)"
study8_dt$NUM_CALLS <- nrow(balbrown_mCEMT)
study8_dt$NUM_GENES <- length(unique(balbrown_mCEMT$GENE))
study8_dt$NUM_TISS <- 6

# STUDY 9
study9_dt <- data.table()
study9_dt$NUMBER <- 9
study9_dt$STUDY <- "Balaton + Brown Epigenetic Predictor (CREST)"
study9_dt$NUM_CALLS <- nrow(balbrown_CREST)
study9_dt$NUM_GENES <- length(unique(balbrown_CREST$GENE))
study9_dt$NUM_TISS <- 2

# STUDY 10
study10_dt <- data.table()
study10_dt$NUMBER <- 10
study10_dt$STUDY <- "Tukiainen et al. Male-Female DGEA (multi-tissue)"
study10_dt$NUM_CALLS <- nrow(TukDEG)
study10_dt$NUM_GENES <- length(unique(TukDEG$GENE))
study10_dt$NUM_TISS <- 29


# Merge all dts
meta_stat_dt <- rbind(study1_dt, study2_dt, study3_dt, study4_dt,
                      study5_dt, study6_dt, study7_dt, study8_dt,
                      study9_dt, study10_dt)

rm(study1_dt, study2_dt, study3_dt, study4_dt,
   study5_dt, study6_dt, study7_dt, study8_dt,
   study9_dt, study10_dt)

# Save RDS
saveRDS(meta_stat_dt, "rds/meta_stat_dt.rds")
