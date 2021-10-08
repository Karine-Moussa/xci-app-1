# Consolidate all escape states for each gene in a table
# to minimize loading speed for tab 2 in application.
# Also summarizes studies statistics

num_of_studies = 10 # change here (when a new study is added)

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


# change here (when adding study)

# Organize GENE | STUDY | STATE for each study
# STUDY 1
study1_dt <- data.table()
study1_dt$GENE <- x_expr_mod$GENE
study1_dt$STUDY <- "Sauteraud et al. GEUVADIS (lymphoblast)"
study1_dt$STATE <- x_expr_mod$status_adv
study1_dt$STATE <- ifelse(study1_dt$STATE == "E", "escape", 
                          ifelse(study1_dt$STATE == "S", "inactive",
                                 ifelse(study1_dt$STATE == "V", "variable","")))
study1_dt <- unique(study1_dt)

# STUDY 2
study2_dt <- data.table()
study2_dt$GENE <- cott_carr_will_df$gene
study2_dt$STUDY <- "Cotton et al. (lymphoblast & fibroblast)"
study2_dt$STATE <- cott_carr_will_df$status_cott
study2_dt <- unique(study2_dt)

# STUDY 3
study3_dt <- data.table()
study3_dt$GENE <- cott_carr_will_df$gene
study3_dt$STUDY <- "Carrel/Willard (hybrid fibroblast)"
study3_dt$STATE <- cott_carr_will_df$status_carrwill
study3_dt <- unique(study3_dt)

# STUDY 4
study4_dt <- data.table()
study4_dt$GENE <- kat_lin_df_lb$gene
study4_dt$STUDY <- "Katsir + Linial (lymphoblast)"
study4_dt$STATE <- kat_lin_df_lb$status_lb
study4_dt <- unique(study4_dt)

# STUDY 5
study5_dt <- data.table()
study5_dt$GENE <- kat_lin_df_fb$gene
study5_dt$STUDY <- "Katsir + Linial (fibroblast)"
study5_dt$STATE <- kat_lin_df_fb$status_fb
study5_dt <- unique(study5_dt)

# STUDY 6
study6_dt <- data.table()
study6_dt$GENE <- TukGTExMod$`Gene name`
study6_dt$STUDY <- "Tukiainen et al. Fully Skewed Female (multi-tissue)"
study6_dt$STATE <- TukGTExMod$status_adv
study6_dt$STATE <- ifelse(study6_dt$STATE == "E", "escape", 
                          ifelse(study6_dt$STATE == "S", "inactive",
                                 ifelse(study6_dt$STATE == "V", "variable","")))
study6_dt <- unique(study6_dt)

# STUDY 7
study7_dt <- data.table()
study7_dt$GENE <- cotton_mDNA_merge_b$GENE
study7_dt$STUDY <- "Cotton et al. mDNA (multi-tissue)"
study7_dt$STATE <- cotton_mDNA_merge_b$STATUS
study7_dt <- unique(study7_dt)

# STUDY 8
study8_dt <- data.table()
study8_dt$GENE <- balbrown_mCEMT$GENE
study8_dt$STUDY <- "Balaton + Brown DNAme (Cancer Cells)"
study8_dt$NUM_TISS <- 6

# STUDY 9
study9_dt <- data.table()
study9_dt$GENE <- balbrown_CREST$GENE
study9_dt$STUDY <- "Balaton + Brown Epigenetic Predictor (CREST)"
study9_dt$STATE <- balbrown_CREST$STATUS

# STUDY 10
study10_dt <- data.table()
study10_dt$GENE <- TukDEG$GENE
study10_dt$STUDY <- "Tukiainen et al. Male-Female DGEA (multi-tissue)"
study10_dt$STATE <- TukDEG$BIAS

# change here (add the study)

# Get list of all genes
if (!exists("all_genes")){
    all_genes <- unique(c(study1_dt$GENE, study2_dt$GENE, study7_dt$GENE, 
                          study3_dt$GENE,study4_dt$GENE, study5_dt$GENE, 
                          study6_dt$GENE, study10_dt$GENE, study8_dt$GENE,
                          study9_dt$GENE)) # change here
}

# Create meta data frame
studies_string <- c(study1_dt$STUDY[1], study2_dt$STUDY[1], study7_dt$STUDY[1],
                    study3_dt$STUDY[1], study4_dt$STUDY[1], study5_dt$STUDY[1], 
                    study6_dt$STUDY[1], study10_dt$STUDY[1], study8_dt$STUDY[1],
                    study9_dt$STUDY[1]) # change here
studies <- list(study1_dt, study2_dt, study7_dt, study3_dt, study4_dt, study5_dt, 
                study6_dt, study10_dt, study8_dt, study9_dt) # change here
meta_dt <- data.table()
for (gene in all_genes){
    gene_dt <- data.table()
    gene_dt$GENE <- rep(gene, num_of_studies)
    gene_dt$STUDY <- studies_string
    # Get statuses for each study
    status_entries <- list()
    for (study in studies){
        if(gene %in% study$GENE){
            status_entry <- study[GENE == gene, STATE]
            status_entry <- paste(unlist(status_entry), collapse = ", ")
            # ^this takes care of cases when the escape status
            # varies across samples.
        } else {
            status_entry <- "NA"
        }
        status_entries <- c(status_entries, status_entry)
    }
    gene_dt$STATUS <- status_entries
    meta_dt <- rbind(meta_dt, gene_dt)
}

# Save as an RDS object
saveRDS(meta_dt, "rds/meta_dt.rds")
