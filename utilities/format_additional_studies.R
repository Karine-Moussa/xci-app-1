# Libraries
library(readxl)
library(readr)
##################################
#### IMPORT AND FORMAT FILES
##################################
# TUK ET AL 2017 (which combined cotton et al and carrel/willard)
tuketal_suppl_table_1 <- read.csv("resources_studies/Tuketal2017/Suppl.Table.1.csv", header=F,na.strings="?")
tuketal_suppl_table_1 <- tuketal_suppl_table_1[-1,]  # remove top row
names(tuketal_suppl_table_1) <- tuketal_suppl_table_1[1,] # make "new top row" the headers
tuketal_suppl_table_1 <- tuketal_suppl_table_1[-1,] # remove "new top row"
tuketal_suppl_table_1 <- tuketal_suppl_table_1[,-c(15:ncol(tuketal_suppl_table_1))] # remove NA colums
tuketal_suppl_table_1_combined <- tuketal_suppl_table_1[,1:7]       # section suppl table
tuketal_suppl_table_1_combined <- tuketal_suppl_table_1_combined[-which(tuketal_suppl_table_1_combined == ""), ] # remove blank rows
tuketal_suppl_table_1_cottonetal <- tuketal_suppl_table_1[,8:11]    # section suppl table
tuketal_suppl_table_1_cottonetal <- tuketal_suppl_table_1_cottonetal[-which(tuketal_suppl_table_1_cottonetal == ""), ] # remove blank rows
tuketal_suppl_table_1_carrwillard <- tuketal_suppl_table_1[,12:14]  # section suppl table
tuketal_suppl_table_1_carrwillard <- tuketal_suppl_table_1_carrwillard[-which(tuketal_suppl_table_1_carrwillard == ""), ] # remove blank rows
rm(tuketal_suppl_table_1)   # remove full tuketal_suppl_table_1
hg19_to_hg38_cotcar <- read.csv("resources_studies/Tuketal2017/hg19_to_hg38_cottcar.csv")
hg19_to_hg38_cotcar <- hg19_to_hg38_cotcar[hg19_to_hg38_cotcar$recip != "Second Pass",] # for now remove repeated mapping

# TUK ET AL 2017 (Tuk et al study GTEx findings)
TukGTEx <- read_xlsx("resources_studies/Tuketal2017/TukSupTables/Suppl.Table.5.xlsx")
saveRDS(TukGTEx, "rds/TukGTEx.rds")
TukGTEx <- readRDS("rds/TukGTEx.rds")
TukGTExMod <- TukGTEx
hg19_to_hg38_tuk <- read.csv("resources_studies/Tuketal2017/hg19_to_hg38_tuk.csv")

# MERIT ET AL 2020 (the GTEx papers)
path <- "resources_studies/Meritxelletal2020/aba3066-Table-S3.xlsx"
meritetal_suppl_table_3_top30 <- read_excel(path, sheet = "Top30_autosomal_predictive_gene")
meritetal_suppl_table_3_top100 <- read_excel(path, sheet = "Top100_autosomal_predictive_acr")
rm(path)

# Katsir + Linial 2019 scRNA-seq study
path <- "resources_studies/KatsirLinial2019/table_s3_mod.xlsx"
katsir_linail_s3 <- read_excel(path, sheet = "ChrX")
hg19_to_hg38_katlin<- read.csv("resources_studies/KatsirLinial2019/hg19_to_hg38.csv")
hg19_to_hg38_katlin <- hg19_to_hg38_katlin[hg19_to_hg38_katlin$recip != "Second Pass",] # for now remove repeated mapping
rm(path)

# USER UPLOADED STUDY (created a blank rds object)
# see utilites/create_study0_df.R for creation of default blank study0_df

## MANUALLY ADDED STUDIES (for DISEASE/TRAITS data)
MANUAL_STUDIES <- read_excel("data_intermediate/additional_findings.xlsx", sheet = "additional_findings")


##################################
### PREPARE FILES FOR USE IN CODE
##################################
# TUK ET AL 2017 (the combind cotton et al and carrel/willard dataset)
# first need to map the start positions from hg19 to hg38
cott_carr_will_df <- data.frame(gene = tuketal_suppl_table_1_combined$`Gene name`,
                                gene_mapped = rep("",length(tuketal_suppl_table_1_combined$`Gene name`)),
                                start = as.numeric(tuketal_suppl_table_1_combined$`Start position`),
                                start_mapped = rep("",length(tuketal_suppl_table_1_combined$`Start position`)),
                                end = as.numeric(tuketal_suppl_table_1_combined$`End position`),
                                end_mapped = rep("",length(tuketal_suppl_table_1_combined$`End position`)),
                                status = tuketal_suppl_table_1_combined$`Combined XCI status`,
                                status_cott = tuketal_suppl_table_1_cottonetal$`XCI status`,
                                status_carrwill = tuketal_suppl_table_1_carrwillard$`XCI status.1`,
                                color = rep("",nrow(tuketal_suppl_table_1_combined)),
                                color_cott = rep("",nrow(tuketal_suppl_table_1_combined)),
                                color_carrwill = rep("",nrow(tuketal_suppl_table_1_combined))
                                )
                              
# Get the remapped versions
for(i in 1:length(cott_carr_will_df$start)){
    pos_start <- cott_carr_will_df$start[i]
    mapped_pos_start <- ifelse(pos_start %in% hg19_to_hg38_cotcar$source_start,
                               mapped_pos_start <- hg19_to_hg38_cotcar[hg19_to_hg38_cotcar$source_start == pos_start & !is.na(hg19_to_hg38_cotcar$source_start), "mapped_start"],
                               mapped_pos_start <- NA)
    ifelse(mapped_pos_start %in% x_expr$start,
           mapped_gene <- unique(x_expr[x_expr$start == mapped_pos_start, "GENE"]),
           mapped_gene <- NA)
    pos_stop <- cott_carr_will_df$end[i]
    mapped_pos_stop <- ifelse(pos_stop %in% hg19_to_hg38_cotcar$source_stop,
                               mapped_pos_stop <- hg19_to_hg38_cotcar[hg19_to_hg38_cotcar$source_stop == pos_stop & !is.na(hg19_to_hg38_cotcar$source_stop), "mapped_stop"],
                               mapped_pos_stop <- NA)
    cott_carr_will_df$start_mapped[i] <- mapped_pos_start
    cott_carr_will_df$end_mapped[i] <- mapped_pos_stop
    cott_carr_will_df$gene_mapped[i] <- mapped_gene
}
cott_carr_will_df$start_mapped <- as.numeric(cott_carr_will_df$start_mapped)
rm(hg19_to_hg38_cotcar)

# Change the nomenclature for Cotton et al. study
cott_carr_will_df$status_cott <- ifelse(cott_carr_will_df$status_cott == "variable escape", "variable",
                                        ifelse(cott_carr_will_df$status_cott == "subject", "inactive",
                                               ifelse(cott_carr_will_df$status_cott == "escape", "escape", "NA")))

# Get color
col_escape = "purple"
col_variable = "turquoise3"
col_inactive = "lightsteelblue3"
col_na = "white"
col_check = "green"
for (i in 1:nrow(cott_carr_will_df)){
    # Combined studies, colors
    if(cott_carr_will_df$status[i] == "escape"){
        cott_carr_will_df$color[i] = col_escape
    } else if(cott_carr_will_df$status[i] == "variable"){
        cott_carr_will_df$color[i] = col_variable
    } else if(cott_carr_will_df$status[i] == "inactive"){
        cott_carr_will_df$color[i] = col_inactive
    } else if(cott_carr_will_df$status[i] == "NA"){
        cott_carr_will_df$color[i] = col_na
    } else {
        cott_carr_will_df$color[i] = col_check
    }
    # Cotton colors
    if(cott_carr_will_df$status_cott[i] == "escape"){
        cott_carr_will_df$color_cott[i] = col_escape
    } else if(cott_carr_will_df$status_cott[i] == "variable"){
        cott_carr_will_df$color_cott[i] = col_variable
    } else if(cott_carr_will_df$status_cott[i] == "inactive"){
        cott_carr_will_df$color_cott[i] = col_inactive
    } else if(cott_carr_will_df$status_cott[i] == "NA"){
        cott_carr_will_df$color_cott[i] = col_na
    } else {
        cott_carr_will_df$color_cott[i] = col_check
    }
    # Carrell / Willard colors
    if(cott_carr_will_df$status_carrwill[i] == "escape"){
        cott_carr_will_df$color_carrwill[i] = col_escape
    } else if(cott_carr_will_df$status_carrwill[i] == "variable"){
        cott_carr_will_df$color_carrwill[i] = col_variable
    } else if(cott_carr_will_df$status_carrwill[i] == "inactive"){
        cott_carr_will_df$color_carrwill[i] = col_inactive
    } else if(cott_carr_will_df$status_carrwill[i] == "NA"){
        cott_carr_will_df$color_carrwill[i] = col_na
    } else {
        cott_carr_will_df$color_carrwill[i] = col_check
    }
}

# Tuk et al 2017 (GTEx data)
# reformat the IDs
split_ids <- function(id) {
    return(unlist(strsplit(id, "[.]"))[1])
}
geneid_clean <- unlist(lapply(TukGTEx$`Gene ID`, split_ids))
TukGTExMod$`Gene ID clean` <- geneid_clean

# reformat the positions
split_pos <- function(pos) {
    return(unlist(strsplit(pos, "[:]"))[2])
}
pos_clean <- unlist(lapply(TukGTEx$`chr:pos`, split_pos))
TukGTExMod$`Pos clean` <- as.numeric(pos_clean)

# remap positions
TukGTExMod$`start_mapped` <- as.numeric(hg19_to_hg38_tuk$mapped_start)
rm(hg19_to_hg38_tuk)

# Determine the "variable" state of genes in gtex data set
# First get percent of samples for which gene escaped
TukGTExMod$status_adv <- rep("",nrow(TukGTExMod))
TukGTExMod$color <- rep("",nrow(TukGTExMod))
TukGTExMod$perc_tissues_esc <- rep("",nrow(TukGTExMod))
for(gene in unique(TukGTExMod$`Gene name`)){
    perc_tissues_esc = mean(TukGTExMod[TukGTExMod$`Gene name` == gene, "Incomplete XCI"] == "TRUE")
    if(perc_tissues_esc < SV_threshold){
        st = "S"
        color = "lightsteelblue3"
    } else if (perc_tissues_esc >= SV_threshold & perc_tissues_esc < VE_threshold) {
        st = "V"
        color = "turquoise3"
    } else if (perc_tissues_esc >= VE_threshold){
        st = "E"
        color = "purple"
    }
    # Add as columns to TukGTExMod
    for(i in 1:nrow(TukGTExMod)){
        if(TukGTExMod$`Gene name`[i] == gene){
            TukGTExMod$status_adv[i] <- st
            TukGTExMod$color[i] <- color
            TukGTExMod$perc_tissues_esc[i] <- perc_tissues_esc
        }
    }
}

## Katsir + Linial 2019
kat_lin_df <- data.frame(gene = katsir_linail_s3$`geneSymbol`,
                         gene_mapped = rep("",length(katsir_linail_s3$`geneSymbol`)),
                         snp_fb = katsir_linail_s3$SNPs_per_Gene.fib,
                         snp_lb = katsir_linail_s3$SNPs_per_Gene.lymph,
                         start = as.numeric(katsir_linail_s3$`start`),
                         start_mapped = rep("",length(katsir_linail_s3$`start`)),
                         start_name_based = rep("",length(katsir_linail_s3$`geneSymbol`)),
                         end = as.numeric(katsir_linail_s3$`end`),
                         end_mapped = rep("",length(katsir_linail_s3$`end`)),
                         status_fb_raw = katsir_linail_s3$`iSNP_Protocol_Identification.fib`,
                         status_fb = ifelse(katsir_linail_s3$`iSNP_Protocol_Identification.fib` == "Escaper", "escape",
                                            ifelse(katsir_linail_s3$`iSNP_Protocol_Identification.fib` == "Inactivated","inactive",
                                                   ifelse(katsir_linail_s3$`iSNP_Protocol_Identification.fib` == "less than 2 iSNPs","",""))),
                         status_lb_raw = katsir_linail_s3$`overall_Identification`,
                         status_lb = rep("",length(katsir_linail_s3$`overall_Identification`)),
                         color_fb = ifelse(katsir_linail_s3$`iSNP_Protocol_Identification.fib` == "Escaper", col_escape,
                                           ifelse(katsir_linail_s3$`iSNP_Protocol_Identification.fib` == "Inactivated",
                                                  col_inactive,NA)),
                         color_lb = rep("",length(katsir_linail_s3$`geneSymbol`)))

# get status_lb and color_lb
for (i in 1:nrow(kat_lin_df)){
    status = ""
    color = NA
    if(grepl("Escaper",kat_lin_df$status_lb_raw[i])){
        status <- "escape"
        color <- col_escape
    }
    if(grepl("Inactivated",kat_lin_df$status_lb_raw[i])){
        status <- "inactive"
        color <- col_inactive
    }
    kat_lin_df$status_lb[i] <- status
    kat_lin_df$color_lb[i] <- color
}
rm(status)

# Get mapped position of each gene
for(i in 1:nrow(kat_lin_df)){
    start_orig <- kat_lin_df$start[i]
    end_orig <- kat_lin_df$end[i]
    kat_lin_df$start_mapped[i] <- hg19_to_hg38_katlin[hg19_to_hg38_katlin$source_start == start_orig, "mapped_start"]
    kat_lin_df$end_mapped[i] <- hg19_to_hg38_katlin[hg19_to_hg38_katlin$source_stop == end_orig, "mapped_stop"]
}
kat_lin_df$start_mapped <- as.numeric(kat_lin_df$start_mapped)
kat_lin_df$end_mapped <- as.numeric(kat_lin_df$end_mapped)

# Split table based on cell type, and remove fb_snp or lb_snp column
kat_lin_df_fb <- subset(kat_lin_df, snp_fb != "NA")
kat_lin_df_lb <- subset(kat_lin_df, snp_lb != "NA")

## Study 7: Cotton 2014
cotton_mDNA <- readRDS("resources_studies/Cotton2014/cotton_mDNA_precolor.rds")
for (row_num in 1:nrow(cotton_mDNA)){
    if (cotton_mDNA$STATUS[row_num] == "escape"){row_color = col_escape}
    if (cotton_mDNA$STATUS[row_num] == "variable"){row_color = col_variable}
    if (cotton_mDNA$STATUS[row_num] == "inactive"){row_color = col_inactive}
    cotton_mDNA$COLOR[row_num] = row_color
}

# Cleanup
rm(col_check, col_escape, col_variable, col_inactive, col_na)

# Save as RDS for easy compiling
saveRDS(MANUAL_STUDIES, "rds/MANUAL_STUDIES.rds")
saveRDS(cott_carr_will_df, "rds/cott_carr_will_df.rds")
saveRDS(kat_lin_df, "rds/kat_lin_df.rds")
saveRDS(kat_lin_df_fb, "rds/kat_lin_df_fb.rds")
saveRDS(kat_lin_df_lb, "rds/kat_lin_df_lb.rds")
saveRDS(TukGTExMod, "rds/TukGTExMod.rds")
saveRDS(cotton_mDNA, "rds/cotton_mDNA.rds")