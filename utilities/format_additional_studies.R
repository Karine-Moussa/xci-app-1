# Libraries
library(readxl)
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
hg19_to_hg38 <- read.csv("resources_studies/Tuketal2017/hg19_to_hg38.csv")
#hg19_to_hg38 <- hg19_to_hg38[hg19_to_hg38$mapped_int != "NULL",] # remove NULL lines
hg19_to_hg38 <- hg19_to_hg38[hg19_to_hg38$recip != "Second Pass",] # for now remove repeated mapping


# MERIT ET AL 2020
meritetal_suppl_table_3_path <- "resources_studies/Meritxelletal2020/aba3066-Table-S3.xlsx"
meritetal_suppl_table_3_top30 <- read_excel(meritetal_suppl_table_3_path, sheet = "Top30_autosomal_predictive_gene")
meritetal_suppl_table_3_top100 <- read_excel(meritetal_suppl_table_3_path, sheet = "Top100_autosomal_predictive_acr")


##################################
### PREPARE FILES FOR USE IN CODE
##################################
# TUK ET AL 2017 (which combined cotton et al and carrel/willard)
# first need to map the start positions from hg19 to hg38
cott_carr_will_df <- data.frame(gene = tuketal_suppl_table_1_combined$`Gene name`,
                                gene_mapped = rep("",length(tuketal_suppl_table_1_combined$`Gene name`)),
                                start = as.numeric(tuketal_suppl_table_1_combined$`Start position`),
                                start_mapped = rep("",length(tuketal_suppl_table_1_combined$`Start position`)),
                                end = as.numeric(tuketal_suppl_table_1_combined$`End position`),
                                end_mapped = rep("",length(tuketal_suppl_table_1_combined$`End position`)),
                                status = tuketal_suppl_table_1_combined$`Combined XCI status`,
                                color = ifelse(tuketal_suppl_table_1_combined$`Combined XCI status` == "escape", "purple", 
                                               ifelse(tuketal_suppl_table_1_combined$`Combined XCI status` == "variable", 
                                                      "turquoise3","lightsteelblue3")))
# Get the remapped versions 
for(i in 1:length(cott_carr_will_df$start)){
    pos_start <- cott_carr_will_df$start[i]
    mapped_pos_start <- ifelse(pos_start %in% hg19_to_hg38$source_start,
                               mapped_pos_start <- hg19_to_hg38[hg19_to_hg38$source_start == pos_start & !is.na(hg19_to_hg38$source_start), "mapped_start"],
                               mapped_pos_start <- NA)
    ifelse(mapped_pos_start %in% x_expr$start, 
           mapped_gene <- unique(x_expr[x_expr$start == mapped_pos_start, "GENE"]),
           mapped_gene <- NA)
    pos_stop <- cott_carr_will_df$end[i]
    mapped_pos_stop <- ifelse(pos_stop %in% hg19_to_hg38$source_stop,
                               mapped_pos_stop <- hg19_to_hg38[hg19_to_hg38$source_stop == pos_stop & !is.na(hg19_to_hg38$source_stop), "mapped_stop"],
                               mapped_pos_stop <- NA)
    cott_carr_will_df$start_mapped[i] <- mapped_pos_start
    cott_carr_will_df$end_mapped[i] <- mapped_pos_stop
    cott_carr_will_df$gene_mapped[i] <- mapped_gene
}
# Then create a data frame from the x_expr_mod data using only those genes that
# are shared across both data sets
matching_TF_vector <- x_expr_mod$GENE %in% cott_carr_will_df$gene[!is.na(cott_carr_will_df$gene)]
#matching_TF_vector <- x_expr_mod$GENE %in% cott_carr_will_df$gene_mapped[!is.na(cott_carr_will_df$gene_mapped)]
p_cott_carr_will <- x_expr_mod[matching_TF_vector,]
# add a color column
for(i in 1:nrow(p_cott_carr_will)){
    gene <- p_cott_carr_will$GENE[i]
    color_vector <- cott_carr_will_df$color[cott_carr_will_df$gene == gene]
    #color_vector <- cott_carr_will_df$color[cott_carr_will_df$gene_mapped == gene]
    color <- color_vector[!is.na(color_vector)]
    p_cott_carr_will$color[i] <- color
}
### optional: remove "inactive" states for plotting
state_TF_vector <- p_cott_carr_will$GENE %in% cott_carr_will_df[cott_carr_will_df$status != "inactive", "gene"]
p_cott_carr_will_noinactive <- p_cott_carr_will[state_TF_vector,]
rm(matching_TF_vector, gene, color_vector, color, state_TF_vector)

# MERIT ET AL 2020
merit_top30 <- unique(meritetal_suppl_table_3_top30$ENSEMBL_gene_id)
merit_top100 <- unique(meritetal_suppl_table_3_top100$ENSEMBL_gene_id)
