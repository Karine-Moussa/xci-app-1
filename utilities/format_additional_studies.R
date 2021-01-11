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
                                start = as.numeric(tuketal_suppl_table_1_combined$`Start position`),
                                end = as.numeric(tuketal_suppl_table_1_combined$`End position`),
                                status = tuketal_suppl_table_1_combined$`Combined XCI status`,
                                color = ifelse(tuketal_suppl_table_1_combined$`Combined XCI status` == "escape", "purple", 
                                               ifelse(tuketal_suppl_table_1_combined$`Combined XCI status` == "variable", 
                                                      "turquoise3","white")))
#mapped_start <- c()
#for(pos in cott_carr_will_df$start){
#    mapped_pos <- ifelse(pos %in% hg19_to_hg38$source_start,
#                         mapped_pos <- hg19_to_hg38[hg19_to_hg38$source_start == testpos & !is.na(hg19_to_hg38$source_start), "mapped_start"],
#                         mapped_pos <- NA)
#    mapped_start <- c(cbind(mapped_start, mapped_pos))
#}
#cott_carr_will_df <- cbind(cott_carr_will_df,
         #                  data.frame("mapped_start" = mapped_start))

# MERIT ET AL 2020
merit_top30 <- unique(meritetal_suppl_table_3_top30$ENSEMBL_gene_id)
merit_top100 <- unique(meritetal_suppl_table_3_top100$ENSEMBL_gene_id)
