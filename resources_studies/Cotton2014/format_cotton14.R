# This script takes in Cotton 2014 supplementary 
# table 5 and formats it for use in the application.
# Output is an rds file: cotton_mDNA.rds

library(readxl)
setwd("/Users/karinemoussa/Documents/DrLiu_Lab/Shiny_Apps/xci-app-1")
file_path <- "resources_studies/Cotton2014/ddu564supp_table5_mod.xlsx"

cotton_mDNA <- read_xlsx(file_path)

colnames(cotton_mDNA) <- cotton_mDNA[1,]
cotton_mDNA <- cotton_mDNA[-1,]
# columns with tissue are 3:29

options_1 <- as.character(unique(unlist(cotton_mDNA[,3:29])))
options_1 <- rbind(options_1,
                   map = c("escape", "escape", "variable", "inactive", "inactive", "uncalled",
                           "variable"))

options_2 <- as.character(unique(unlist(cotton_mDNA[,30])))
options_2 <- rbind(options_2,
                   map = c("escape", "variable", "variable", "inactive", "variable"))

# How to return the new name:
options_1[2,][options_1[1,] %in% "U"] # should return "uncalled"
options_2[2,][options_2[1,] %in% cotton_mDNA[14, 30]]

# Update the names of all escape calls
for (row_num in 1:nrow(cotton_mDNA)){
    for (col_num in 3:29){
        new_name <- options_1[2,][options_1[1,] %in% cotton_mDNA[row_num, col_num]]
        cotton_mDNA[row_num, col_num] <- new_name
    }
    cotton_mDNA[row_num, 30] <- options_2[2,][options_2[1,] %in% cotton_mDNA[row_num, 30]]
}


# Get HG38 remapping information (used in creating pivot table)
remapping_file <- read_xlsx("resources_studies/Cotton2014/report_cotton_mDNA_chrX_pos.xlsx")
remapping_file[,"mapped_start"][remapping_file[,"source_start"] == 2746862] # example

# Pivot entire table
full_tiss_names <- c("peripheral_blood_leukocytes",	"buccal",	"buffy_coat",	
                     "blood",	"saliva",	"kidney",	"muscle",	
                     "subcutaneous_fat",	"omentum",	"stomach",	
                     "hair",	"heart",	"pancreas",	"adrenal",	
                     "spleen",	"lung",	"liver",	"brain_prefrontal_cortex",	
                     "brain",	"brain10",	"brain20",	"brain7",	
                     "whole_blood",	"PBMC",	"cord_blood",	"blood_spot",	
                     "kidney_tubule")
cotton_mDNA_pivot <- data.frame()
for (row_num in 1:nrow(cotton_mDNA)){
    # Create gene object
    gene_object <- list(gene = unlist(cotton_mDNA[row_num, "GENE"]), 
                        pos = unlist(cotton_mDNA[row_num, "POS"]),
                        pos_mapped = remapping_file[,"mapped_start"][remapping_file[,"source_start"] == unlist(cotton_mDNA[row_num, "POS"])],
                        tiss = unlist(unlist(cotton_mDNA[row_num, 3:29])),
                        state = unlist(cotton_mDNA[row_num, "STATUS"]))
    # add to pivot table
    cotton_mDNA_pivot <- rbind(cotton_mDNA_pivot, 
                               data.frame(gene_object$gene, 
                                          gene_object$pos,
                                          gene_object$pos_mapped[1],
                                          names(gene_object$tiss),
                                          full_tiss_names,
                                          c(gene_object$tiss),
                                          gene_object$state))
}
colnames(cotton_mDNA_pivot) <- c("GENE", "POS_HG19", "POS", "TISS", "FULL_TISS", "TISS_STATE", "STATUS")

## Get START and STOP of genes
## Comment out biomaRt to elminate it during compiling
# library(biomaRt)
# ensembl <- readRDS("rds/ensembl.rds")
# filters = listFilters(ensembl)
# attributes = listAttributes(ensembl)
# 
# pos_ids_3 = getBM(attributes = c('external_gene_name', 'start_position', 'end_position'), 
#                   filters =  c('external_gene_name', 'chromosome_name'),
#                   values = list(genes = c(cotton_mDNA_pivot$GENE), chromosome = "X"), 
#                   mart = ensembl)
# colnames(pos_ids_3) <- c("GENE", "START", "STOP")
# saveRDS(pos_ids_3, "rds/pos_ids_3.rds")
pos_ids_3 <- readRDS("rds/pos_ids_3.rds")
cotton_mDNA_pivot_merge_a <- merge(cotton_mDNA_pivot, pos_ids_3, all.x = T)
# 47 TSSs need to have locations manually added
write.csv(cotton_mDNA_pivot_merge_a, "sandbox/cotton_mDNA_merge_a.csv", row.names = FALSE)
# ran gene_locator.py on TSSs missing start and stop positions.
# output is in: returned resources_studies/Cotton14/gene_locations_cotton14.xlsx

# First: handle non-duplicates 
gene_loc_no_dup <- read_xlsx("resources_studies/Cotton2014/gene_locations_cotton14_no_dup.xlsx")
for ( i_row in 1:nrow(cotton_mDNA_pivot_merge_a) ) {
    if ( is.na(cotton_mDNA_pivot_merge_a$START[i_row]) ){
        for (j_row in 1:nrow(gene_loc_no_dup)){
            if (cotton_mDNA_pivot_merge_a[i_row,]$POS == gene_loc_no_dup[j_row,]$POS){
                cotton_mDNA_pivot_merge_a[i_row,]$START <- gene_loc_no_dup[j_row,]$START
                cotton_mDNA_pivot_merge_a[i_row,]$STOP <- gene_loc_no_dup[j_row,]$STOP
                cotton_mDNA_pivot_merge_a[i_row,]$GENE <- gene_loc_no_dup[j_row,]$SYMBOL
            }   
        }
    }
}
# Second: handle genes with duplicates
gene_loc_dup <- read_xlsx("resources_studies/Cotton2014/gene_locations_cotton14_dup.xlsx")
for ( i_row in 1:nrow(cotton_mDNA_pivot_merge_a) ) {
    if ( is.na(cotton_mDNA_pivot_merge_a$START[i_row]) ){ # if no START or STOP
        for (j_row in seq(1, nrow(gene_loc_dup),2)){ # every other row in j
            if (cotton_mDNA_pivot_merge_a[i_row,]$POS == gene_loc_dup[j_row,]$POS){
                # Add the START and STOP and FIRST GENE NAME for the original row
                cotton_mDNA_pivot_merge_a[i_row,]$START <- gene_loc_dup[j_row,]$START
                cotton_mDNA_pivot_merge_a[i_row,]$STOP <- gene_loc_dup[j_row,]$STOP
                cotton_mDNA_pivot_merge_a[i_row,]$GENE <- gene_loc_dup[j_row,]$SYMBOL
                # Create a row with the START and STOP and SECOND GENE NAME 
                # Then add to the bottom of full df
                temp_row <- cotton_mDNA_pivot_merge_a[i_row,]
                temp_row$START <- gene_loc_dup[j_row+1,]$START
                temp_row$STOP <- gene_loc_dup[j_row+1,]$STOP
                temp_row$GENE <- gene_loc_dup[j_row+1,]$SYMBOL
                cotton_mDNA_pivot_merge_a <- rbind(cotton_mDNA_pivot_merge_a, temp_row)
                rm(temp_row)
            }   
        }
    }
}
rm(i_row, j_row) 

# Save information
saveRDS(cotton_mDNA_pivot_merge_a, "resources_studies/Cotton2014/cotton_mDNA_precolor.rds")

# Clean up 
rm(cotton_mDNA, cotton_mDNA_pivot, gene_object,
   options_1, options_2, remapping_file, col_num,
   file_path, full_tiss_names, new_name, row_num)
rm(pos_ids_3)
