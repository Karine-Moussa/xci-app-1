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

# Update the sames of all escape calls
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
                                          gene_object$pos_mapped,
                                          names(gene_object$tiss),
                                          full_tiss_names,
                                          c(gene_object$tiss),
                                          gene_object$state))
}
colnames(cotton_mDNA_pivot) <- c("GENE", "POS_HG19", "POS", "TISS", "FULL_TISS", "TISS_STATE", "STATUS")
saveRDS(cotton_mDNA_pivot, "resources_studies/Cotton2014/cotton_mDNA.rds")
