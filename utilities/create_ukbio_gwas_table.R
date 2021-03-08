# This script creates xchrom_ukbio_gwas_terms.rds
# | GWAS_TRAIT | EFO | MAPPED_UKBIO_TRAIT | MAPPING_TYPE | ICD10_CODE/SELF_REPORTED_TRAIT 
#
# The GWAS traits are all of the traits from GWAS association studies
# which map to X-chromosome
#################################################################################
# Packages/Libraries
library(tidyverse)
library(data.table)

# Import necessary tables
GWAS_ASSOCIATIONS <- read.csv("data_intermediate/gwas_assoc_v1_02_xonly.csv", header=T,na.strings="?")
GWAS_ASSOCIATIONS <- data.table(GWAS_ASSOCIATIONS)
UK_BIOBANK_MASTER <- read_tsv("data_sources/UK_Biobank_master_file.tsv", col_names = T)
UK_BIOBANK_MASTER <- data.table(UK_BIOBANK_MASTER)
UK_BIOBANK_MASTER <- subset(UK_BIOBANK_MASTER, MAPPING_TYPE == "Broad" | MAPPING_TYPE == "Exact" | MAPPING_TYPE == "Narrow")
# ^ subset only the clean returns

# Get list of GWAS traits
GWAS_TRAITS <- unique(GWAS_ASSOCIATIONS$DISEASE.TRAIT)

# Create a blank dt to build upon
dt <- data.table()

# For each GWAS trait, get the EFO
for(trait in GWAS_TRAITS){
    efo_link <- GWAS_ASSOCIATIONS[DISEASE.TRAIT == trait, MAPPED_TRAIT_URI][1]
    efo <- str_extract(efo_link, "[A-Z].*")
    # Search for the EFO in UK_Biobank_master_file. Return
    # a data frame for "ukbioname", "mapping_type", and "icd10_code/s_r_t"
    # and assign it to temp_dt
    temp_dt2 <- UK_BIOBANK_MASTER[MAPPED_TERM_URI == efo, 
                                 c("ZOOMA QUERY", "MAPPING_TYPE", "ICD10_CODE/SELF_REPORTED_TRAIT_FIELD_CODE")]
    N <- nrow(temp_dt2)
    temp_dt1 <- data.table("GWAS_TRAIT" = rep(trait, N),
                           "EFO" = rep(efo,N))
    temp_dt <- cbind(temp_dt1,temp_dt2)

    # append temp_dt to dt
    dt <- rbind(dt, temp_dt)
}
# note, will need to handle exceptions such as multiple EFO's per entry

saveRDS(dt, file = "data_intermediate/gwas_ukbio_mapping_xchrom.rds")

# cleanup
rm(dt, temp_dt, temp_dt1, temp_dt2)
