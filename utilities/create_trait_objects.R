#### Create trait class: attributes for each trait ######
create_single_trait_stats <- function(trait)
    ### User passes in a trait
    ### Function returns the object "<trait>_stats" with attributes of trait
    ### Usage:  assign("traitofinterest_stats", create_single_trait_stats(trait))
    ## Note: the assumption is that the trait is the GWAS term
{
    trait <- tolower(trait)
    trait_no_parenthesis <- gsub('\\(|\\)',"",trait) # This is necessary to handle parenthesis in traits
    GWAS_UKBIO_MAPPING <- readRDS("data_intermediate/gwas_ukbio_mapping_xchrom.rds")
    trait_gwas2ukbio_old = LIST_OF_TRAITS_GWAS[grepl(tolower(paste0("^",trait_no_parenthesis,"$")), 
                                                 gsub('\\(|\\)',"", tolower(LIST_OF_TRAITS_GWAS$GWAS_NAME))) & 
                                               !is.na(LIST_OF_TRAITS_GWAS$UKBIO_NAME), 'UKBIO_NAME']
    trait_gwas2ukbio = unique(tolower(unique(GWAS_UKBIO_MAPPING[tolower(GWAS_TRAIT) == trait, 
                                                                c("ZOOMA QUERY")])))
    efo = unique(GWAS_UKBIO_MAPPING[tolower(GWAS_TRAIT) == trait, c("EFO")])
    icd = unique(GWAS_UKBIO_MAPPING[tolower(GWAS_TRAIT) == trait, c("ICD10_CODE/SELF_REPORTED_TRAIT_FIELD_CODE")])
    trait_nels2ukbio = LIST_OF_TRAITS_NELSON_2[grepl(tolower(paste0("^",trait_no_parenthesis,"$")), 
                                                   gsub('\\(|\\)',"", tolower(LIST_OF_TRAITS_NELSON_2$NELS2_NAME))) & 
                                                 !is.na(LIST_OF_TRAITS_NELSON_2$UKBIO_NAME), 'UKBIO_NAME']
    trait_ukbio <- ""
    ifelse(trait_gwas2ukbio != "", trait_ukbio <- trait_gwas2ukbio,"")
    ifelse(trait_nels2ukbio != "", trait_ukbio <- trait_nels2ukbio,"")
    # Assign attributes to trait
    assign(paste0(trait, "_stats"),
           # Add any attributes of interest to this list
           (list(
               # Trait name (single)
               trait = trait,
               trait_ukbio = trait_ukbio_old,
               efo = efo,
               icd = icd,
               # UKBIO stats 
               case_female = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_ukbio & PHENO_RATES_UKBIO$gender == 'female', 'case'],
               case_male = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_ukbio & PHENO_RATES_UKBIO$gender == 'male', 'case'],
               ratio = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_ukbio & PHENO_RATES_UKBIO$gender == 'female', 'ratio'],
               bias = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_ukbio & PHENO_RATES_UKBIO$gender == 'female', 'bias'],
               # GWAS to UKBIO stats
               trait_gwas2ukbio = trait_gwas2ukbio,
               case_female_gwas2ukbio = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_gwas2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'case'],
               case_male_gwas2ukbio = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_gwas2ukbio & PHENO_RATES_UKBIO$gender == 'male', 'case'],
               ratio_gwas2ukbio = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_gwas2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'ratio'],
               bias_gwas2ukbio = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_gwas2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'bias'],
               # NELSON2 to UKBIO stats
               trait_nels2ukbio = trait_nels2ukbio,
               case_female_nels2ukbio = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_nels2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'case'],
               case_male_nels2ukbio  = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_nels2ukbio & PHENO_RATES_UKBIO$gender == 'male', 'case'],
               ratio_nels2ukbio  = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_nels2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'ratio'],
               bias_nels2ukbio  = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_nels2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'bias']
                )
           )
    )
}

# Create gene class object for multiple traits at once
create_multiple_trait_stats <- function(trait_list){
    ### User passes a list of genes 
    ### Function returns a list of "<gene>_stats" for each passed argument
    ### Usage 
    for(trait in trait_list) {
        assign((paste0(trait, "_stats")), create_single_trait_stats(trait),
               env = globalenv())
    }
}