#### Create trait class: attributes for each trait ######
create_single_trait_stats <- function(trait)
    ### User passes in a trait
    ### Function returns the object "<trait>_stats" with attributes of trait
    ### Usage:  assign("traitofinterest_stats", create_single_trait_stats(trait))
{
    trait <- tolower(trait)
    trait_no_parenthesis <- gsub('\\(|\\)',"",trait)
    trait_gwas2ukbio = LIST_OF_TRAITS_GWAS[grepl(tolower(paste0("^",trait_no_parenthesis,"$")), 
                                                 gsub('\\(|\\)',"", tolower(LIST_OF_TRAITS_GWAS$GWAS_NAME))) & 
                                               !is.na(LIST_OF_TRAITS_GWAS$UKBIO_NAME), 'UKBIO_NAME']
    # Assign attributes to trait
    assign(paste0(trait, "_stats"),
           # Add any attributes of interest to this list
           (list(
               # Trait name (single)
               trait = trait,
               # Where it can be found (single)
            #   in_GWAS = ifelse(trait %in% tolower(LIST_OF_TRAITS_GWAS$GWAS_NAME), "yes","no"),
            #   in_UKBIO = ifelse(trait %in% tolower(LIST_OF_TRAITS_UKBIO), "yes","no"),
            #   in_NELSON = ifelse(trait %in% tolower(LIST_OF_TRAITS_NELSON), "yes","no"),
               # GWAS to UKBIO name conversion
               # UK Biobank rates
               trait_gwas2ukbio = trait_gwas2ukbio,
               ukbio_case_female = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_gwas2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'case'],
               ukbio_case_male = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_gwas2ukbio & PHENO_RATES_UKBIO$gender == 'male', 'case'],
               ukbio_ratio = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_gwas2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'ratio'],
               ukbio_bias = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait_gwas2ukbio & PHENO_RATES_UKBIO$gender == 'female', 'bias']
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