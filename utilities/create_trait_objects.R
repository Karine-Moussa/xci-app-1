#### Create trait class: attributes for each trait ######
create_single_trait_stats <- function(trait)
    ### User passes in a trait
    ### Function returns the object "<trait>_stats" with attributes of trait
    ### Usage:  assign("traitofinterest_stats", create_single_trait_stats(trait))
    ## Note: the assumption is that the trait is the GWAS term
{
    trait <- tolower(trait)
    trait_no_parenthesis <- gsub('\\(|\\)',"",trait) # This is necessary to handle parenthesis in traits#
    trait_gwas2ukbio = unique(tolower(unique(GWAS_UKBIO_MAPPING[tolower(GWAS_TRAIT) == trait, 
                                                                c(`ZOOMA QUERY`)])))
    ifelse(identical(trait_gwas2ukbio, "character(0)"), trait_gwas2ukbio <- "N/A","")
    trait_ukbio <- trait_gwas2ukbio
    if(length(trait_ukbio) == 0){
      trait_ukbio = ""
    }
    # Manual exceptions
    if (trait == "systemic lupus erythematosus"){
        trait_ukbio <- "systemic lupus erythematosis/sle"
    }
    if (trait == "venous thromboembolism"){
      trait_ukbio <- c(trait_ukbio, "deep venous thrombosis (dvt)")
    }
    # ukbio_stats
    ukbio_stats = c()
    for(t in trait_ukbio){
        stat <- get_ukbiobank_information(t)
        ukbio_stats <- rbind(ukbio_stats, stat)
    }
    #### Assign attributes to trait
    assign(paste0(trait, "_stats"),
           # Add any attributes of interest to this list
           (list(
               # Trait name (single)
               trait = trait,
               trait_ukbio = ukbio_stats$trait,
               ukbio_stats = ukbio_stats,
               efo = ukbio_stats$efo,
               icd = ukbio_stats$icd,
               # UKBIO stats 
               case_female = ukbio_stats$case_female,
               case_male = ukbio_stats$case_male,
               ratio = ukbio_stats$ratio,
               bias = ukbio_stats$bias,
               # GWAS to UKBIO stats
               trait_gwas2ukbio = trait_gwas2ukbio,
               case_female_gwas2ukbio = ukbio_stats$case_female,
               case_male_gwas2ukbio = ukbio_stats$case_male,
               ratio_gwas2ukbio = ukbio_stats$ratio,
               bias_gwas2ukbio = ukbio_stats$bias
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

# Create to return ukbiobank trait descriptions
get_ukbiobank_information <- function(trait){
  trait = tolower(trait)
  efo = unique(GWAS_UKBIO_MAPPING[tolower(`ZOOMA QUERY`) == trait, `EFO`])
  icd = unique(GWAS_UKBIO_MAPPING[tolower(`ZOOMA QUERY`) == trait, `ICD10_CODE/SELF_REPORTED_TRAIT_FIELD_CODE`])
  case_female = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait & PHENO_RATES_UKBIO$gender == 'female', 'case']
  case_male = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait & PHENO_RATES_UKBIO$gender == 'male', 'case']
  ratio = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait & PHENO_RATES_UKBIO$gender == 'female', 'ratio']
  bias = PHENO_RATES_UKBIO[PHENO_RATES_UKBIO$des == trait & PHENO_RATES_UKBIO$gender == 'female', 'bias']
  if(length(efo) == 0){efo = ""}
  if(length(icd) == 0){icd = ""}
  if(length(case_female) == 0){case_female = ""}
  if(length(case_male) == 0){case_male = ""}
  if(length(ratio) == 0){ratio = ""}
  if(length(bias) == 0){bias = ""}
  dt <- data.frame(
    "trait" = trait,
    "efo" = efo,
    "icd" = icd,
    "case_female" = case_female,
    "case_male" = case_male,
    "ratio" = ratio,
    "bias" = bias
  )
  #### This next loop makes sure each item isn't N/A
  #### if it does, replace it with "", otherwise the shinyapp data table will 
  #### throw warnings.
  for(i in 1:ncol(dt)){
    if(is.na(dt[,i])){dt[,i] = ""}
  }
  return(dt)
}
