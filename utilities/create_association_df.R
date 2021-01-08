create_association_df <- function(gene){
# create base association data frame
Link = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"LINK"]
association_df <- data.frame(Date = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"DATE.ADDED.TO.CATALOG"],
                     "Mapped.Gene" = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"MAPPED_GENE"],
                     "Disease.Trait" = tolower(GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"DISEASE.TRAIT"]),
                     Link = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"LINK"])
# collect sex bias information on disease/trait
list_of_traits <- c(association_df$`Disease.Trait`)
list_of_ukbionames <- c() # collect uk bio names
list_of_ratios <- c()   # collect ratio
list_of_bias <- c()     # collect bias
for(trait in list_of_traits) {
    assign(("stats"), create_single_trait_stats(trait)) # create diseasetrait object
    ukbioname <- "" # to build ukbioname vector
    ifelse(is.na(stats$trait_gwas2ukbio), ukbioname <- "N/A", ukbioname <- stats$trait_gwas2ukbio)
    list_of_ukbionames <- c(list_of_ukbionames, ukbioname)
    ratio <- "" # to build ratio vector
    ifelse(is.na(stats$ukbio_ratio), ratio <- "N/A", ratio <- stats$ukbio_ratio)
    list_of_ratios <- c(list_of_ratios, ratio)
    bias <- "" # to build bias vector
    ifelse(is.na(stats$ukbio_bias), bias <- "N/A", bias <- stats$ukbio_bias)
    list_of_bias <- c(list_of_bias, bias)
}
# add to the base association df
association_df <- cbind(association_df, 
                        data.frame("UK Bio Des" = list_of_ukbionames,
                                   "Sex Ratio (f/m)" = list_of_ratios),
                                    "Bias" = list_of_bias)
return(association_df)
}