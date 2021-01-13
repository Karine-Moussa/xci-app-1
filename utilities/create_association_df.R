######## For GWAS study ##########
create_gwas_association_df <- function(gene){
# create base association data frame
association_df <- data.frame("Date" = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"DATE.ADDED.TO.CATALOG"],
                     "Mapped Gene" = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"MAPPED_GENE"],
                     "Disease/Trait" = tolower(GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"DISEASE.TRAIT"]),
                     "Link" = GWAS_ASSOCIATIONS[grepl(gene, GWAS_ASSOCIATIONS$MAPPED_GENE),"LINK"],
                     check.names = FALSE)
# collect sex bias information on disease/trait
list_of_traits <- c(association_df$`Disease/Trait`)
list_of_ukbionames <- c() # collect uk bio names
list_of_ratios <- c()   # collect ratio
list_of_bias <- c()     # collect bias
for(trait in list_of_traits) {
    assign(("stats"), create_single_trait_stats(trait)) # create diseasetrait object
    ukbioname <- "" # to build ukbioname vector
    ifelse(is.na(stats$trait_gwas2ukbio), ukbioname <- "N/A", ukbioname <- stats$trait_gwas2ukbio)
    list_of_ukbionames <- c(list_of_ukbionames, ukbioname)
    ratio <- "" # to build ratio vector
    ifelse(is.na(stats$ratio_gwas2ukbio), ratio <- "N/A", ratio <- format(stats$ratio_gwas2ukbio, digits = 4))
    list_of_ratios <- c(list_of_ratios, ratio)
    bias <- "" # to build bias vector
    ifelse(is.na(stats$bias_gwas2ukbio), bias <- "N/A", bias <- stats$bias_gwas2ukbio)
    list_of_bias <- c(list_of_bias, bias)
}
# add to the right of association df
association_df <- cbind(data.frame(association_df, check.names = FALSE), 
                        data.frame("UK Bio Desc." = list_of_ukbionames,
                                   "Ratio (f/m)" = list_of_ratios,
                                    "Bias" = list_of_bias,
                                    check.names = FALSE))
return(association_df)
}

######## For Nelson study ##########
create_nelson_association_df <- function(gene){
    # first create gene stats then collect nelson df
    assign(("gene_stats"), create_single_gene_stats(gene, x_expr))
    association_df <- gene_stats$nelson_df
    # ADD-ONS:
    # add hyperlinks
    list_of_hyperlinks = c() 
    for(i in 1:length(association_df$Source)){
        hyperlink <- ""
        if (association_df$Source[i] == "OMIM"){
            hyperlink <- paste0("https://www.omim.org/entry/", gsub("OMIM:", "", association_df$Link[i]))
        }
        if (association_df$Source[i] == "GWAS:A" || association_df$Source[i] == "GWAS:A") {
            hyperlink <- paste0("https://pubmed.ncbi.nlm.nih.gov/", gsub("PUBMEDID:", "", association_df$Link[i]))
        }
        list_of_hyperlinks <- c(list_of_hyperlinks, hyperlink)
    }
    # add sex bias information on disease/trait
    list_of_traits <- c(association_df$`Disease/Trait`)
    list_of_ukbionames <- c() # collect uk bio names
    list_of_ratios <- c()   # collect ratio
    list_of_bias <- c()     # collect bias
    for(trait in list_of_traits) {
        assign(("stats"), create_single_trait_stats(trait)) # create diseasetrait object
        ukbioname <- "" # to build ukbioname vector
        ifelse(is.na(stats$trait_nelson2ukbio), ukbioname <- "N/A", ukbioname <- stats$trait_nelson2ukbio)
        list_of_ukbionames <- c(list_of_ukbionames, ukbioname)
        ratio <- "" # to build ratio vector
        ifelse(is.na(stats$ratio_nelson2ukbio), ratio <- "N/A", ratio <- format(stats$ratio_nelson2ukbio, digits = 4))
        list_of_ratios <- c(list_of_ratios, ratio)
        bias <- "" # to build bias vector
        ifelse(is.na(stats$bias_nelson2ukbio), bias <- "N/A", bias <- stats$bias_nelson2ukbio)
        list_of_bias <- c(list_of_bias, bias)
    }
    # add to the right of association df
    association_df <- cbind(data.frame(association_df, check.names = FALSE), 
                            data.frame("Hyperlink" = list_of_hyperlinks, 
                                       "UK Bio Desc." = list_of_ukbionames,
                                       "Ratio (f/m)" = list_of_ratios,
                                       "Bias" = list_of_bias,
                                       check.names = FALSE))
    return(association_df)
}