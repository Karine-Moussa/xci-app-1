######## For GWAS study ##########
create_gwas_association_df <- function(gene){
# create base association data frame
    TRUE_FALSE_VECTOR_GWAS = grepl(paste0("\\b",gene,"\\b"), GWAS_ASSOCIATIONS$MAPPED_GENE)
    association_df <- data.frame("Date" = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"DATE.ADDED.TO.CATALOG"],
                         "Mapped Gene" = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"MAPPED_GENE"],
                         "Disease/Trait" = tolower(GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"DISEASE.TRAIT"]),
                         "Link" = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"LINK"],
                         "Hyperlink" = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"LINK"],
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
    TRUE_FALSE_VECTOR_NELSON = grepl(paste0("\\b",gene,"\\b"), NELSON_ASSOCIATIONS_2$Gene)
    association_df <- data.frame("Gene" = NELSON_ASSOCIATIONS_2[TRUE_FALSE_VECTOR_NELSON ,"Gene"],
                                 "Disease/Trait" = tolower(NELSON_ASSOCIATIONS_2[TRUE_FALSE_VECTOR_NELSON ,"MSH"]),
                                 "Link" = NELSON_ASSOCIATIONS_2[TRUE_FALSE_VECTOR_NELSON ,"Link"],
                                 "Source" = NELSON_ASSOCIATIONS_2[TRUE_FALSE_VECTOR_NELSON ,"Source"],
                                 "eQTL" = NELSON_ASSOCIATIONS_2[TRUE_FALSE_VECTOR_NELSON ,"eqtl"],
                                 check.names = FALSE)
    # Make sure for each row, the Gene can be found within our data
    # if not, then remove it. 
    if(nrow(association_df != 0)){ # first make sure assocation_df is populated
        to_remove <- ""
        for(i in 1:nrow(association_df)){
            if(sum(association_df$Gene[i] %in% LIST_OF_GENES) == 0){ 
                # if to_remove is blank, assign it to i. Otherwise append i to it. 
                ifelse(to_remove == "", to_remove <- i, to_remove <- c(to_remove, i))
            }
        }
        # make sure to_remove actually exists before de-subbing it from data frame
        ifelse(to_remove != "", association_df <- association_df[-c(to_remove),], "")
    }
    # ADD-ONS:
    # add hyperlinks
    list_of_hyperlinks = c()
    # first check if there are any entries in association_df
    if(nrow(association_df) != 0){
        for(i in 1:length(association_df$Source)){
            hyperlink <- ""
            if(!is.na(association_df$Source[i])){  # First need to make sure Source exists.
                if (association_df$Source[i] == "OMIM"){
                    hyperlink <- paste0("www.omim.org/entry/", gsub("OMIM:", "", association_df$Link[i]))
                }
                if (association_df$Source[i] == "GWAS:A" || association_df$Source[i] == "GWAS:A") {
                    hyperlink <- paste0("www.pubmed.ncbi.nlm.nih.gov/", gsub("PUBMEDID:", "", association_df$Link[i]))
                }
                list_of_hyperlinks <- c(list_of_hyperlinks, hyperlink)
            } 
        }
    }
    # add sex bias information on disease/trait
    list_of_traits <- c(association_df$`Disease/Trait`)
    list_of_ukbionames <- c() # collect uk bio names
    list_of_ratios <- c()   # collect ratio
    list_of_bias <- c()     # collect bias
    for(trait in list_of_traits) {
        assign(("stats"), create_single_trait_stats(trait)) # create diseasetrait object
        ukbioname <- "" # to build ukbioname vector
        ifelse(is.na(stats$trait_nels2ukbio), ukbioname <- "N/A", ukbioname <- stats$trait_nels2ukbio)
        list_of_ukbionames <- c(list_of_ukbionames, ukbioname)
        ratio <- "" # to build ratio vector
        ifelse(is.na(stats$ratio_nels2ukbio), ratio <- "N/A", ratio <- format(stats$ratio_nels2ukbio, digits = 4))
        list_of_ratios <- c(list_of_ratios, ratio)
        bias <- "" # to build bias vector
        ifelse(is.na(stats$bias_nels2ukbio), bias <- "N/A", bias <- stats$bias_nels2ukbio)
        list_of_bias <- c(list_of_bias, bias)
    }
    # add to the right of association df
    association_df <- cbind(data.frame(association_df, check.names = FALSE), 
                            data.frame("Hyperlink" = list_of_hyperlinks, 
                                       "UK Bio Desc." = list_of_ukbionames,
                                       "Ratio (f/m)" = list_of_ratios,
                                       "Bias" = list_of_bias,
                                       check.names = FALSE))
    # lastly, remove any duplicate rows
    association_df <- unique(association_df)
    return(association_df)
}

create_manual_association_df <- function(gene){
    # first create gene stats then collect nelson df
    TRUE_FALSE_VECTOR_MANUAL = grepl(paste0("\\b",gene,"\\b"), MANUAL_STUDIES$MAPPED_GENE)
    association_df <- data.frame("Gene" = MANUAL_STUDIES[TRUE_FALSE_VECTOR_MANUAL,"MAPPED_GENE"],
                                 "Disease/Trait" = tolower(MANUAL_STUDIES[TRUE_FALSE_VECTOR_MANUAL,"DISEASE_TRAIT"]),
                                 "Link" = MANUAL_STUDIES[TRUE_FALSE_VECTOR_MANUAL,"LINK"],
                                 "Source" = MANUAL_STUDIES[TRUE_FALSE_VECTOR_MANUAL,"SOURCE"],
                                 "eQTL" = "",
                                 check.names = FALSE)
    # Make sure for each row, the Gene can be found within our data
    # if not, then remove it. 
    if(nrow(association_df != 0)){ # first make sure assocation_df is populated
        to_remove <- ""
        for(i in 1:nrow(association_df)){
            if(sum(association_df$Gene[i] %in% LIST_OF_GENES) == 0){ 
                # if to_remove is blank, assign it to i. Otherwise append i to it. 
                ifelse(to_remove == "", to_remove <- i, to_remove <- c(to_remove, i))
            }
        }
        # make sure "to_remove" actually exists before de-subbing it from data frame
        ifelse(to_remove != "", association_df <- association_df[-c(to_remove),], "")
    }
    # ADD-ONS:
    # add hyperlinks
    list_of_hyperlinks = association_df$Link
    # add sex bias information on disease/trait
    list_of_traits <- c(association_df$`Disease/Trait`)
    list_of_ukbionames <- c() # collect uk bio names
    list_of_ratios <- c()   # collect ratio
    list_of_bias <- c()     # collect bias
    for(trait in list_of_traits) {
        assign(("stats"), create_single_trait_stats(trait)) # create diseasetrait object
        ukbioname <- "" # to build ukbioname vector
        ifelse(is.na(stats$trait_ukbio), ukbioname <- "N/A", ukbioname <- stats$trait_ukbio)
        list_of_ukbionames <- c(list_of_ukbionames, ukbioname)
        ratio <- "" # to build ratio vector
        ifelse(is.na(stats$ratio), ratio <- "N/A", ratio <- format(stats$ratio, digits = 4))
        list_of_ratios <- c(list_of_ratios, ratio)
        bias <- "" # to build bias vector
        ifelse(is.na(stats$bias), bias <- "N/A", bias <- stats$bias)
        list_of_bias <- c(list_of_bias, bias)
    }
    # add to the right of association df
    association_df <- cbind(data.frame(association_df, check.names = FALSE), 
                            data.frame("Hyperlink" = list_of_hyperlinks, 
                                       "UK Bio Desc." = list_of_ukbionames,
                                       "Ratio (f/m)" = list_of_ratios,
                                       "Bias" = list_of_bias,
                                       check.names = FALSE))
    # lastly, remove any duplicate rows
    association_df <- unique(association_df)
    return(association_df)
}
    