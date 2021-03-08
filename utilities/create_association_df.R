######## For GWAS study ##########
create_gwas_association_df <- function(gene){
    # 1. get a list of traits
    TRUE_FALSE_VECTOR_GWAS = grepl(paste0("\\b",gene,"\\b"), GWAS_ASSOCIATIONS$MAPPED_GENE)
    mapped_genes = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"MAPPED_GENE"]
    mapped_traits = tolower(GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"DISEASE.TRAIT"])
    links = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"LINK"]
    dates = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"DATE.ADDED.TO.CATALOG"]
    # 2. for each trait get ukbio info
    # This may return multiple rows of information if there is more than 
    # one synomous ukbio term.
    df_final <- data.frame()
    for (i in 1:length(mapped_genes)){
        assign("trait_stats", create_single_trait_stats(mapped_traits[i]))
        n <- nrow(trait_stats$ukbio_stats)
        # df0 will store columns with GWAS information.
        # the information will be entered n times, where n is the 
        # number of similar ukbio names. 
        df0 <- data.frame("date" = rep(dates[i], n),
                          "gene" = rep(mapped_genes[i], n),
                          "gwas_trait" = rep(mapped_traits[i], n),
                          "link" = rep(links[i], n),
                          "hyperlink" = rep(links[i], n))
        # df1 stores the ukbio_stats information
        df1 <- trait_stats$ukbio_stats[,c("trait", "ratio", "bias")]
        df_final <- rbind(df_final, cbind(df0, df1))
    }
    # 3. Clean up (remove duplicate rows, rename columns)
    df_final <- unique(df_final)
    col_ <- c("Date", "Mapped Gene", "Disease/Trait", "Link", "Hyperlink",
              "UK Bio Desc.", "Ratio (f/m)", "Bias")
    colnames(df_final) <- col_
    df_final <- data.frame(df_final, 
                           check.names = FALSE)
    return(df_final)
}
