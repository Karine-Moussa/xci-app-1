######## For GWAS study ##########
create_gwas_association_df <- function(gene){
    # 1. get a list of traits
    TRUE_FALSE_VECTOR_GWAS = grepl(paste0("\\b",gene,"\\b"), GWAS_ASSOCIATIONS$MAPPED_GENE)
    mapped_genes = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"MAPPED_GENE"]
    mapped_traits = tolower(GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"DISEASE.TRAIT"])
    links = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"LINK"]
    dates = GWAS_ASSOCIATIONS[TRUE_FALSE_VECTOR_GWAS,"DATE.ADDED.TO.CATALOG"]
    # 1.5 If the gene could not be found, set the returns to ""
    # otherwise an error will be thrown because they are = character(0)
    if(length(mapped_genes) == 0){mapped_genes = ""}
    if(length(mapped_traits) == 0){mapped_traits = ""}
    if(length(links) == 0){links = ""}
    if(length(dates) == 0){dates = ""}
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
    # 3. Clean up
    # Remove duplicate rows.
    df_final <- unique(df_final)
    # for testing
    #View(df_final)
    # Remove blank rows (if the df_final exists)
    if(length(nrow(df_final)) != 0){
        to_remove = c()
        for(i in 1:nrow(df_final)){
            # for testing
            #print(paste("sum(df_final[i,] == '')", sum(df_final[i,] == "")))
            #print(paste("ncol(df_final)", ncol(df_final)))
            # First check if there's an NA in the row. If so, keep it.
            if(is.na(sum(df_final[i,] == ""))){
                to_remove = to_remove
            }
            # THEN, check if each entry of a row is blank
            else if (sum(df_final[i,] == "") == ncol(df_final)){
                to_remove = c(to_remove, i)
            }
        }
        # Make sure to_remove vector has values before using it
        if(!is.null(to_remove)){df_final <- df_final[-to_remove, , drop = FALSE]}
    }
    # Add column names.
    col_ <- c("Date", "Mapped Gene", "Disease/Trait", "Link", "Hyperlink",
              "UK Bio Desc.", "Ratio (f/m)", "Bias")
    colnames(df_final) <- col_
    df_final <- data.frame(df_final, 
                           check.names = FALSE)
    return(df_final)
}
