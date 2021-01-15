# Functions in this segment:
# getAssocObjGene <- creates an association table by querying genes
# getAssocObjDiseease <- creates an association table by first finding genes that
#     are mapped to the disease/trait of interest. Once that list of genes 
#     is obtained, creates an association table by querying those genes. 
#     It also takes care of subsetting the returned table to make sure
#     only the disease/trait of interest is included. 

getAssocObjGene <- function(db){
    # db options are "gwas" and "nels"
    obj <- renderDataTable({
        validate(need(rv$geneofinterest1,""))
        geneofinterest <- rv$geneofinterest1
        df <- data.frame()
        for(gene in geneofinterest){
            if(db == "gwas") {
                df <- rbind(df, create_gwas_association_df(gene))
            }
            if(db == "nels"){
                df <- rbind(df, create_nelson_association_df(gene))
            }
        }
        ### only perform this section if the association_df isn't empty ###
        ### this cleans up selection to remove columns that are empty ####
        if(nrow(df) != 0){
            df$Link <- paste0('<a href="https://', df$Hyperlink,'" target="_blank">', df$Hyperlink, '</a>')
            if(db == "nels"){
                df <- select(df, -"Hyperlink") # remove Hyperlink column if its from Nelson
            }
            to_remove <- "" # if all rows in a column are blank, then remove the column
            for(i in 1:ncol(df)){
                if(sum((df[,i]) == "") == nrow(df)){
                    ifelse(to_remove == "", to_remove <- i, to_remove <- c(to_remove, i))
                }
            }
            # make sure to_remove actually exists before removing it from df
            ifelse(to_remove != "", df <- select(df, -c(all_of(to_remove))),"")
        } 
        #### done ########################################################
        df}, # display df
        options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width='20px',targets=2))
        ),
        escape = FALSE
    )
}

getAssocObjDisease <- function(db){
    # db options are "gwas" and "nels"
    obj <- renderDataTable({
        validate(need(rv$diseaseofinterest1,""))
        diseaseofinterest <- rv$diseaseofinterest1
        # First get a list of mapped genes (per disease "d")
        mapped_genes <- c()
        for(d in diseaseofinterest){
            if(db == "gwas"){
                mg <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
            }
            if(db == "nels"){
                mg <- NELSON_ASSOCIATIONS_2[tolower(NELSON_ASSOCIATIONS_2$MSH) == d,'Gene']
            }
            if(!identical(mg, character(0))){
                ifelse(is.null(mapped_genes), mapped_genes <- mg, mapped_genes <- c(mapped_genes, mg))
            }
        }
        returned_genes_list <- c()
        returned_genes <- for(gene in c(unique(x_expr[,"GENE"]))){
            ifelse(TRUE %in% grepl(gene, mapped_genes), returned_genes_list <- c(returned_genes_list,gene),"")
        }
        # for event conditioning syntax, returned_genes would need to be "" if empty
        ifelse(returned_genes_list != c(), rv$returned_genes_list <- returned_genes_list, rv$returned_genes_list <- "")
        df <- data.frame()
        for(d in diseaseofinterest){
            for(gene in returned_genes_list){
                assign(("gene_stats"), create_single_gene_stats(gene, x_expr)) # may not need this
                if(db == "gwas"){
                    temp_df <- create_gwas_association_df(gene)
                }
                if(db == "nels"){
                    temp_df <- create_nelson_association_df(gene)
                }
                df <- rbind(df, temp_df[temp_df$`Disease/Trait` == d,])
                # ^subsets the table only for the disease of interest
            }
        }
        ### only perform this section if the assocation_df isn't empty ###
        ### this cleans up selection to remove columns that are empty ####
        if(nrow(df) != 0){
            df$Link <- paste0('<a href="https://', df$Hyperlink,'" target="_blank">', df$Hyperlink, '</a>')
            if(db == "nels"){
                df <- select(df, -"Hyperlink") # remove Hyperlink column if Nelson
            }
            to_remove <- "" # if all rows in a column are blank, then remove the column
            for(i in 1:ncol(df)){
                if(sum((df[,i]) == "") == nrow(df)){
                    ifelse(to_remove == "", to_remove <- i, to_remove <- c(to_remove, i))
                }
            }
            # make sure to_remove actually exists before removing it from df
            ifelse(to_remove != "", df <- select(df, -c(all_of(to_remove))),"")
        } 
        #### done ########################################################
        df}, # display df
        options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width='20px',targets=2)),
            processing = FALSE
        ),
        escape = FALSE
    )
}