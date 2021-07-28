# Functions in this segment:
# getAssocObjGene <- creates an association table by querying genes
# getAssocObjDiseease <- creates an association table by first finding genes that
#     are mapped to the disease/trait of interest. Once that list of genes 
#     is obtained, creates an association table by querying those genes. 
#     It also takes care of subsetting the returned table to make sure
#     only the disease/trait of interest is included. 
getAssocObjGene <- function(){
    obj <- renderDataTable({
        validate(need(rv$geneofinterest1,""))
        geneofinterest <- rv$geneofinterest1
        df <- data.frame()
        db <- "gwas"
        for(gene in geneofinterest){
            df <- rbind(df, create_gwas_association_df(gene))
        }
        # CLEAN UP
        ### only perform this section if the association_df isn't empty ###
        ### this cleans up selection to remove columns that are empty ####
        if(nrow(df) != 0){
            df$Link <- paste0('<a href="https://', df$Hyperlink,'" target="_blank">', df$Hyperlink, '</a>')
            df <- df[, -5] # remove Hyperlink column
            to_remove <- c()  # if all entries of a column are blank, then remove the column
            for(i in 1:ncol(df)){
                # First check if there's an NA in the row. If so, keep row.
                NAflag = is.na(sum((df[,i]) == "") == nrow(df))
                if(NAflag == TRUE) {
                    to_remove = to_remove
                }
                # OTHERWISE, check if each entry of a row is blank
                else if (sum((df[,i]) == "") == ncol(df)){
                    to_remove <- c(to_remove,i)
                }
            }
            # make sure to_remove actually exists before removing it from df
            #if(to_remove != ""){df <- df[, -c(all_of(to_remove))]}
        }
        #### done ########################################################
        df <- unique(df)    # remove duplicate rows
        df}, # display df
        options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width='20px',targets=2))
        ),
        escape = FALSE
    )
}

getAssocObjDisease <- function(){
    obj <- renderDataTable({
        validate(need(rv$diseaseofinterest1,""))
        diseaseofinterest <- rv$diseaseofinterest1
        study <- rv$addStudies
        # First get a list of mapped genes (per disease "d")
        mapped_genes <- c()
        db <- "gwas"
        for(d in diseaseofinterest){
            mg <- GWAS_ASSOCIATIONS[tolower(GWAS_ASSOCIATIONS$DISEASE.TRAIT) == d,'MAPPED_GENE']
            if(!identical(mg, character(0))){
                ifelse(is.null(mapped_genes), mapped_genes <- mg, mapped_genes <- c(mapped_genes, mg))
            }
        }
        returned_genes_list <- c()
        if(study == "study0"){
            returned_genes <- for(gene in c(rv$study0_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study1"){
            returned_genes <- for(gene in c(study1_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study2"){
            returned_genes <- for(gene in c(study2_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study3"){
            returned_genes <- for(gene in c(study3_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study4"){
            returned_genes <- for(gene in c(study4_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study5"){
            returned_genes <- for(gene in c(study5_genes)){
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study6"){
            returned_genes <- for(gene in c(study6_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study7"){
            returned_genes <- for(gene in c(study7_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study8"){
            returned_genes <- for(gene in c(study8_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        if(study == "study9"){
            returned_genes <- for(gene in c(study9_genes)){ 
                if(TRUE %in% grepl(gene, mapped_genes)){returned_genes_list <- c(returned_genes_list, gene)}
            }
        }
        # for event conditioning syntax, returned_genes would need to be "" if empty
        ifelse(returned_genes_list != c(), rv$returned_genes_list <- returned_genes_list, rv$returned_genes_list <- "")
        df <- data.frame()
        for(d in diseaseofinterest){
            for(gene in returned_genes_list){
                temp_df <- create_gwas_association_df(gene)
                df <- rbind(df, temp_df[temp_df$`Disease/Trait` == d,])
                # ^subsets the table only for the disease of interest
                #df <- df[df$`Mapped Gene` %in% returned_genes_list,]
                # ^subsets the table only for the genes of interest
            }
        }
        ### CLEAN UP
        ### only perform this section if the assocation_df isn't empty ###
        ### this cleans up selection to remove columns that are empty ####
        if(nrow(df) != 0){
            df$Link <- paste0('<a href="https://', df$Hyperlink,'" target="_blank">', df$Hyperlink, '</a>')
            df <- df[, -5] # remove Hyperlink column 
            to_remove <- "" # if all rows in a column are blank, then remove the column
            for(i in 1:ncol(df)){
                # First check if there's an NA in the row. If so, keep row.
                NAflag = is.na(sum((df[,i]) == "") == nrow(df))
                if(NAflag == TRUE){
                    to_remove = to_remove
                }
                # THEN, check if each entry of a row is blank
                else if (sum((df[,i]) == "") == nrow(df)){
                    to_remove <- c(to_remove,i)
                }
            }
            # make sure to_remove actually exists before removing it from df
            #if(to_remove != ""){df <- df[, -c(all_of(to_remove))]}
        } 
        #### done ########################################################
        df <- unique(df)  # remove duplicate rows
        df}, # display df
        options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width='20px',targets=2)),
            processing = FALSE
        ),
        escape = FALSE
    )
}