asdf <- renderDataTable({
    validate(need(rv$geneofinterest1,""))
    geneofinterest <- rv$geneofinterest1
    df <- data.frame()
    for(gene in geneofinterest){
        df <- rbind(df, create_gwas_association_df(gene))
    }
    ### only perform this section if the assocation_df isn't empty ###
    ### this cleans up selection to remove columns that are empty ####
    if(nrow(df) != 0){
        df$Link <- paste0('<a href="https://', df$Link,'" target="_blank">', df$Link, '</a>')
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

fdsa <- renderDataTable({
    validate(need(rv$geneofinterest1,""))
    geneofinterest <- rv$geneofinterest1
    df <- data.frame()
    for(gene in geneofinterest){
        df <- rbind(df, create_nelson_association_df(gene))
    }
    ### only perform this section if the assocation_df isn't empty ###
    ### this cleans up selection to remove columns that are empty ####
    if(nrow(df) != 0){
        df$Link <- paste0('<a href="https://', df$Hyperlink,'" target="_blank">', df$Hyperlink, '</a>')
        df <- select(df, -"Hyperlink") # remove Hyperlink column
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