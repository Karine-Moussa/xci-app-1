create_escape_df <- function(gene){
    # Searches for escape state from various studies
    # And compiles them into a data frame
    # ####################################################
    # GENE    STUDY     ESCAPE STATE
    # <name>  <study>   [escape | inactive | variable]
    # ####################################################
    #
    status = list()
    study = list()
    # Study 1 (GEUVIDAS)
    study_name <- "GEUVIDAS"
    ref_table <- x_expr_mod
    query <- "status_adv"
    status_abr = unique(ref_table[ref_table$GENE==gene, query])
    if(status_abr == "E"){status <- list.append(status,"escape")}
    if(status_abr == "V"){status <- list.append(status,"variable")}
    if(status_abr == "S"){status <- list.append(status,"inactive")}
    if(length(status_abr) != 0){study <- list.append(study, study_name)}
    
    # Study 2 (COTTON AND CARREL)
    study_name <- "Cotton et al. + Carrel/Willard"
    ref_table <- cott_carr_will_df
    query <- "status"
    status_abr = unique(ref_table[ref_table$gene==gene, query])
    status <- list.append(status,status_abr) 
    # no need to convert the abbreviations since they are 
    # already correct.
    if(length(status_abr) != 0){study <- list.append(study, study_name)}
    
    status <- unlist(status)
    study <- unlist(study)
    
    df <- data.frame(gene = gene,
                     study = study,
                     state = status)
    
    return(df)
}
