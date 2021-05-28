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
    
    # Study 0 (MANUAL study)
    study_name <- "UPLOADED STUDY"
    ref_table <- readRDS("rds/study0_df.rds")
    query <- "state"
    if(nrow(ref_table) != 0) { # first need to make sure it's not empty
        if (gene %in% ref_table$gene){
            status_abr <- unique(ref_table[ref_table$gene==gene, query])
        } else {
            status_abr <- "NA"
        }
    } else { # if table was empty, status = NA
        status_abr <- "NA"
    }
    status <- list.append(status, status_abr)
    study <- list.append(study, study_name)

    # Study 1 (GEUVIDAS)
    study_name <- "GEUVADIS (lymphoblast)"
    ref_table <- x_expr_mod
    query <- "status_adv"
    # Check if gene is in the study
    if(gene %in% study1_genes){
        status_abr = unique(ref_table[ref_table$GENE==gene, query])
        if(status_abr == "E"){status <- list.append(status,"escape")}
        if(status_abr == "V"){status <- list.append(status,"variable")}
        if(status_abr == "S"){status <- list.append(status,"inactive")}
        if(length(status_abr) != 0){study <- list.append(study, study_name)}
    } else {
        status <- list.append(status, "NA")
        study <- list.append(study, study_name)
    }

        
    # Study 6 (GTEx) (very similar to Study1 format)
    study_name <- "GTEx (multi-tissue)"
    ref_table <- unique(TukGTExMod[,-4]) # don't use Tissue column, make unique
    query <- "status_adv"
    if (gene %in% study6_genes){
        status_abr = unique(ref_table[ref_table$`Gene name`==gene, query])
        if(status_abr == "E"){status <- list.append(status,"escape")}
        if(status_abr == "V"){status <- list.append(status,"variable")}
        if(status_abr == "S"){status <- list.append(status,"inactive")}
    } else {
        status <- list.append(status,"NA")
    }
    study <- list.append(study, study_name)
    
    # Study 2 (COTTON)
    study_name <- "Cotton et al. (multi-tissue)"
    ref_table <- cott_carr_will_df
    query <- "status_cott"
    if (gene %in% study2_genes){
    status_abr = unique(ref_table[ref_table$gene==gene, query])
    } else {
        status_abr = "NA"
    }
    status <- list.append(status, status_abr) 
    study <- list.append(study, study_name)
    
    # Study 3 (CARREL/WILLARD)
    study_name <- "Carrel/Willard (hybrid fibroblast)"
    ref_table <- cott_carr_will_df
    query <- "status_carrwill"
    if (gene %in% study3_genes){
        status_abr = unique(ref_table[ref_table$gene==gene, query])
    } else {
        status_abr = "NA"
    }
    status <- list.append(status, status_abr) 
    study <- list.append(study, study_name)

    # Study 4 (KATSIR LINIAL LYMPHOBLAST)
    study_name <- "Katsir + Linial (lymphoblast)"
    ref_table <- kat_lin_df
    query <- "status_lb"
    if (gene %in% study4_genes){
        status_abr = unique(ref_table[ref_table$gene==gene, query])
        if(length(status_abr) == 0 || status_abr == ""){
            status_abr = "NA"
        }
    } else {
        status_abr = "NA"
    }
    status <- list.append(status, status_abr) 
    study <- list.append(study, study_name)
    
    # Study 5 (KATSIR LINIAL FIBROBLAST)
    study_name <- "Katsir + Linial (fibroblast)"
    ref_table <- kat_lin_df
    query <- "status_fb"
    if (gene %in% study5_genes){
        status_abr = unique(ref_table[ref_table$gene==gene, query])
        if(length(status_abr) == 0 || status_abr == ""){
            status_abr = "NA"
        }
    } else {
        status_abr = "NA"
    }
    status <- list.append(status, status_abr) 
    study <- list.append(study, study_name)
    
    # clean up
    status <- unlist(status)
    study <- unlist(study)
    
    # final
    df <- data.frame(gene = gene,
                     study = study,
                     state = status)
    return(df)
}
