create_escape_df <- function(geneofinterest) {
    # Searches for gene in meta_dt (consolidates all studies)
    # And extracts its information.
    # ####################################################
    # GENE    STUDY     ESCAPE STATE
    # <name>  <study>   [escape | inactive | variable]
    # ####################################################
    # geneofinterest can be multiple genes
    #
    # Make sure empty spaces are "NA"
    for (i in 1:nrow(meta_dt)) {
        if (meta_dt$STATUS[i] == "") {
            meta_dt$STATUS[i] <- "NA"
        }
    }
    
    # set up dt
    dt <- meta_dt[GENE %in% c(geneofinterest),]

    # Add STUDY 0
    ref_table <- readRDS("rds/study0_df.rds")
    query <- "STATE"
    study0_entries <- data.table()
    for (gene in geneofinterest) {
        if (nrow(ref_table) != 0) {
            # first need to make sure it's not empty
            if (gene %in% ref_table$GENE) {
                status_abr <- unique(ref_table[ref_table$GENE == gene, query])
            } else {
                status_abr <- "NA"
            }
        } else {
            # if table was empty, status = NA
            status_abr <- "NA"
        }
        entry <- data.table("GENE" = gene, "STUDY" = "UPLOADED STUDY", 
                              "STATUS" = status_abr)
        study0_entries <- rbind(study0_entries, entry)
    }
    
    # Combine STUDY 0 with meta_dt
    dt <- rbind(dt, study0_entries)
    dt <- dt[order(dt$GENE)]
    
    return(dt)
}