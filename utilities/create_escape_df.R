create_escape_df <- function(gene){
    # Searches for gene in meta_dt (consolidates all studies)
    # And extracts its information.
    # ####################################################
    # GENE    STUDY     ESCAPE STATE
    # <name>  <study>   [escape | inactive | variable]
    # ####################################################
    #
    return(meta_dt[GENE == gene,])
}